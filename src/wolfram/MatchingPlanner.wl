(* External channels and vertex combinations are derived from action metadata. *)
ClearAll[theoryVertices,vertexCombinations,planTheoryMatching];
fieldCompatibilityComponents[instances_List,allowed_List]:=Module[{remaining=Range[Length[instances]],reached,next,count=0},
 While[remaining=!={},count++;reached={First[remaining]};
  Do[next=Union[reached,Select[remaining,Function[i,AnyTrue[reached,Intersection[instances[[i]],instances[[#]],allowed]=!={}&]]]];
   If[next===reached,Break[]];reached=next,{Length[instances]}];remaining=Complement[remaining,reached]];count
];
connectedVertexContentQ[instances_List,external_List]:=Module[{types,internal,active},
 If[Length[instances]<=1,Return[True]];types=DeleteDuplicates[Flatten[instances]];
 internal=Association[Table[f->(Count[Flatten[instances],f]-Count[external,f]),{f,types}]];
 active=Select[types,internal[#]>=2&];
 (* After deleting any set of field species, its available propagators must
    be numerous enough to connect the remaining compatibility components.
    This necessary spanning-tree bound also catches disconnected allocations
    that pass pairwise compatibility but lack enough ports of one species. *)
 AllTrue[Subsets[active],Function[removed,
   Total[(internal[#]/2)&/@removed]>=fieldCompatibilityComponents[instances,Complement[active,removed]]-1]]
];
theoryVertices[theory_]:=Catch[Module[{interaction=interactionTheory[theory],channels,rows},
 channels=DeleteDuplicates[Sort/@Select[Lookup[interaction["Terms"],"Fields"],Length[#]>0&]];
 rows=Table[With[{momenta=independentExternalMomenta[Length[channel]]},
  <|"Fields"->channel,"Degree"->Length[channel],"Response"->canonicalMomentumPolynomial[ScalarLocalVertex[interaction,channel,momenta],momenta]|>],{channel,channels}];
 Select[rows,#["Response"]=!=0&]
],"FM2TheoryStage"];
vertexCombinations[vertices_List,allFields_List,external_List,loops_Integer]:=Catch[Module[{rhs=2loops+Length[external]-2,active,bounds,combinations,half},
 active=Select[vertices,#["Degree"]>2&];
 If[rhs<0||active==={},Throw[{},"FM2TheoryStage"]];
 bounds=Floor[rhs/(#["Degree"]-2)]&/@active;
 combinations=Select[Tuples[Range[0,#]&/@bounds],Function[counts,
  Total[counts*(Lookup[active,"Degree"]-2)]===rhs&&
   (half=Table[Sum[counts[[i]]Count[active[[i]]["Fields"],f],{i,Length[active]}]-Count[external,f],{f,allFields}];
    AllTrue[half,IntegerQ[#]&&#>=0&&EvenQ[#]&]&&connectedVertexContentQ[Flatten[MapThread[ConstantArray[#2["Fields"],#1]&,{counts,active}],1],external])]];
 Map[<|"Counts"->#,"Vertices"->Lookup[active,"Fields"],"VertexOrder"->Total[#]|>&,combinations]
],"FM2TheoryStage"];
planTheoryMatching[task_]:=Catch[Module[{uv=task["UV"],eft=task["EFT"],request=task["Request"],response,vertices,tasks={},loops,maxdim,external,uvExternal,combinations,orders,channelId,derivativeOrder,extra},
 response=BuildOperatorResponses[eft];If[FailureQ[response],Throw[response,"FM2TheoryStage"]];
 If[response["Rank"]<Length[response["Coefficients"]],Throw[Failure["EFTDependentBasis",<|"MessageTemplate"->"The supplied Green basis has undetermined coefficient combinations.","NullSpace"->response["NullSpace"],"Coefficients"->response["Coefficients"]|>],"FM2TheoryStage"]];
 vertices=theoryVertices[uv];loops=request["Loops"];maxdim=request["MaxDimension"];
 Do[
  external=channel;uvExternal=external/.task["LightFieldMap"];
  channelId=StringTake[theoryIdentity[fieldName/@external],16];
  derivativeOrder=Max[0,Sequence@@Lookup[Select[response["Rows"],#["ExternalFields"]===external&],"MomentumDegree"]];
  Do[
   combinations=vertexCombinations[vertices,uv["FieldSymbols"],uvExternal,loop];orders=Sort[DeleteDuplicates[Lookup[combinations,"VertexOrder"]]];
   extra=2Max[0,loops-loop];
   AppendTo[tasks,<|"id"->("channel-"<>channelId<>"-L"<>ToString[loop]),"ChannelIdentity"->channelId,"EFTFields"->external,"UVFields"->uvExternal,
    "external_fields"->(fieldName/@uvExternal),"loops"->loop,"legs"->Length[external],"momentum_order"->derivativeOrder,
    "taylor_order"->Max[derivativeOrder,maxdim-Length[external]]+extra,"coefficient_weight_limit"->maxdim-Length[external],
    "generator_vertex_orders"->orders,"vertex_combinations"->combinations,"matching_object"->"1LPI","off_shell"->True,
    "precision_reason"->"Operator response plus conservative two units of auxiliary light weight per remaining loop; downstream Laurent budget is refined after reduction."|>],{loop,0,loops}],{channel,response["Channels"]}];
 <|"Schema"->"feammatch2.matching-plan.v2","TaskIdentity"->task["Identity"],"Response"->response,"tasks"->tasks,
  "UVVertices"->vertices,"Scope"->"Real scalar component actions; response-driven channels and nonquadratic UV vertex combinations. Quadratic counterterm vertices are separately budgeted insertions.",
  "Identity"->theoryIdentity[{task["Identity"],tasks,response["Identity"]}]|>
],"FM2TheoryStage"];
PlanMatching[task_Association/;Lookup[task,"Schema",""]==="feammatch2.matching-task.v2"]:=planTheoryMatching[task];

GenerateTheoryTask[theory_Association,export_Association,task_Association,vertexOrder_Integer,output_String,configuration_Association]:=Catch[Module[
 {config=configuration,external,light,card,modelRoot,interaction,zeroEligible,zeroExternal,compress},
 external=task["TheoryFields"]/.export["Aliases"]["Fields"];
 light=Lookup[Select[theory["Fields"],#["Role"]==="light"&],"Symbol"]/.export["Aliases"]["Fields"];
 If[external==={},Throw[fail["ProcessFields","Declare external fields for the generated theory task."],"FM2TheoryStage"]];
 interaction=scalarActionTheory[theory,theory["Lagrangian"]-freeScalarAction[theory]];If[FailureQ[interaction],Throw[interaction,"FM2TheoryStage"]];
 zeroExternal=task["momentum_order"]===0&&
  (Lookup[task,"taylor_order",task["momentum_order"]]===0||TrueQ[Lookup[task,"zero_external_for_mass_expansion",False]]);
 zeroEligible=zeroExternal&&AllTrue[interaction["Terms"],#["Coefficient"]===0||#["DerivativeOrder"]===0&];
 compress=zeroEligible&&TrueQ[Lookup[config,"ZeroMomentumCompression",True]];
 card=<|"model_name"->"TheoryUFO","unitary_gauge"->False,"DropTadpole"->False,"DropWFcorrection"->False,"fm2_include_local_contacts"->True,
  "n_loop"->task["loops"],"QCDCT_order"->0,"Amp_QCD_order"->0,"Amp_QED_order"->vertexOrder,"Amp_SPC_order"->0,
  "Amp_Min_Ep_Xpt"->-4,"Amp_Max_Ep_Xpt"->2,"incoming"->{SymbolName[First[external]]},"outgoing"->(SymbolName/@Rest[external]),
  "couplingfactor"->"1","momentum_symmetry"->{},"color_symmetry"->{},"fm2_light_fields"->(SymbolName/@light),
  "fm2_auxiliary_external_fields"->(SymbolName/@((#["Symbol"]&/@Select[theory["Fields"],TrueQ[Lookup[#,"Auxiliary",False]]&])/.export["Aliases"]["Fields"])),
  "fm2_external_only_fields"->(SymbolName/@((#["Symbol"]&/@Select[theory["Fields"],TrueQ[Lookup[#,"ExternalOnly",False]]&])/.export["Aliases"]["Fields"])),
  "fm2_tadpole_free"->TrueQ[Lookup[config,"TadpoleFree",False]],"fm2_zero_momentum_compression"->compress,
  "fm2_zero_boundary_vertices"->{},"fm2_max_zero_vertices"->Lookup[config,"MaxZeroVertices",0]|>;
 (* Setting external momenta to zero is exact for Taylor order zero even
    with derivative vertices. Graph compression has the stricter separate
    condition because its isomorphism labels do not encode derivative rays. *)
 config=Join[config,<|"ZeroMomentumCompression"->compress,"ZeroExternalForPotentialTasks"->zeroExternal|>];
 (* The common generator searches output/models; export is required there. *)
 modelRoot=ExpandFileName[FileNameJoin[{output,"models"}]];
 If[ExpandFileName[export["Directory"]]=!=modelRoot,Throw[fail["TheoryModelDirectory","Export the UFO into this theory output's models directory before generation."],"FM2TheoryStage"]];
 generateOffshellTask[theory,card,output,task,vertexOrder,config]
],"FM2TheoryStage"];

PlanMatching[___]:=fail["MatchingTask","Create a matching task with CreateMatchingTask before planning."];
