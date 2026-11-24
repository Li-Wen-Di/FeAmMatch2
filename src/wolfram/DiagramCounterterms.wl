(* UV and EFT use the same one-loop generator and massive Taylor evaluator.
   The soft masses are kept in denominators until the UV residue is extracted. *)
diagramProcess[theory_,fields_,process_,object_,loop_]:=Module[{combinations},
 combinations=vertexCombinations[theoryVertices[theory],theory["FieldSymbols"],fields,loop];
 Join[process,<|"TheoryFields"->fields,"UVFields"->fields,"loops"->loop,
  "matching_object"->object,"generator_vertex_orders"->Sort[DeleteDuplicates[(#["VertexOrder"]&)/@combinations]]|>]
];

diagramUVActionCounterterms[task_,out_,cfg_]:=Catch[Module[
 {uv=task["UV"],fields,channels,processes,raw,eps=modelSymbol["eps"],tadpole,bubble,vacuum,
  local,order,monomial,finiteHeavyTadpole,convention,overall},
 fields=uv["FieldSymbols"];convention=Lookup[task["Request"],"Tadpole","MSbar"];
 channels=Flatten[Table[DeleteDuplicates[Sort/@Tuples[fields,n]],{n,1,4}],1];
 processes=Table[
   order=If[Length[ch]===2,2,0];
   diagramProcess[uv,ch,<|"id"->("uv-pole-"<>StringTake[theoryChannelKey[ch],16]),
    "EFTFields"->ch,"legs"->Length[ch],"momentum_order"->order,"taylor_order"->order|>,"1PI",1],
   {ch,channels}];
 (* A nondifferentiated one-loop graph with V vertices has UV degree 4-2V. *)
 processes=Map[Join[#,<|"generator_vertex_orders"->Select[#["generator_vertex_orders"],#<=2&]|>]&,processes];
 raw=ComputeTheoryAmplitudes[task,1,out,Join[cfg,<|"Processes"->processes,
  "ExtractUVPoles"->True,"TadpoleFree"->False,"EpsilonOrder"->If[convention==="heavy_tadpole_free",0,-1]|>]];
 If[FailureQ[raw],Throw[raw,"FM2TheoryStage"]];
 monomial[ch_]:=(Times@@ch)/(Times@@(Factorial/@Values[Counts[ch]]));
 local[records_]:=-Total[(Factor[theoryPolePart[#["LaurentExpression"],eps,1]]monomial[#["Channel"]])&/@records];
 tadpole=Expand[local[Select[raw["Records"],#["Order"]===1&]]];
 bubble=Expand[local[Select[raw["Records"],#["Order"]===2&]]];
 If[!FreeQ[tadpole+bubble,_SP]||
    !AllTrue[Select[raw["Records"],#["Order"]>2&],Expand[theoryPolePart[#["LaurentExpression"],eps,1]]===0&],
  Throw[fail["UVPoleProjection","The one-loop renormalizable scalar poles must be local potential terms."],"FM2TheoryStage"]];
 finiteHeavyTadpole=If[convention==="heavy_tadpole_free",
   -Total[Table[f raw["Channels"][theoryChannelKey[{f}]]["Expression"],
     {f,Lookup[Select[uv["Fields"],#["Role"]==="heavy"&],"Symbol"]}]],0];
 (* In the tadpole-free scheme the entire heavy one-point vertex is
    cancelled before the 1LPI bridge sum; the graph policy removes that pair. *)
 local=Expand[tadpole+bubble-If[convention==="heavy_tadpole_free",
     theoryPolePart[finiteHeavyTadpole,eps,1],0]];
 (* The optional overall-pole audit uses the general two-loop UV action. *)
 overall=theoryUVActionCounterterms[task];
 If[FailureQ[overall],Throw[overall,"FM2TheoryStage"]];
 vacuum=overall["UniversalUVPoles"]["PotentialOneLoopByTopology"]["Vacuum"];
 writeResultArtifact[out,"uv-1pi-amplitudes",raw];
 <|"OneLoop"->Expand[local-vacuum],"TwoLoop"->overall["TwoLoop"],"TadpoleConvention"->convention,
   "PotentialPolesByTopology"-><|"Vacuum"->vacuum,"Tadpole"->-tadpole,"Bubble"->-bubble|>,
   "HeavyTadpoleCancellation"->Expand[finiteHeavyTadpole],"DiagramAmplitudes"->raw|>
],"FM2TheoryStage"];

diagramEFTCounterterms[task_,eft_,baseRules_,processes_,out_,cfg_]:=Catch[Module[
 {uv=task["UV"],target=task["EFT"],massRules,weights,reference,correction,prepared,
  base,direction,directions,graphProcesses,settings,eps=modelSymbol["eps"],basePoles,derivativePoles,
  project,dproject,dirWeights,derivativeAction=0,baseAction,records,projectBack},
 massRules=Table[f["Mass"]->SelectFirst[uv["Fields"],#["Symbol"]===(f["Symbol"]/.task["LightFieldMap"])&]["Mass"],
   {f,eft["Fields"]}];
 reference=scalarActionTheory[eft,(eft["Lagrangian"]-freeScalarAction[eft])/.baseRules/.massRules];
 If[FailureQ[reference],Throw[reference,"FM2TheoryStage"]];
 If[AnyTrue[reference["Terms"],#["FieldDegree"]<=2&&#["Coefficient"]=!=0&],
   Throw[fail["LocalPoleFreeKernel","Tree quadratic action must equal the declared EFT reference kernel."],"FM2TheoryStage"]];
 weights=Association[Table[c->Max[0,-eft["Parameters"][c]["Dimension"]],{c,eft["WilsonCoefficients"]}]];
 graphProcesses=diagramProcess[eft,#["EFTFields"],#,"1PI",1]&/@processes;
 settings=Join[cfg,<|"ExtractUVPoles"->True,"EpsilonOrder"->-1,"TadpoleFree"->False,
  "Processes"->graphProcesses,"CoefficientRules"->Join[baseRules,massRules],"CouplingWeights"->weights|>];
 base=ComputeTheoryAmplitudes[task,1,out<>"/base",Join[settings,<|"GenerationTheory"->eft|>]];
 If[FailureQ[base],Throw[base,"FM2TheoryStage"]];
 basePoles=transformTheoryChannels[base["Channels"],Function[ch,Factor[theoryPolePart[ch["Expression"],eps,1]]]];
 projectBack[polynomials_]:=ProjectTheoryAmplitudes[target,polynomials];
 project=projectBack[basePoles];If[FailureQ[project],Throw[project,"FM2TheoryStage"]];
 baseAction=-project["Lagrangian"];
 directions=Lookup[cfg,"PoleDirections",{}];records=base["Records"];
 derivativePoles=transformTheoryChannels[basePoles,Function[ch,0]];
 If[directions=!={},
  correction=Total[(Last[#] D[target["Lagrangian"],First[#]])&/@directions];
  prepared=PrepareInsertionTheory[eft,correction];If[FailureQ[prepared],Throw[prepared,"FM2TheoryStage"]];
  dirWeights=Join[weights,Association[(#["Coefficient"]->Max[0,#["Dimension"]-4])&/@prepared["Vertices"]]];
  direction=ComputeTheoryAmplitudes[task,1,out<>"/direction",Join[settings,<|
   "GenerationTheory"->prepared["Theory"],"InsertionSymbol"->prepared["Marker"],
   "CoefficientRules"->Join[baseRules,massRules,prepared["CoefficientRules"]],"CouplingWeights"->dirWeights|>]];
  If[FailureQ[direction],Throw[direction,"FM2TheoryStage"]];
  derivativePoles=transformTheoryChannels[direction["Channels"],Function[ch,Factor[theoryPolePart[ch["Expression"],eps,1]]]];
  dproject=projectBack[derivativePoles];If[FailureQ[dproject],Throw[dproject,"FM2TheoryStage"]];
  derivativeAction=-dproject["Lagrangian"];records=Join[records,direction["Records"]]];
 writeResultArtifact[out,"eft-one-loop-counterterms",<|"Action"->baseAction,"DirectionAction"->derivativeAction,
   "GraphRecords"->records,"Channels"->basePoles,"DirectionChannels"->derivativePoles|>];
 <|"Stage"->"eft_1pi_poles_complete","Channels"->basePoles,"DirectionChannels"->derivativePoles,
   "Action"->baseAction,"DirectionAction"->derivativeAction,"GraphRecords"->records,
   "PoleDirections"->directions,"ExternalEFTGraphGeneration"->True|>
],"FM2TheoryStage"];
