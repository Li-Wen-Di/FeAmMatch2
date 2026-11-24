(* Lossless off-shell amplitudes. No paired/four-point-specific kinematics. *)
matchingWeightTruncate[expr_,task_,momenta_List,externalCount_Integer,extra_Integer:0,momentumCap_:Infinity]:=Module[
 {rho=Unique["matchingWeight"],s=Unique["momentumWeight"],weights,result=expr,uv=task["UV"]},
 If[momentumCap=!=Infinity,result=scaledPolynomialCut[result/.sp_SP:>s^Count[List@@sp,Alternatives@@momenta]sp,s,momentumCap]];
 weights=Join[Association[Table[f["Mass"]->1,{f,Select[uv["Fields"],#["Role"]==="light"&]}]],Lookup[task["Request"],"ParameterWeights",<||>]];
 scaledPolynomialCut[(result/.sp_SP:>rho^Count[List@@sp,Alternatives@@momenta]sp)/.KeyValueMap[#1->rho^#2 #1&,weights],rho,task["Request"]["MaxDimension"]-externalCount+extra]
];
IntegrateTheoryTask[theory_Association,export_Association,generation_Association,task_Association,config_Association]:=Catch[Module[
 {path,document,records,loops=task["loops"],n=task["legs"],momenta,independent,aliasBack,external,rules,integrand=0,expr,result,laurent,heavy,light,masses,phase,order,eps,scale,sources={},weight,weights,
  poleMode=TrueQ[Lookup[config,"ExtractUVPoles",False]],region},
 path=FileNameJoin[{generation["Directory"],"amplitudes.json"}];If[!FileExistsQ[path],Throw[fail["TheoryAmplitudeFile","Generated amplitude file is absent."],"FM2TheoryStage"]];
 document=Import[path,"RawJSON"];If[Lookup[document,"schema",""]=!="feammatch2.feamgen.v1",Throw[fail["TheoryAmplitudeSchema","Unexpected amplitude interchange schema."],"FM2TheoryStage"]];
 records=Lookup[document,"amplitudes",{}];momenta=independentExternalMomenta[Lookup[task,"PhysicalExternalCount",n]];independent=Most[momenta];
 aliasBack=Reverse/@export["Aliases"]["Parameters"];
 integrand=Total[Table[
  If[record["n_loop"]=!=loops||Length[record["ext_mom_list"]]=!=n,Throw[fail["TheoryAmplitudeProcess","Amplitude process metadata disagree with the frozen task."],"FM2TheoryStage"]];
  external=ParseScalarExpression/@record["ext_mom_list"];
  rules=Thread[external->If[Lookup[task,"taylor_order",task["momentum_order"]]===0,ConstantArray[0,n],Join[Take[momenta,1],-Rest[momenta],ConstantArray[0,n-Length[momenta]]]]];
  expr=ImportScalarAmplitude[record,rules];If[FailureQ[expr],Throw[expr,"FM2TheoryStage"]];
  expr=expr/.aliasBack;
  If[Head[Lookup[config,"InsertionSymbol",None]]===Symbol&&Lookup[config,"InsertionSymbol",None]=!=None,
   expr=Coefficient[Expand[expr],config["InsertionSymbol"],Lookup[config,"InsertionOrder",1]]];
  weights=Lookup[config,"CouplingWeights",<||>];
  If[Length[weights]>0,weight=Unique["coefficientWeight"];expr=Normal[Series[expr/.KeyValueMap[#1->weight^#2 #1&,weights],{weight,0,Lookup[config,"MaxCouplingWeight",2]}]]/.weight->1];
  expr=(expr Lookup[config,"IntegrandPrefactor",1])/.Lookup[config,"CoefficientRules",{}];
  expr=expandScalarProducts[expr/.rules/.Last[momenta]->-Total[independent],Join[Take[modelSymbol/@{"q1","q2"},loops],independent]];
  expr,{record,records}]];
 sources=(<|"File"->#["source_file"],"SHA256"->#["source_sha256"]|>&/@records);
 integrand=Expand[integrand];
 heavy=#["Mass"]&/@Select[theory["Fields"],#["Role"]==="heavy"&];
 light=#["Mass"]&/@Select[theory["Fields"],#["Role"]==="light"&];
 heavy=DeleteDuplicates[heavy/.Lookup[config,"CoefficientRules",{}]];light=DeleteDuplicates[light/.Lookup[config,"CoefficientRules",{}]];
 region=If[poleMode,"MassiveTaylorUVPole","AllHard"];
 If[poleMode,
  If[loops=!=1||MemberQ[Join[heavy,light],0],
   Throw[fail["UVPoleReferenceMasses","One-loop UV pole extraction requires nonzero reference masses for physical fields."],"FM2TheoryStage"]];
  heavy=DeleteDuplicates[Join[heavy,light]];light={}];
 order=Lookup[task,"taylor_order",task["momentum_order"]];
 If[integrand===0,Throw[<|"Stage"->"bare_hard_integrated","LaurentExpression"->0,"Expression"->0,"Integrand"->0,"Sources"->sources,"ExternalMomenta"->momenta,"Region"->region|>,"FM2TheoryStage"]];
 result=EvaluateHardAmplitude[integrand,Take[modelSymbol/@{"q1","q2"},loops],independent,heavy,DeleteCases[light,0],order,config];If[FailureQ[result],Throw[result,"FM2TheoryStage"]];
 phase=I^(loops-1);eps=Lookup[config,"EpsilonSymbol",modelSymbol["eps"]];scale=Lookup[config,"Scale",modelSymbol["mu"]];
 laurent=timedStage["LaurentExpansion",If[loops===0,<|"Expression"->phase result["Expression"]|>,LaurentVacuum[phase result["Expression"],eps,scale,Lookup[config,"EpsilonOrder",If[loops===1,1,0]],config]]];
 If[FailureQ[laurent],Throw[laurent,"FM2TheoryStage"]];
 <|"Stage"->"bare_hard_integrated","Expression"->phase result["Expression"],"LaurentExpression"->laurent["Expression"],"Integrand"->integrand,
  "Sources"->sources,"ExternalMomenta"->momenta,"Reduction"->result,"Laurent"->laurent,"Region"->region|>
],"FM2TheoryStage"];

ClearAll[theoryChannelKey];
theoryChannelKey[channel_List]:=theoryIdentity[Sort[channel]];

ComputeTheoryAmplitudes[task_Association,loop_Integer,directory_String,configuration_Association]:=Catch[Module[
 {theory,plan,processes,out=ExpandFileName[directory],config=configuration,export,actual,orders,results=<||>,records={},p,generated,r,total,key,sourceConfig,
  poleMode=TrueQ[Lookup[configuration,"ExtractUVPoles",False]],record},
 If[!MemberQ[{0,1,2},loop],Throw[fail["AmplitudeLoopOrder","Diagram amplitudes support zero to two loops."],"FM2TheoryStage"]];
 theory=Lookup[config,"GenerationTheory",task["UV"]];
 processes=Lookup[config,"Processes",Automatic];
 If[processes===Automatic,plan=PlanMatching[task];If[FailureQ[plan],Throw[plan,"FM2TheoryStage"]];
  processes=Select[plan["tasks"],#["loops"]===loop&]];
 If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
 export=Lookup[config,"TheoryExport",Automatic];If[export===Automatic,export=ExportTheoryUFO[theory,FileNameJoin[{out,"models"}],config]];If[FailureQ[export],Throw[export,"FM2TheoryStage"]];
 config=Join[config,<|"WorkingDirectory"->FileNameJoin[{out,"form-jobs"}],"RetainAssetCopies"->False|>];
 actual=scalarActionTheory[theory,Expand[(theory["Lagrangian"]-freeScalarAction[theory])/.Lookup[config,"CoefficientRules",{}]]];If[FailureQ[actual],Throw[actual,"FM2TheoryStage"]];
 Do[
  p=Join[process,<|"TheoryFields"->Lookup[process,"TheoryFields",process["UVFields"]]|>];
  If[Lookup[config,"InsertionSymbol",None]=!=None,
   orders=insertionVertexOrders[actual,theory["FieldSymbols"],p["TheoryFields"],loop,config["InsertionSymbol"]],
   orders=p["generator_vertex_orders"]];
  If[loop===0&&p["legs"]<3,orders={}];
  total=0;
  Do[generated=timedStage["GenerateUVTask",GenerateTheoryTask[theory,export,p,order,out,config]];If[FailureQ[generated],Throw[generated,"FM2TheoryStage"]];
   Print["Off-shell FORM integration ",p["id"]," V",order," (",generated["Graphs"]," graphs)"];
   r=timedStage["IntegrateUVTask",IntegrateTheoryTask[theory,export,generated,p,config]];If[FailureQ[r],Throw[r,"FM2TheoryStage"]];
   writeResultArtifact[generated["Directory"],"independent-hard",r];total+=r["LaurentExpression"];
   record=<|"Channel"->p["EFTFields"],"Order"->order,"Directory"->generated["Directory"],"Graphs"->generated["Graphs"]|>;
   If[poleMode,AssociateTo[record,"LaurentExpression"->r["LaurentExpression"]]];
   AppendTo[records,record],{order,orders}];
  If[Lookup[config,"InsertionSymbol",None]=!=None,
   sourceConfig=Join[config,<|"HardLightWeights"->Join[Association[Table[f["Mass"]->1,{f,Select[task["UV"]["Fields"],#["Role"]==="light"&]}]],Lookup[task["Request"],"ParameterWeights",<||>]],
     "HardWeightBudget"->task["Request"]["MaxDimension"]-p["legs"]+Lookup[config,"AuxiliaryLightWeight",0]|>];
   r=integrateHeavySourceInsertions[theory,export,actual,p,out,sourceConfig];If[FailureQ[r],Throw[r,"FM2TheoryStage"]];
   total+=r["Expression"];records=Join[records,r["Records"]]];
  If[Lookup[config,"InsertionSymbol",None]=!=None,
   r=integrateQuadraticInsertions[theory,export,actual,p,out,config];If[FailureQ[r],Throw[r,"FM2TheoryStage"]];
   total+=r["Expression"];records=Join[records,r["Records"]]];
  key=theoryChannelKey[p["EFTFields"]];AssociateTo[results,key-><|"ExternalFields"->p["EFTFields"],"Expression"->Expand[total],"Momenta"->independentExternalMomenta[p["legs"]],"Process"->p|>],{process,processes}];
 <|"Stage"->"bare_amplitudes_complete","Channels"->results,"Records"->records,"Export"->export,
  "Region"->If[poleMode,"MassiveTaylorUVPole","AllHard"]|>
],"FM2TheoryStage"];

diagramTreeMatching[task_,out_,cfg_]:=Catch[Module[
 {eft=task["EFT"],uv=task["UV"],interaction,plan,processes,raw,free,channels,project},
 interaction=scalarActionTheory[uv,uv["Lagrangian"]-freeScalarAction[uv]];
 If[FailureQ[interaction],Throw[interaction,"FM2TheoryStage"]];
 If[AnyTrue[Select[interaction["Terms"],#["Coefficient"]=!=0&&#["FieldDegree"]>0&],
    #["DerivativeOrder"]>0||!MemberQ[{3,4},#["FieldDegree"]]&],
  Throw[fail["TreeUVCapability","UV diagram matching requires a canonical diagonal free kernel, a stationary tree vacuum and a renormalizable scalar potential."],"FM2TheoryStage"]];
 plan=PlanMatching[task];If[FailureQ[plan],Throw[plan,"FM2TheoryStage"]];
 processes=Join[#,<|"taylor_order"->Max[#["momentum_order"],task["Request"]["MaxDimension"]-#["legs"]]|>]&/@
  Select[plan["tasks"],#["loops"]===0&];
 raw=ComputeTheoryAmplitudes[task,0,out,Join[cfg,<|"Processes"->processes|>]];
 If[FailureQ[raw],Throw[raw,"FM2TheoryStage"]];
 free=actionTheoryChannels[eft,
   freeScalarAction[Join[uv,<|"Fields"->Select[uv["Fields"],#["Role"]==="light"&]|>]]/.(Reverse/@task["LightFieldMap"]),
   raw["Channels"]];
 If[FailureQ[free],Throw[free,"FM2TheoryStage"]];
 channels=Association[KeyValueMap[#1->Join[#2,<|"Expression"->Expand[#2["Expression"]+free[#1]["Expression"]]|>]&,raw["Channels"]]];
 project=ProjectTheoryAmplitudes[eft,channels];If[FailureQ[project],Throw[project,"FM2TheoryStage"]];
 writeResultArtifact[out,"uv-1lpi-tree-amplitudes",raw];
 Join[project,<|"Stage"->"tree_green_complete","GraphRecords"->raw["Records"]|>]
],"FM2TheoryStage"];

diagramUVTreeCounterterm[task_,action_,processes_,out_,cfg_]:=Catch[Module[
 {uv=task["UV"],prepared,treeProcesses,raw,extra=Lookup[cfg,"AuxiliaryLightWeight",0]},
 prepared=PrepareInsertionTheory[uv,action];If[FailureQ[prepared],Throw[prepared,"FM2TheoryStage"]];
 (* Eq. (53) is a one-loop matching input. The tree source or mass
    counterterm may itself carry soft powers in addition to external momenta. *)
 treeProcesses=Join[#,<|"loops"->0,"id"->(#["id"]<>"-ct-tree"),
  "zero_external_for_mass_expansion"->(#["taylor_order"]===0),"taylor_order"->(#["taylor_order"]+extra)|>]&/@processes;
 raw=ComputeTheoryAmplitudes[task,0,out,Join[cfg,<|"Processes"->treeProcesses,
  "GenerationTheory"->prepared["Theory"],"InsertionSymbol"->prepared["Marker"],
  "CoefficientRules"->prepared["CoefficientRules"]|>]];
 If[FailureQ[raw],Throw[raw,"FM2TheoryStage"]];
 writeResultArtifact[out,"uv-1lpi-tree-counterterm",raw];raw
],"FM2TheoryStage"];

ProjectTheoryAmplitudes[eft_Association,channels_Association,knownAction_:0]:=Catch[Module[{response,rhs,rows,polynomial,key,variables,solution,local,residuals=<||>,vertex,momenta},
 response=BuildOperatorResponses[eft];If[FailureQ[response],Throw[response,"FM2TheoryStage"]];
 rows=response["Rows"];
 If[AnyTrue[response["Channels"],!KeyExistsQ[channels,theoryChannelKey[#]]&],Throw[fail["AmplitudeCoverage","A requested external channel has no explicit amplitude result."],"FM2TheoryStage"]];
 rhs=Table[key=theoryChannelKey[row["ExternalFields"]];polynomial=Expand[channels[key]["Expression"]];variables=row["Variables"];
  If[variables==={},polynomial,Fold[Coefficient[#1,First[#2],Last[#2]]&,polynomial,Transpose[{variables,row["Powers"]}]]],{row,rows}];
 If[knownAction=!=0,rhs-=Table[vertex=canonicalMomentumPolynomial[ScalarLocalVertex[scalarActionTheory[eft,knownAction],row["ExternalFields"],row["Momenta"]],row["Momenta"]];
  If[row["Variables"]==={},vertex,Fold[Coefficient[#1,First[#2],Last[#2]]&,vertex,Transpose[{row["Variables"],row["Powers"]}]]],{row,rows}]];
 solution=SolveExactSystem[response["Matrix"],rhs,response["Coefficients"]];If[FailureQ[solution],Throw[solution,"FM2TheoryStage"]];
 local=knownAction+Total[(# D[eft["Lagrangian"],#])&/@eft["WilsonCoefficients"]]/.solution["Rules"];
 local=scalarActionTheory[eft,local];If[FailureQ[local],Throw[local,"FM2TheoryStage"]];
 Do[momenta=channels[key]["Momenta"];vertex=canonicalMomentumPolynomial[ScalarLocalVertex[local,channels[key]["ExternalFields"],momenta],momenta];
  polynomial=Factor[Expand[channels[key]["Expression"]-vertex]];AssociateTo[residuals,key->polynomial],{key,Keys[channels]}];
 If[AnyTrue[Values[residuals],#=!=0&],Throw[Failure["LocalProjectionResidual",<|"MessageTemplate"->"The supplied Green action does not cover all calculated local momentum structures.","Residuals"->residuals|>],"FM2TheoryStage"]];
 Join[solution,<|"Lagrangian"->local["Lagrangian"],"ChannelResiduals"->residuals|>]
],"FM2TheoryStage"];
