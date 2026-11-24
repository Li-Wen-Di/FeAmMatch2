(* UV hard matching with explicit one-loop UV and EFT pole graphs. Overall
   two-loop EFT poles are inferred along this matching trajectory, not
   advertised as a universal two-loop EFT renormalization function. *)
MatchTheoryOneLoop[task_Association,directory_String,configuration_Association]:=Catch[Module[
 {eft=task["EFT"],uv=task["UV"],out=ExpandFileName[directory],cfg=configuration,extra,tree,aux,ctask,ceft,ctree,
  processes,uvRaw,poleData,uvRenormalization,uvCounter,treeCounter,total=<||>,hard=<||>,cap,momenta,channel,key,raw,ct,lp,project,poles,eps=modelSymbol["eps"]},
 cfg=Join[cfg,<|"TadpoleFree"->(Lookup[task["Request"],"Tadpole","MSbar"]==="heavy_tadpole_free")|>];
 If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
 tree=Lookup[cfg,"DiagramTree",Automatic];
 If[tree===Automatic,tree=diagramTreeMatching[task,out<>"/tree",cfg]];
 If[FailureQ[tree],Throw[tree,"FM2TheoryStage"]];extra=Lookup[cfg,"AuxiliaryLightWeight",0];
 aux=If[extra>0,BuildAuxiliaryGreenBasis[task,task["Request"]["MaxDimension"]+extra],None];If[FailureQ[aux],Throw[aux,"FM2TheoryStage"]];
 ctask=If[extra>0,aux["Task"],task];ceft=ctask["EFT"];ctree=If[extra>0,diagramTreeMatching[ctask,out<>"/auxiliary-tree",cfg],tree];
 If[FailureQ[ctree],Throw[ctree,"FM2TheoryStage"]];
 If[extra>0,aux=Join[aux,<|"Tree"->ctree|>]];
 processes=Join[#,<|"taylor_order"->Max[#["momentum_order"],task["Request"]["MaxDimension"]-#["legs"]]|>]&/@Select[PlanMatching[task]["tasks"],#["loops"]===1&];
 uvRenormalization=timedStage["UVActionCounterterms",diagramUVActionCounterterms[task,out<>"/uv-poles",cfg]];
 If[FailureQ[uvRenormalization],Throw[uvRenormalization,"FM2TheoryStage"]];uvCounter=uvRenormalization["OneLoop"];
 uvRaw=timedStage["UVOneLoopAmplitudes",ComputeTheoryAmplitudes[task,1,out<>"/UV",cfg]];If[FailureQ[uvRaw],Throw[uvRaw,"FM2TheoryStage"]];
 poleData=timedStage["EFTOneLoopPoleDiagrams",diagramEFTCounterterms[task,ceft,ctree["Rules"],processes,
  out<>"/eft-poles",Join[cfg,<|"MaxCouplingWeight"->task["Request"]["MaxDimension"]+extra-4|>]]];
 If[FailureQ[poleData],Throw[poleData,"FM2TheoryStage"]];
 treeCounter=timedStage["OneLoopTreeCounterterm",diagramUVTreeCounterterm[task,uvCounter,processes,out<>"/uv-ct-tree",cfg]];
 If[FailureQ[treeCounter],Throw[treeCounter,"FM2TheoryStage"]];
 Do[channel=uvRaw["Channels"][key];momenta=channel["Momenta"];cap=Max[channel["Process"]["momentum_order"],task["Request"]["MaxDimension"]-Length[channel["ExternalFields"]]];
  raw=matchingWeightTruncate[channel["Expression"],task,Most[momenta],Length[channel["ExternalFields"]],extra,cap];
  ct=matchingWeightTruncate[treeCounter["Channels"][key]["Expression"],task,Most[momenta],Length[channel["ExternalFields"]],extra,cap];
  lp=matchingWeightTruncate[poleData["Channels"][key]["Expression"],task,Most[momenta],Length[channel["ExternalFields"]],extra,cap];
  AssociateTo[hard,key->Join[channel,<|"Expression"->Expand[raw+ct]|>]];
  AssociateTo[total,key->Join[channel,<|"Expression"->Expand[raw+ct+lp]|>]],{key,Keys[uvRaw["Channels"]]}];
 project=timedStage["OneLoopProjection",ProjectTheoryAmplitudes[eft,total]];If[FailureQ[project],Throw[project,"FM2TheoryStage"]];poles=Factor[theoryPolePart[#,eps]]&/@project["Values"];
 If[AnyTrue[poles,#=!=0&],Throw[Failure["OneLoopHardPoleResidual",<|"MessageTemplate"->"UV hard poles do not agree with the explicit UV and EFT one-loop counterterms.","Residuals"->poles|>],"FM2TheoryStage"]];
 writeResultArtifact[out,"uv-bare",uvRaw];writeResultArtifact[out,"uv-hard-action",hard];writeResultArtifact[out,"assembled-channels",total];
 <|"Stage"->"one_loop_green_complete","Method"->"UVHardMSbar","Tree"->tree,"OneLoopLaurentRules"->project["Rules"],"OneLoopLagrangian"->project["Lagrangian"],"Projection"->project,
  "UVAmplitudes"->uvRaw,"UVHardChannels"->hard,"LocalPoleData"->poleData,"UVActionCounterterm"->uvCounter,
  "UVRenormalization"->uvRenormalization,"PoleResiduals"->poles,
  "UVTreeOneLoopCounterterm"->treeCounter,"AuxiliaryLightWeight"->extra,"AuxiliaryBasis"->aux,"ExternalEFTGraphGeneration"->True|>
],"FM2TheoryStage"];

MatchTheoryTwoLoop[task_Association,directory_String,configuration_Association]:=Catch[Module[
 {out=ExpandFileName[directory],cfg=configuration,eft=task["EFT"],uv=task["UV"],eps=modelSymbol["eps"],plan,process1,process2,one,aux,ct,ctTree,uvTwo,prepared,insertions,treeChannels,
  directions,directionData,directionRules,directionChannels,oneConfig,hard=<||>,shifted=<||>,inferred,finite,correction=<||>,contributions=<||>,
  key,ch,cut,allHard,uvInsertion,eftCounterterm,uvTreePoles,terms,project,ctProject,poles},
 If[task["Request"]["Loops"]=!=2,Throw[fail["TwoLoopRequest","Request exactly two loops."],"FM2TheoryStage"]];
 If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
 cfg=Join[cfg,<|"TadpoleFree"->(Lookup[task["Request"],"Tadpole","MSbar"]==="heavy_tadpole_free")|>];
 plan=PlanMatching[task];If[FailureQ[plan],Throw[plan,"FM2TheoryStage"]];
 (* A UV counterterm insertion is already of total order h^2: it needs the
    final Taylor order, not the extra work order of one-loop matching. *)
 process1=Join[#,<|"taylor_order"->Max[#["momentum_order"],task["Request"]["MaxDimension"]-#["legs"]]|>]&/@Select[plan["tasks"],#["loops"]===1&];process2=Select[plan["tasks"],#["loops"]===2&];
 directions=Table[Symbol["FeAmMatch2`HardDirection"<>StringTake[task["Identity"],12]<>"`d"<>ToString[i]],{i,Length[eft["WilsonCoefficients"]]}];
 oneConfig=Join[cfg,<|"AuxiliaryLightWeight"->2,"PoleDirections"->Thread[eft["WilsonCoefficients"]->directions]|>];
 one=timedStage["OneLoopMatching",MatchTheoryOneLoop[task,out<>"/one-loop",oneConfig]];If[FailureQ[one],Throw[one,"FM2TheoryStage"]];writeResultArtifact[out,"one-loop-deep",one];
 aux=one["AuxiliaryBasis"];ct=one["UVRenormalization"];
 uvTwo=timedStage["UVTwoLoopAmplitudes",ComputeTheoryAmplitudes[task,2,out<>"/UV-two",Join[cfg,<|"Processes"->process2,"EpsilonOrder"->0|>]]];If[FailureQ[uvTwo],Throw[uvTwo,"FM2TheoryStage"]];writeResultArtifact[out,"uv-two-bare",uvTwo];
 prepared=PrepareInsertionTheory[uv,ct["OneLoop"]];If[FailureQ[prepared],Throw[prepared,"FM2TheoryStage"]];
 writeResultArtifact[out,"uv-one-loop-counterterms",<|"Action"->ct["OneLoop"],"Vertices"->(prepared["Vertices"]/.prepared["CoefficientRules"]),
  "PotentialPolesByTopology"->ct["PotentialPolesByTopology"],"TadpoleConvention"->ct["TadpoleConvention"],
  "HeavyTadpoleCancellation"->ct["HeavyTadpoleCancellation"],"GraphRecords"->ct["DiagramAmplitudes"]["Records"]|>];
 insertions=timedStage["UVInsertionAmplitudes",ComputeTheoryAmplitudes[task,1,out<>"/UV-insertions",Join[cfg,<|"GenerationTheory"->prepared["Theory"],"Processes"->process1,"InsertionSymbol"->prepared["Marker"],"CoefficientRules"->prepared["CoefficientRules"],"EpsilonOrder"->0|>]]];
 If[FailureQ[insertions],Throw[insertions,"FM2TheoryStage"]];writeResultArtifact[out,"uv-insertions",insertions];
 ctTree=timedStage["TreeCounterterms",treeActionCounterterms[task,task["Request"]["MaxDimension"],ct]];
 If[FailureQ[ctTree],Throw[ctTree,"FM2TheoryStage"]];writeResultArtifact[out,"uv-tree-counterterms",ctTree];
 treeChannels=timedStage["TreeCountertermChannels",actionTheoryChannels[eft,ctTree["TwoLoop"],uvTwo["Channels"]]];If[FailureQ[treeChannels],Throw[treeChannels,"FM2TheoryStage"]];
 directionData=one["LocalPoleData"];
 directionRules=Thread[directions->(Last/@one["OneLoopLaurentRules"])];
 directionChannels=actionTheoryChannels[eft,directionData["DirectionAction"]/.directionRules,uvTwo["Channels"]];
 If[FailureQ[directionChannels],Throw[directionChannels,"FM2TheoryStage"]];
 Do[ch=uvTwo["Channels"][key];
  cut[x_]:=matchingWeightTruncate[x,task,Most[ch["Momenta"]],Length[ch["ExternalFields"]],0,ch["Process"]["momentum_order"]];
  allHard=cut[ch["Expression"]];
  uvInsertion=cut[insertions["Channels"][key]["Expression"]];
  (* Apply the d-dimensional one-loop matching before the final Taylor cut. *)
  eftCounterterm=cut[directionChannels[key]["Expression"]];
  uvTreePoles=cut[treeChannels[key]["Expression"]];
  If[Expand[uvTreePoles-theoryPolePart[uvTreePoles,eps]]=!=0,
   Throw[fail["UVTreeCountertermPrecision","The Eq. (159) UV tree counterterms must be pure poles in the supported scheme."],"FM2TheoryStage"]];
  terms=Expand[allHard+uvInsertion-eftCounterterm];
  AssociateTo[contributions,key->Join[KeyDrop[ch,{"Expression"}],<|"UVAllHard"->allHard,
   "UVOneLoopInsertionHard"->uvInsertion,"EFTOneLoopCountertermDirection"->eftCounterterm|>]];
  AssociateTo[hard,key->Join[ch,<|"Expression"->Expand[allHard+uvInsertion+uvTreePoles]|>]];
  AssociateTo[shifted,key->Join[ch,<|"Expression"->Expand[terms+uvTreePoles]|>]];
  AssociateTo[correction,key->Join[ch,<|"Expression"->-Coefficient[Expand[eftCounterterm],eps,0]|>]],{key,Keys[uvTwo["Channels"]]}];
 inferred=transformTheoryChannels[shifted,Function[c,theoryPolePart[c["Expression"],eps]]];
 ctProject=timedStage["TwoLoopCountertermProjection",ProjectTheoryAmplitudes[eft,inferred]];If[FailureQ[ctProject],Throw[ctProject,"FM2TheoryStage"]];
 If[!FreeQ[Factor/@ctProject["Values"],_Log|_Clausen2],Throw[Failure["NonlocalHardCounterterm",<|"MessageTemplate"->"Inferred overall EFT counterterms contain heavy-scale logarithms after the one-loop direction subtraction.","Rules"->ctProject["Rules"]|>],"FM2TheoryStage"]];
 finite=transformTheoryChannels[shifted,Function[c,Expand[Coefficient[c["Expression"],eps,0]]]];
 project=ProjectTheoryAmplitudes[eft,finite];If[FailureQ[project],Throw[project,"FM2TheoryStage"]];poles=theoryPolePart[#,eps]&/@project["Values"];
 writeResultArtifact[out,"eq160-contributions",contributions];
 writeResultArtifact[out,"uv-hard-action",hard];writeResultArtifact[out,"finite-direction-correction",correction];writeResultArtifact[out,"inferred-trajectory-counterterms",ctProject];writeResultArtifact[out,"two-loop-assembled-channels",finite];
 <|"Stage"->"two_loop_green_complete","Method"->"UVHardMSbar","OneLoop"->one,"TwoLoopLaurentRules"->project["Rules"],"Projection"->project,"PoleResiduals"->poles,
  "EFTTwoLoopCounterterms"->ctProject,"CountertermOrigin"->"Inferred from UV hard action along the matching trajectory; not an independent pole-cancellation test",
  "UVTreeCounterterms"->ctTree,"DirectionRules"->directionRules,"LocalDirectionData"->directionData,"LocalPoleEvaluation"->"ExplicitOneLoop1PI","ExternalEFTGraphGeneration"->True,
  "Scope"->"Scalar UV hard matching with explicit one-loop 1PI pole extraction; the inferred overall pole is not a universal two-loop EFT counterterm function."|>
],"FM2TheoryStage"];
