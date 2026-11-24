(* Independent-action entry point. Only fully audited stages report complete. *)
ClearAll[runTheoryMatching];
truncateTheoryCoefficientRules[theory_,rules_,task_]:=Catch[Module[{terms,dimensions,value,output={}},
 Do[terms=ExtractScalarTerms[Join[theory,<|"Lagrangian"->D[theory["Lagrangian"],First[rule]]|>]];
  If[FailureQ[terms],Throw[terms,"FM2TheoryStage"]];dimensions=DeleteDuplicates[Lookup[Select[terms,#["Coefficient"]=!=0&],"OperatorDimension"]];
  If[Length[dimensions]=!=1,Throw[fail["CoefficientWeight","A coefficient must multiply operators with a unique local dimension."],"FM2TheoryStage"]];
  value=matchingWeightTruncate[Last[rule],task,{},First[dimensions]];AppendTo[output,First[rule]->Factor[value]],{rule,rules}];output
],"FM2TheoryStage"];
runTheoryMatching[task_,directory_,configuration_]:=Catch[Module[
 {out=ExpandFileName[directory],config=resolveToolchain[configuration],request=task["Request"],eft=task["EFT"],plan,basis,conversion,green,physical,h,loops,rules,conversionRules,physicalRules,one,two,finite,artifacts=<||>,status,preflight,sourceSnapshot,externalSnapshot},
 If[Complement[Keys[request],{"Loops","MaxDimension","PhysicalBasis","Scheme","Tadpole","Vacuum","ParameterWeights","LightFieldMap","PhysicalFieldMap","LoopSymbol"}]=!={},Throw[fail["RequestKeys","Recreate this task with the current CreateMatchingTask API."],"FM2TheoryStage"]];
 If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
 status=<|"Schema"->"feammatch2.run-status.v2","Status"->"running","TaskIdentity"->task["Identity"],"Loops"->request["Loops"],"MatchingMethod"->"UVHardMSbar","LocalPoleBackend"->"FeAmGen1PIFORM"|>;
 Export[FileNameJoin[{out,"run-status.json"}],status,"RawJSON"];
 If[!loadedSourceCurrentQ[],Throw[fail["ReloadPackage","Package sources changed after loading. Reload FeAmMatch2.wl before starting a new calculation."],"FM2TheoryStage"]];sourceSnapshot=packageSourceSnapshot[];
 preflight=CheckToolchain[config,"Mode"->"full","Probe"->TrueQ[Lookup[config,"ProbeTools",True]],"RequireABC4EFT"->(request["PhysicalBasis"]===Automatic)];
 If[FailureQ[preflight],Throw[preflight,"FM2TheoryStage"]];config=preflight["config"];Export[FileNameJoin[{out,"toolchain-check.json"}],preflight,"RawJSON"];
 externalSnapshot=externalSourceSnapshot[config];config=Join[config,<|"ExternalSourceIdentity"->theoryIdentity[externalSnapshot]|>];
 Export[FileNameJoin[{out,"runtime-manifest.json"}],<|"schema"->"feammatch2.runtime.v2","wolfram_version"->$Version,"package_root"->toolRoot[],"production_sources"->sourceSnapshot,"external_sources"->externalSnapshot,"task_identity"->task["Identity"]|>,"RawJSON"];
 writeResultArtifact[FileNameJoin[{out,"input"}],"uv-frozen",task["UV"]];writeResultArtifact[FileNameJoin[{out,"input"}],"eft-green-frozen",eft];writeResultArtifact[FileNameJoin[{out,"input"}],"task",task];
 plan=PlanMatching[task];If[FailureQ[plan],Throw[plan,"FM2TheoryStage"]];writeResultArtifact[FileNameJoin[{out,"matching"}],"plan",plan];
 basis=timedStage["ResolvePhysicalBasis",ResolvePhysicalBasis[eft,request,FileNameJoin[{out,"basis"}],config]];If[FailureQ[basis],Throw[basis,"FM2TheoryStage"]];
 conversion=timedStage["DeriveBasisConversion",DeriveBasisConversion[eft,basis,request]];If[FailureQ[conversion],Throw[conversion,"FM2TheoryStage"]];writeResultArtifact[FileNameJoin[{out,"conversion"}],"problem",conversion];
 green=timedStage["TreeMatching",diagramTreeMatching[task,out<>"/tree",config]];If[FailureQ[green],Throw[green,"FM2TheoryStage"]];writeResultArtifact[FileNameJoin[{out,"matching"}],"tree",green];
 config=Join[config,<|"DiagramTree"->green|>];
 loops=request["Loops"];h=Lookup[request,"LoopSymbol",modelSymbol["h"]];rules=green["Rules"];
 If[loops===2,
  two=MatchTheoryTwoLoop[task,FileNameJoin[{out,"loops","two"}],config];If[FailureQ[two],Throw[two,"FM2TheoryStage"]];one=two["OneLoop"];
  writeResultArtifact[FileNameJoin[{out,"green"}],"laurent-and-auxiliary",two];
  rules=Thread[eft["WilsonCoefficients"]->((Last/@rules)+h(Coefficient[#,modelSymbol["eps"],0]&/@(Last/@one["OneLoopLaurentRules"]))+h^2(Coefficient[#,modelSymbol["eps"],0]&/@(Last/@two["TwoLoopLaurentRules"])) )]];
 If[loops===1,
  one=MatchTheoryOneLoop[task,FileNameJoin[{out,"loops","one"}],config];If[FailureQ[one],Throw[one,"FM2TheoryStage"]];
  writeResultArtifact[FileNameJoin[{out,"green"}],"laurent-and-auxiliary",one];
  finite=Last/@one["OneLoopLaurentRules"];finite=Coefficient[#,modelSymbol["eps"],0]&/@finite;
  rules=Thread[eft["WilsonCoefficients"]->((Last/@rules)+h finite)]];
 conversionRules=rules;rules=truncateTheoryCoefficientRules[eft,rules,task];If[FailureQ[rules],Throw[rules,"FM2TheoryStage"]];
 writeResultArtifact[FileNameJoin[{out,"conversion"}],"input-coefficients",conversionRules];
 writeResultArtifact[FileNameJoin[{out,"green"}],"coefficients",rules];writeResultArtifact[FileNameJoin[{out,"green"}],"lagrangian",eft["Lagrangian"]/.rules];
 physical=timedStage["ApplyBasisConversion",ApplyBasisConversion[conversion,conversionRules]];If[FailureQ[physical],Throw[physical,"FM2TheoryStage"]];
 physicalRules=truncateTheoryCoefficientRules[basis,physical["Rules"],task];If[FailureQ[physicalRules],Throw[physicalRules,"FM2TheoryStage"]];
 physical=Join[physical,<|"UntruncatedRules"->physical["Rules"],"Rules"->physicalRules,"Lagrangian"->(basis["Lagrangian"]/.physicalRules),"PowerCounting"->request["ParameterWeights"]|>];
 writeResultArtifact[FileNameJoin[{out,"physical"}],"coefficients",physical["Rules"]];writeResultArtifact[FileNameJoin[{out,"physical"}],"lagrangian",physical["Lagrangian"]];
 writeResultArtifact[FileNameJoin[{out,"conversion"}],"field-map",<|"Normalization"->physical["KineticNormalization"]["FieldRules"],"WeightedLocalMaps"->physical["WeightedFieldMaps"],"WeightSymbol"->physical["WeightSymbol"]|>];
 writeResultArtifact[FileNameJoin[{out,"conversion"}],"coefficient-map",physical];
 Export[FileNameJoin[{out,"green","coefficients.tex"}],ToString[TeXForm[rules]],"Text"];Export[FileNameJoin[{out,"green","lagrangian.tex"}],ToString[TeXForm[eft["Lagrangian"]/.rules]],"Text"];
 Export[FileNameJoin[{out,"physical","coefficients.tex"}],ToString[TeXForm[physical["Rules"]]],"Text"];Export[FileNameJoin[{out,"physical","lagrangian.tex"}],ToString[TeXForm[physical["Lagrangian"]]],"Text"];
 If[sourceSnapshot=!=packageSourceSnapshot[],Throw[fail["SourcesChangedDuringRun","Production sources changed during this run. Checkpoints were retained, but the mixed-source run is not published as complete."],"FM2TheoryStage"]];
 If[externalSnapshot=!=externalSourceSnapshot[config],Throw[fail["ExternalSourcesChanged","An external tool or physics source changed during the run. Checkpoints were retained."],"FM2TheoryStage"]];
 status=Join[status,<|"Status"->"complete","GreenAudit"->"passed","PhysicalAudit"->"passed","PhysicalBasisIdentity"->basis["Identity"],"SourceIdentity"->theoryIdentity[sourceSnapshot],"ExternalSourceIdentity"->theoryIdentity[externalSnapshot],
  "Coverage"-><|"GreenRank"->plan["Response"]["Rank"],"CoefficientCount"->Length[eft["WilsonCoefficients"]],"GeneratedChannelCount"->Length[plan["Response"]["Channels"]],"AllGeneratedMomentumRowsChecked"->True,"FullGreenBasisCompletenessProven"->False|>,
  "ExternalEFTGraphGeneration"->(loops>0),
  "OverallTwoLoopPoleAudit"->"Inferred matching-trajectory counterterms; pole removal itself is not independent validation",
  "Scope"->"Real scalar components, canonical diagonal UV free action, nonderivative renormalizable UV potential, dimensions through six and one heavy threshold. Ordered formal dimension-six physical conversion; auxiliary closure is not a general joint-threshold expansion proof. Passing generated channels is not a full Green-basis completeness proof."|>];
 If[!DirectoryQ[FileNameJoin[{out,"audit"}]],CreateDirectory[FileNameJoin[{out,"audit"}]]];Export[FileNameJoin[{out,"audit","report.json"}],status,"RawJSON"];Export[FileNameJoin[{out,"run-status.json"}],status,"RawJSON"];
 <|"Stage"->"complete","Directory"->out,"Loops"->loops,"GreenRules"->rules,"GreenLagrangian"->(eft["Lagrangian"]/.rules),"Physical"->physical,"Audit"->status|>
],"FM2TheoryStage"];
RunMatching[task_Association/;Lookup[task,"Schema",""]==="feammatch2.matching-task.v2",directory_String,configuration_Association]:=Block[
 {$formTaskCache=<||>,$formExecutionStats=<|"Executed"->0,"Hits"->0,"ExecutionSeconds"->0.|>,$performanceEvents={},$performanceStarted=AbsoluteTime[]},Module[{result,out=ExpandFileName[directory]},
 result=runTheoryMatching[task,out,configuration];
 If[FailureQ[result],If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
  Put[result,FileNameJoin[{out,"failure.wl"}]];Export[FileNameJoin[{out,"run-status.json"}],<|"Schema"->"feammatch2.run-status.v2","Status"->"failed","TaskIdentity"->task["Identity"],"FailureTag"->First[result],"Failure"->ToString[result,InputForm]|>,"RawJSON"]];
 Export[FileNameJoin[{out,"performance.json"}],<|"Schema"->"feammatch2.performance.v1","Status"->If[FailureQ[result],"failed","complete"],"ElapsedSeconds"->N[AbsoluteTime[]-$performanceStarted],"FORM"->$formExecutionStats,"Events"->$performanceEvents,"Scope"->"This execution only. Nested event times overlap; total includes graph generation. No cross-run speedup inferred."|>,"RawJSON"];result
]];

RunMatching[___]:=fail["MatchingTask","Use RunMatching[CreateMatchingTask[uv,eft,request],directory,config]."];
