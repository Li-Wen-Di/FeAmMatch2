Begin["FeAmMatch2`Private`"];
(* One-loop Tr log of the scalar Hessian. Background derivatives are obtained
   from exact local vertices, with two quantum slots; cyclic ordered labelled
   partitions implement functional differentiation of the trace. No UFO,
   QGRAF, FeAmGen, or stored pole/matching table is used. *)
ClearAll[localPolePartitions,localPolePolynomialCut,localPoleUVConvergentQ];
localPolePartitions[labels_List,n_Integer,sizes_List,empty_Integer:0]:=Module[{result={},rest},
 If[n===0,Return[If[labels==={},{{}},{}]]];
 If[n===1,Return[If[MemberQ[sizes,Length[labels]]&&!(labels==={}&&empty>0),{{labels}},{}]]];
 Do[If[size===0&&empty>0,Continue[]];
  Do[rest=Complement[labels,block];
   result=Join[result,Prepend[#,block]&/@localPolePartitions[rest,n-1,sizes,empty+Boole[size===0]]],
   {block,Subsets[labels,{size}]}],{size,Select[sizes,#<=Length[labels]&]}];result
];
localPolePolynomialCut[x_,r_,t_,maximum_]:=Total[
 (Last[#]r^First[#][[1]]t^First[#][[2]])&/@Select[CoefficientRules[Expand[x],{r,t}],First[#][[1]]<=maximum&&First[#][[2]]<=1&]];

(* One loop, n scalar propagators: delta <= 4 + rank_q(numerator) - 2 n.
   Only strictly convergent terms may be discarded. Unknown bounds and
   explicit regulator poles are retained. Reference masses stay nonzero. *)
localPoleUVConvergentQ[numerator_,q_,vectors_List,n_Integer,eps_]:=Module[{s=Unique["uvRank"],x,rank},
 If[!FreeQ[numerator,eps],Return[False]];
 x=expandScalarProducts[numerator,vectors];
 x=x/.sp_SP:>s^Count[List@@sp,q] sp;
 If[!PolynomialQ[x,s],Return[False]];
 rank=Exponent[x,s];TrueQ[4+rank-2n<0]
];

FeAmMatch2`ComputeLocalOneLoopPoles[task_Association,eft_Association,baseRules_List,processes_List,directory_String,configuration_Association]:=Catch[Module[
 {out=ExpandFileName[directory],cfg=configuration,fields=eft["FieldSymbols"],massRules,masses,
  rho=Unique["poleWeight"],t=Unique["poleDirection"],q=modelSymbol["q1"],eps=modelSymbol["eps"],
  directions,weighted,interaction,quadratic,sizes,maximum,cache=<||>,vertex,slots,
  channels=<||>,directionChannels=<||>,records={},p,external,momenta,independent,blocks,cycles,
  integrand,term,kernel,routes,states,ff,key,value,prefactor,processed=0,result,laurent,expression,
  limit,partitionCache=<||>,partitionKey,optimized,zeroExternal,convergent=0,
  auditMode,auditRecords={},recordPath,record,started=AbsoluteTime[],channelStats={},beforeProcessed,beforeConvergent},
 If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
 cfg=Join[<|"WorkingDirectory"->FileNameJoin[{out,"form"}]|>,configuration];
 directions=Lookup[cfg,"PoleDirections",{}];maximum=Lookup[cfg,"MaxCouplingWeight",task["Request"]["MaxDimension"]-4];
 limit=Lookup[cfg,"LocalPoleTermLimit",2000000];
 optimized=TrueQ[Lookup[cfg,"OptimizeLocalPoles",True]];
 auditMode=Lookup[cfg,"LocalPoleAudit","External"];
 If[!MemberQ[{"External","Embedded"},auditMode],Throw[fail["LocalPoleAudit","Choose External or Embedded for LocalPoleAudit."],"FM2TheoryStage"]];
 massRules=Table[f["Mass"]->SelectFirst[task["UV"]["Fields"],#["Symbol"]===(f["Symbol"]/.task["LightFieldMap"])&]["Mass"],{f,eft["Fields"]}];
 masses=Association[Table[f["Symbol"]->(f["Mass"]/.massRules),{f,eft["Fields"]}]];
 If[MemberQ[Values[masses],0],Throw[fail["MasslessPoleExtraction","The local Hessian backend currently requires nonzero reference light masses."],"FM2TheoryStage"]];
 weighted=eft["Lagrangian"]/.Table[c->rho^Max[0,-eft["Parameters"][c]["Dimension"]]((c/.baseRules)+t If[MemberQ[First/@directions,c],c/.directions,0]),{c,eft["WilsonCoefficients"]}];
 interaction=scalarActionTheory[eft,(weighted-freeScalarAction[eft])/.massRules];If[FailureQ[interaction],Throw[interaction,"FM2TheoryStage"]];
 (* The reference free kernel is exact. Zero-background perturbations are
    permitted once in the direction, including mass, kinetic and Box^2. *)
 quadratic=Select[interaction["Terms"],#["FieldDegree"]<=2&&Expand[#["Coefficient"]/.t->0]=!=0&];
 If[quadratic=!={},Throw[fail["LocalPoleFreeKernel","Tree quadratic action must equal the declared canonical diagonal massive reference kernel."],"FM2TheoryStage"]];
 sizes=DeleteDuplicates[#["FieldDegree"]-2&/@Select[interaction["Terms"],#["FieldDegree"]>=2&]];
 slots=Table[Symbol["FeAmMatch2`LocalPoleMomenta`v"<>ToString[i]],{i,Max[2,Max[0,Sequence@@sizes]+2]}];
 vertex[species_List]:=Module[{id=ToString[species,InputForm],v},
  If[KeyExistsQ[cache,id],Return[cache[id]]];
  v=ScalarLocalVertex[interaction,species,Take[slots,Length[species]]];If[FailureQ[v],Throw[v,"FM2TheoryStage"]];AssociateTo[cache,id->v];v];
 Do[
  p=process;external=p["EFTFields"];momenta=independentExternalMomenta[Length[external]];independent=Most[momenta];integrand=0;
  beforeProcessed=processed;beforeConvergent=convergent;
  zeroExternal=optimized&&p["momentum_order"]===0;
  Do[
   (* Every nonempty labelled cycle has exactly one rotation with label 1
      in its first block. This cancels the 1/n in Tr log, leaving 1/2. *)
   partitionKey=ToString[{Length[external],n},InputForm];
   If[!KeyExistsQ[partitionCache,partitionKey],AssociateTo[partitionCache,partitionKey->Select[localPolePartitions[Range[Length[external]],n,sizes],MemberQ[First[#],1]&]]];
   blocks=partitionCache[partitionKey];
   Do[routes=If[zeroExternal,ConstantArray[q,n+1],FoldList[Plus,q,Total[momenta[[#]]]&/@partition]];
    Do[term=1;
     Do[ff=Join[{state[[i]],state[[Mod[i,n]+1]]},external[[partition[[i]]]]];kernel=vertex[ff];
      If[kernel===0,term=0;Break[]];
      kernel=kernel/.Thread[Take[slots,Length[ff]]->Join[{routes[[i]],-routes[[i+1]]},If[zeroExternal,ConstantArray[0,Length[partition[[i]]]],momenta[[partition[[i]]]]]]];
      term=localPolePolynomialCut[term kernel,rho,t,maximum];If[term===0,Break[]],{i,n}];
     If[term===0,Continue[]];processed++;If[processed>limit,Throw[fail["LocalPoleResourceLimit","Local Hessian trace exceeded LocalPoleTermLimit; no incomplete result was published."],"FM2TheoryStage"]];
     If[optimized&&localPoleUVConvergentQ[term,q,Join[{q},independent],n,eps],convergent++;Continue[]];
     term=(-1)^n term Times@@Table[Den[routes[[i]],masses[state[[i]]],0],{i,n}]/2;
     integrand+=term,
     {state,Tuples[fields,n]}],{partition,blocks}],
   {n,1,Length[external]+Boole[MemberQ[sizes,0]]}];
  integrand=Expand[integrand/.{rho->1,Last[momenta]->-Total[independent]}];
  integrand=Expand[expandScalarProducts[integrand,Join[{q},independent]]];
  If[integrand===0,expression=0;result=<|"Stage"->"zero_hessian_trace"|>;laurent=<||>,
   Print["Local Hessian pole ",Length[external]," legs, ",p["momentum_order"]," external-momentum order"];
   result=EvaluateHardAmplitude[integrand,{q},independent,DeleteDuplicates[Values[masses]],{},p["momentum_order"],cfg];If[FailureQ[result],Throw[result,"FM2TheoryStage"]];
   laurent=LaurentVacuum[result["Expression"],eps,modelSymbol["mu"],-1,cfg];If[FailureQ[laurent],Throw[laurent,"FM2TheoryStage"]];expression=Coefficient[Expand[laurent["Expression"]],eps,-1]/eps];
  key=theoryChannelKey[external];
  AssociateTo[channels,key-><|"ExternalFields"->external,"Expression"->Expand[Coefficient[expression,t,0]],"Momenta"->momenta,"Process"->p|>];
  AssociateTo[directionChannels,key-><|"ExternalFields"->external,"Expression"->Expand[Coefficient[expression,t,1]],"Momenta"->momenta,"Process"->p|>];
  record=<|"Channel"->external,"Integrand"->integrand,"Reduction"->result,"Laurent"->laurent|>;
  If[auditMode==="Embedded",AppendTo[records,record],
   recordPath=writeResultArtifact[out<>"/records","channel-"<>key,record];
   AppendTo[auditRecords,<|"Channel"->external,"Path"->recordPath,"SHA256"->toolHash[recordPath]|>]];
  AppendTo[channelStats,<|"Channel"->external,"KernelTerms"->processed-beforeProcessed,"UVConvergentTermsSkipped"->convergent-beforeConvergent,"EarlyZeroExternalMomenta"->zeroExternal|>],{process,processes}];
 result=<|"Stage"->"local_one_loop_poles_complete","Channels"->channels,"DirectionChannels"->directionChannels,
  "PoleDirections"->directions,"ExternalEFTGraphGeneration"->False,"KernelTerms"->processed,
  "AuditMode"->auditMode,"AuditRecords"->auditRecords,
  "Optimization"-><|"Enabled"->optimized,"UVConvergentTermsSkipped"->convergent,"Channels"->channelStats,"ElapsedSeconds"->N[AbsoluteTime[]-started]|>,
  "Algorithm"->"Scalar Hessian Tr log; labelled cyclic background derivatives; exact-D FORM/OPITeR massive vacuum poles",
  "SignConvention"->"Channels contain loop-divergence residues; action counterterms are their negatives."|>;
 If[auditMode==="Embedded",AssociateTo[result,"Records"->records]];
 writeResultArtifact[out,"local-one-loop-poles",result];result
],"FM2TheoryStage"];

End[];
