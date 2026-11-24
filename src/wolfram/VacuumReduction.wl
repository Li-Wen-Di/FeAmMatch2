(* Exact-D dispatch. No generic four-dimensional master substitution occurs here. *)
ClearAll[reduceTadpoleFORM];
reduceTadpoleFORM[n_Integer,mass_,config_]:=Module[{header,rules,back,source,job,expr,symbols,nmax},
 If[n<=0||TrueQ[PossibleZeroQ[mass]],Return[<|"Expression"->0,"Stage"->"scaleless_vacuum"|>]];
 If[n===1,Return[<|"Expression"->VacuumIntegral[1,mass],"Stage"->"tadpole_master"|>]];
 header=Lookup[DefaultToolchain[config],"SunsetHeader",Missing[]];
 If[!StringQ[header]||!FileExistsQ[header],Return[fail["SunsetHeader","Bundled sunset header is missing, or SunsetHeader override is invalid."]]];
 symbols=DeleteDuplicates[Cases[{mass},_Symbol,Infinity]];
 rules=MapIndexed[#1->Symbol["fmMass"<>ToString[First[#2]]]&,symbols];
 back=rules/.Rule[x_,y_]:>Rule[Symbol["FeAmMatch2`FORM`"<>SymbolName[y]],x];
 nmax=Max[2,n];
 source="#include sunset-proc.h\n#define N \""<>ToString[nmax]<>"\"\nCF F;\nSymbols a1,a2,a3,m1,m2,m3,D"<>
   If[symbols=!={},","<>StringRiffle[formText/@(symbols/.rules),","],""]<>";\nOff Statistics;\nL Result=F("<>ToString[n]<>","<>formText[mass/.rules]<> ");\n#call OneLoopRecursion\nid F(0,m1?)=0;\n.sort\nFormat Mathematica;\n#write <result.out> \"%E\",Result\n.end\n";
 job=RunFORMTask[source,Join[config,<|"Assets"-><|"sunset-proc.h"->header|>|>]];
 If[FailureQ[job],Return[job]];
 expr=readExpression[FileNameJoin[{job["directory"],"result.out"}]];
 If[FailureQ[expr],Return[expr]];
 <|"Expression"->(expr/.back),"Stage"->"exact_D_tadpole_reduced","Job"->job|>
];

ReduceVacuumIntegrals[expression_,config_Association]:=Module[
 {reduce,cache=<||>,jobs={},key,ints,replacements,result},
 reduce[vi_VacuumIntegral]:=Module[{args=List@@vi,idx,masses,j,other,polynomial,tensor,family,r,q=Unique["boundaryQ"],s=Unique["boundaryR"]},
  key=ToString[vi,InputForm];If[KeyExistsQ[cache,key],Return[cache[key]]];
  If[Length[args]===2,
   If[!IntegerQ[args[[1]]],Return[fail["IntegralIndex","Integer integral index required."]]];
   r=reduceTadpoleFORM[args[[1]],args[[2]],config],
   If[Length[args]=!=6,Return[fail["IntegralArity","Expected two or six integral arguments."]]];
   idx=Take[args,3];masses=Drop[args,3];
   If[!AllTrue[idx,IntegerQ],Return[fail["IntegralIndex","Integer integral indices required."]]];
   If[Count[idx,x_/;x<=0]>=2,r=<|"Expression"->0|>,
    If[Min[idx]<0,
     j=First[FirstPosition[idx,x_/;x<0]];other=Delete[Range[3],j];
     polynomial=(SP[q,q]+SP[s,s]-2 SP[q,s]-masses[[j]]^2)^(-idx[[j]]);
     (* A missing line makes the two remaining integrations independent. OPITeR
        reduces one loop with the second momentum as its external projector. *)
     tensor=ReduceHardTensor[polynomial*VacuumDenominator[1,masses[[other[[1]]]],idx[[other[[1]]]]]*
        VacuumDenominator[2,masses[[other[[2]]]],idx[[other[[2]]]]],{q},{s},config];
     If[FailureQ[tensor],Return[tensor]];AppendTo[jobs,tensor["Job"]];
     family=NormalizeVacuumFamilies[tensor["Expression"],{q,s},{},config];
     If[FailureQ[family],Return[family]];AppendTo[jobs,family["Job"]];
     ints=DeleteDuplicates[Cases[family["Expression"],_VacuumIntegral,{0,Infinity}]];
     replacements=Table[vi2->reduce[vi2],{vi2,ints}];
     If[AnyTrue[Last/@replacements,FailureQ],Return[SelectFirst[Last/@replacements,FailureQ]]];
     r=<|"Expression"->(family["Expression"]/.replacements)|>,
     r=ReduceSunset[idx,masses,config]]]];
  If[FailureQ[r],Return[r]];
  If[KeyExistsQ[r,"Job"],AppendTo[jobs,r["Job"]]];
  AssociateTo[cache,ToString[vi,InputForm]->r["Expression"]];r["Expression"]
 ];
 ints=DeleteDuplicates[Cases[expression,_VacuumIntegral,{0,Infinity}]];
 replacements=Table[vi->reduce[vi],{vi,ints}];
 If[AnyTrue[Last/@replacements,FailureQ],Return[SelectFirst[Last/@replacements,FailureQ]]];
 result=(expression/.replacements)/.VacuumIntegral[1,1,1,x_,y_,z_]:>Apply[VacuumIntegral,Join[{1,1,1},Sort[{x,y,z}]]];
 <|"Expression"->result,"Stage"->"exact_D_vacuum_reduced","Jobs"->jobs,"PhysicalCoefficients"->Null|>
];

EvaluateHardAmplitude[expression_,loops_List,external_List,heavy_List,light_List,order_Integer,config_Association]:=Module[
 {hard,fractions,tensor,family,reduced},
 hard=timedStage["FORMHardExpansion",HardExpand[expression,loops,external,heavy,light,order,config]];If[FailureQ[hard],Return[hard]];
 If[loops==={},Return[hard]];
 fractions=timedStage["FORMPartialFractions",partialFractionVacuum[hard["Expression"],loops,external,config]];If[FailureQ[fractions],Return[fractions]];
 tensor=timedStage["FORMTensorReduction",ReduceHardTensor[fractions["Expression"],loops,external,config]];If[FailureQ[tensor],Return[tensor]];
 family=timedStage["FORMFamilyNormalization",normalizeSingleMassFamilies[tensor["Expression"],loops,external,config]];If[FailureQ[family],Return[family]];
 reduced=timedStage["FORMIntegralReduction",ReduceVacuumIntegrals[family["Expression"],config]];If[FailureQ[reduced],Return[reduced]];
 Join[reduced,<|"Stages"-><|"Hard"->hard,"PartialFractions"->fractions,"Tensor"->tensor,"Family"->family|>|>]
];
