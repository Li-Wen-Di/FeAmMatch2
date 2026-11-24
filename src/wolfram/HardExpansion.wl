(* Loaded in FeAmMatch2`Private`. FORM performs polynomial expansion and truncation. *)
ClearAll[linearMomentumQ, scalarDot, expandScalarProducts];
linearMomentumQ[x_,vectors_List]:=TrueQ[Expand[x-Total[(Coefficient[Expand[x],#]*#)&/@vectors]]===0]&&
 AllTrue[Coefficient[Expand[x],#]&/@vectors,NumberQ];
scalarDot[a_,b_,vectors_List]:=Expand[Total[Flatten[Table[
 Coefficient[Expand[a],u]*Coefficient[Expand[b],v]*(SP@@Sort[{u,v}]),{u,vectors},{v,vectors}]]]];
expandScalarProducts[x_,vectors_List]:=x/.SP[a_,b_]:>scalarDot[a,b,vectors];

HardExpand[expression_,loops_List,external_List,heavy_List,light_List,order_Integer,config_Association]:=Catch[Module[
 {expr=expression,vectors,dens,num,slots,scalars,rules,back,td=Unique["hardWeight"],baseKeys={},
  d,mom,m,width,lp,ep,route,key,base,delta,series,source,job,result,pieces={},aliases={},
  massScales,sps,loop1,loop2,reflection,encoded},
 If[Length[loops]>2||order<0||Length[DeleteDuplicates[Join[loops,external]]]!=Length[Join[loops,external]],Return[fail["HardMomenta","Declare zero to two distinct loop momenta and disjoint external vectors."]]];
 If[Intersection[heavy,light]=!={},Return[fail["HardMasses","Heavy and light masses overlap."]]];
 If[!AllTrue[Join[loops,external],Head[#]===Symbol&],Return[fail["HardMomenta","Momenta must be symbols."]]];
 vectors=Join[loops,external];
 dens=DeleteDuplicates[Cases[expr,_Den,{0,Infinity}]];
 If[!AllTrue[dens,Length[#]===3&&#[[3]]===0&&MemberQ[Join[{0},heavy,light],#[[2]]]&&linearMomentumQ[#[[1]],vectors]&],Return[fail["HardPropagator","Each propagator needs a declared mass, zero width and numeric linear momentum."]]];
 If[dens=!={}&&!PolynomialQ[expr,dens],Return[fail["HardPropagatorPower","Propagator objects may only have nonnegative integer powers."]]];
 If[Length[loops]===2,
  {loop1,loop2}=loops;
  reflection=DeleteDuplicates[Sign[Coefficient[Expand[#[[1]]],loop1]*Coefficient[Expand[#[[1]]],loop2]]&/@dens];
  If[MemberQ[reflection,-1],
   If[MemberQ[reflection,1],Return[fail["HardRouting","Both sum and difference loop routes occur; reroute before Taylor expansion."]]];
   expr=expr/.loop2->-loop2;dens=DeleteDuplicates[Cases[expr,_Den,{0,Infinity}]]]];
 slots=Table[Unique["hardPropagator"],Length[dens]];
 num=expr/.Thread[dens->slots];
 sps=DeleteDuplicates[Cases[num,_SP,{0,Infinity}]];
 If[!AllTrue[sps,Length[#]===2&&AllTrue[List@@#,linearMomentumQ[#,vectors]&]&],Return[fail["HardNumerator","Scalar products must use numeric linear momenta."]]];
 num=expandScalarProducts[num,vectors];
 num=num/.sp_SP:>td^Count[List@@sp,Alternatives@@external]*sp;
 num=num/.Thread[light->(td*light)];
 Do[
  {mom,m,width}=List@@d;
  lp=Expand[Total[(Coefficient[Expand[mom],#]*#)&/@loops]];ep=Expand[mom-lp];
  If[lp=!=0&&First[Select[Coefficient[lp,#]&/@loops,#=!=0&]]<0,lp=-lp;ep=-ep];
  route=Which[lp===0,0,Length[loops]>=1&&lp===loops[[1]],1,
    Length[loops]===2&&lp===loops[[2]],2,Length[loops]===2&&lp===Total[loops],3,True,-1];
  If[route<0,Throw[fail["HardRouting","Only canonical q1,q2,q1+q2 vacuum routes are accepted."],"HardExpansionFailure"]];
  If[route===0&&!MemberQ[heavy,m],Throw[fail["LightBridge","A light tree propagator is not a local 1LPI hard contribution."],"HardExpansionFailure"]];
  key={route,If[MemberQ[heavy,m],m,0]};
  If[route===0,base=-1/m^2,
   If[!MemberQ[baseKeys,key],AppendTo[baseKeys,key]];
   base=Symbol["fmBase"<>ToString[First[FirstPosition[baseKeys,key]]]]];
  delta=2 td scalarDot[lp,ep,vectors]+td^2(scalarDot[ep,ep,vectors]-If[MemberQ[light,m],m^2,0]);
  (* Only assemble a geometric series here. FORM expands products and removes terms by weight. *)
  series=base+Total[Table[(-delta)^j*base^(j+1),{j,1,order}]];
  AppendTo[pieces,series],{d,dens}];
 (* Expand one distinct propagator at a time and discard excess Taylor weight
    between substitutions. Expanding their full product before truncation is
    exponentially expensive for independent off-shell external momenta. *)
 scalars=DeleteDuplicates[Cases[{num,pieces,baseKeys},x_Symbol/;!MemberQ[Join[vectors,slots,{SP,td}],x]&&!StringStartsQ[SymbolName[x],"fmBase"],Infinity]];
 rules=Join[MapIndexed[#1->Symbol["fmV"<>ToString[First[#2]]]&,vectors],
  MapIndexed[#1->Symbol["fmC"<>ToString[First[#2]]]&,scalars],
  MapIndexed[#1->Symbol["fmProp"<>ToString[First[#2]]]&,slots],{td->Symbol["fmWeight"]}];
 back=rules/.Rule[x_,y_]:>Rule[Symbol["FeAmMatch2`FORM`"<>SymbolName[y]],x];
 encoded=formText[num/.rules];
 If[!StringQ[encoded],Return[fail["HardNumerator","Unsupported numerator or coefficient head."]]];
 source="Off Statistics;\nSymbols fmWeight,fmPower,fmImag"<>
  If[Length[scalars]>0,","<>StringRiffle[formText/@(scalars/.rules),","],""]<>
  If[Length[baseKeys]>0,","<>StringRiffle[Table["fmBase"<>ToString[i],{i,Length[baseKeys]}],","],""]<>";\n"<>
  If[Length[slots]>0,"Symbols "<>StringRiffle[formText/@(slots/.rules),","]<>";\n",""]<>
  If[Length[vectors]>0,"Vectors "<>StringRiffle[formText/@(vectors/.rules),","]<>";\n",""]<>
  "CF fmVD;\nL Result="<>encoded<>";\n.sort\n"<>
  "if (count(fmWeight,1)>"<>ToString[order]<>") discard;\n"<>
  StringJoin[MapThread["id "<>formText[#1/.rules]<>"="<>formText[#2/.rules]<>";\nif (count(fmWeight,1)>"<>ToString[order]<>") discard;\n.sort\n"&,{slots,pieces}]]<>
  "id fmWeight=1;\n"<>
  StringJoin[MapIndexed["id fmBase"<>ToString[First[#2]]<>"^fmPower?pos_=fmVD("<>
   ToString[#1[[1]]]<>","<>formText[#1[[2]]/.rules]<>",fmPower);\n"&,baseKeys]]<>
  ".sort\nFormat Mathematica;\n#write <result.out> \"%E\",Result\n.end\n";
 job=RunFORMTask[source,config];If[FailureQ[job],Return[job]];
 result=readExpression[FileNameJoin[{job["directory"],"result.out"}]];If[FailureQ[result],Return[result]];
 result=result/.back/.SP[a_,b_]:>SP@@Sort[{a,b}];
 <|"Expression"->result,"Stage"->"hard_taylor_expanded","Order"->order,"Job"->job,"PhysicalCoefficients"->Null|>
],"HardExpansionFailure"];

ReduceHardTensor[expression_,loops_List,external_List,config_Association]:=Module[{dens,slots,z},
 If[Length[loops]===0,Return[<|"Expression"->expression,"Stage"->"tree_local_expanded","PhysicalCoefficients"->Null|>]];
 dens=DeleteDuplicates[Cases[expression,_VacuumDenominator,{0,Infinity}]];
 slots=Table[Unique["scalarDenominator"],Length[dens]];
 z=ReduceVacuumTensor[expression/.Thread[dens->slots],loops,external,config];
 If[FailureQ[z],Return[z]];
 Join[z,<|"Expression"->(z["Expression"]/.Thread[slots->dens])|>]
];

partialFractionVacuum[expression_,loops_List,external_List,config_Association]:=Module[
 {terms,pairs,dens,vectors=Join[loops,external],scalars,rules,back,source,job,result},
 terms=If[Head[expression]===Plus,List@@expression,{expression}];
 pairs=DeleteDuplicates[Flatten[Table[
   dens=DeleteDuplicates[Cases[term,_VacuumDenominator,{0,Infinity}]];
   Cases[Subsets[dens,{2}],{VacuumDenominator[r_,a_,_],VacuumDenominator[r_,b_,_]} /; a=!=b :> {r,Sequence@@Sort[{a,b}]}],
   {term,terms}],1]];
 If[pairs==={},Return[<|"Expression"->expression,"Stage"->"single_mass_per_route"|>]];
 If[AnyTrue[pairs,TrueQ[PossibleZeroQ[#[[2]]^2-#[[3]]^2]]&],
  Return[fail["VacuumMasses","Equal mass squares must have a common mass representative."]]];
 scalars=DeleteDuplicates[Cases[expression,x_Symbol/;!MemberQ[Join[vectors,{SP,VacuumDenominator,D}],x],Infinity]];
 rules=Join[MapIndexed[#1->Symbol["fmV"<>ToString[First[#2]]]&,vectors],MapIndexed[#1->Symbol["fmC"<>ToString[First[#2]]]&,scalars]];
 back=rules/.Rule[x_,y_]:>Rule[Symbol["FeAmMatch2`FORM`"<>SymbolName[y]],x];
 source="Off Statistics;\nSymbols fmR,fmA,fmB,fmN,fmM,fmImag,D"<>
   If[scalars=!={},","<>StringRiffle[formText/@(scalars/.rules),","],""]<>";\n"<>
   If[vectors=!={},"Vectors "<>StringRiffle[formText/@(vectors/.rules),","]<>";\n",""]<>
   "CF fmVD;\nL Result="<>formText[expression/.rules]<>";\n"<>
   "id fmVD(fmR?,fmM?,fmA?)^fmN?pos_=fmVD(fmR,fmM,fmA*fmN);\nrepeat;\n"<>
   "id fmVD(fmR?,fmM?,0)=1;\n"<>
   "id fmVD(fmR?,fmM?,fmA?)*fmVD(fmR?,fmM?,fmB?)=fmVD(fmR,fmM,fmA+fmB);\n"<>
   StringJoin[("id fmVD("<>ToString[#[[1]]]<>","<>formText[#[[2]]/.rules]<>",fmA?pos_)*fmVD("<>
    ToString[#[[1]]]<>","<>formText[#[[3]]/.rules]<>",fmB?pos_)=(fmVD("<>
    ToString[#[[1]]]<>","<>formText[#[[2]]/.rules]<>",fmA)*fmVD("<>ToString[#[[1]]]<>","<>formText[#[[3]]/.rules]<>",fmB-1)-fmVD("<>
    ToString[#[[1]]]<>","<>formText[#[[2]]/.rules]<>",fmA-1)*fmVD("<>ToString[#[[1]]]<>","<>formText[#[[3]]/.rules]<>",fmB))/("<>
    formText[(#[[2]]^2-#[[3]]^2)/.rules]<> ");\n")&/@pairs]<>
   "endrepeat;\n.sort\nFormat Mathematica;\n#write <result.out> \"%E\",Result\n.end\n";
 job=RunFORMTask[source,config];If[FailureQ[job],Return[job]];
 result=readExpression[FileNameJoin[{job["directory"],"result.out"}]];If[FailureQ[result],Return[result]];
 <|"Expression"->(result/.back/.SP[a_,b_]:>SP@@Sort[{a,b}]),"Stage"->"vacuum_partial_fractions","Job"->job|>
];

NormalizeVacuumFamilies[expression_,loops_List,external_List,config_Association]:=Module[{fractions},
 fractions=partialFractionVacuum[expression,loops,external,config];
 If[FailureQ[fractions],Return[fractions]];
 normalizeSingleMassFamilies[fractions["Expression"],loops,external,config]
];

normalizeSingleMassFamilies[expression_,loops_List,external_List,config_Association]:=Module[
 {expr=expression,qd=Table[Symbol["fmQuadratic"<>ToString[i]],{i,3}],dens,allowed,
  rules,back,vectors=Join[loops,external],source,job,result,sps,scalars},
 If[!MemberQ[{1,2},Length[loops]],Return[fail["VacuumLoops","One or two loops required."]]];
 sps=DeleteDuplicates[Cases[expr,_SP,{0,Infinity}]];
 allowed=If[Length[loops]===1,{SP@@{loops[[1]],loops[[1]]}},DeleteDuplicates[Flatten[Table[SP@@Sort[{a,b}],{a,loops},{b,loops}]]]];
 If[AnyTrue[sps,(!FreeQ[#,Alternatives@@loops])&&!MemberQ[allowed,#]&],Return[fail["VacuumTensor","An open loop scalar product remains; call OPITeR first."]]];
 expr=expr/.SP[a_,b_]/;a===loops[[1]]&&b===loops[[1]]:>qd[[1]];
 If[Length[loops]===2,
  expr=expr/.{SP[a_,b_]/;a===loops[[2]]&&b===loops[[2]]:>qd[[2]],
   SP[a_,b_]/;Sort[{a,b}]===Sort[loops]:>(qd[[3]]-qd[[1]]-qd[[2]])/2}];
 dens=DeleteDuplicates[Cases[expr,_VacuumDenominator,{0,Infinity}]];
 If[!AllTrue[dens,MemberQ[Range[If[Length[loops]===1,1,3]],#[[1]]]&&IntegerQ[#[[3]]]&&#[[3]]>0&],Return[fail["VacuumDenominator","Invalid route or nonpositive unnormalized propagator power."]]];
 scalars=DeleteDuplicates[Cases[expr,x_Symbol/;!MemberQ[Join[vectors,qd,{SP,VacuumDenominator,D}],x],Infinity]];
 rules=Join[MapIndexed[#1->Symbol["fmV"<>ToString[First[#2]]]&,vectors],MapIndexed[#1->Symbol["fmC"<>ToString[First[#2]]]&,scalars]];
 back=rules/.Rule[x_,y_]:>Rule[Symbol["FeAmMatch2`FORM`"<>SymbolName[y]],x];
 source="Off Statistics;\nSymbols fmR,fmA,fmB,fmN,fmM,fmX,fmY,fmZ,fmImag,D,"<>
  StringRiffle[formText/@qd,","]<>
  If[Length[scalars]>0,","<>StringRiffle[formText/@(scalars/.rules),","],""]<>";\n"<>
  If[Length[vectors]>0,"Vectors "<>StringRiffle[formText/@(vectors/.rules),","]<>";\n",""]<>
  "CF fmVD,F,fmFamily;\nL Result="<>formText[expr/.rules]<>";\n"<>
  "id fmVD(fmR?,fmM?,fmA?)^fmN?pos_=fmVD(fmR,fmM,fmA*fmN);\nrepeat;\n"<>
  "id fmVD(fmR?,fmM?,0)=1;\n"<>
  "id fmVD(fmR?,fmM?,fmA?)*fmVD(fmR?,fmM?,fmB?)=fmVD(fmR,fmM,fmA+fmB);\n"<>
  "endrepeat;\n.sort\nrepeat;\n"<>
  StringJoin[Table["id "<>formText[qd[[i]]]<>"*fmVD("<>ToString[i]<>",fmM?,fmA?pos_)=fmVD("<>
   ToString[i]<>",fmM,fmA-1)+fmM^2*fmVD("<>ToString[i]<>",fmM,fmA);\n",{i,Length[qd]}]]<>
  "id fmVD(fmR?,fmM?,0)=1;\nendrepeat;\n.sort\n"<>
  StringJoin[Table["id "<>formText[qd[[i]]]<>"^fmN?pos_=fmVD("<>ToString[i]<>",0,-fmN);\n",{i,3}]]<>
  If[Length[loops]===1,
   "id fmVD(1,fmM?,fmA?)=F(fmA,fmM);\nif (count(F,1)==0) discard;\n",
   "Multiply fmFamily(0,0,0,0,0,0);\nrepeat;\n"<>
   "id fmVD(1,fmM?,fmN?)*fmFamily(fmA?,fmB?,fmR?,fmX?,fmY?,fmZ?)=fmFamily(fmN,fmB,fmR,fmM,fmY,fmZ);\n"<>
   "id fmVD(2,fmM?,fmN?)*fmFamily(fmA?,fmB?,fmR?,fmX?,fmY?,fmZ?)=fmFamily(fmA,fmN,fmR,fmX,fmM,fmZ);\n"<>
   "id fmVD(3,fmM?,fmN?)*fmFamily(fmA?,fmB?,fmR?,fmX?,fmY?,fmZ?)=fmFamily(fmA,fmB,fmN,fmX,fmY,fmM);\n"<>
   "endrepeat;\nid fmFamily(fmA?,fmB?,fmR?,fmX?,fmY?,fmZ?)=F(fmA,fmB,fmR,fmX,fmY,fmZ);\n"]<>
  ".sort\nFormat Mathematica;\n#write <result.out> \"%E\",Result\n.end\n";
 job=RunFORMTask[source,config];If[FailureQ[job],Return[job]];
 result=readExpression[FileNameJoin[{job["directory"],"result.out"}]];If[FailureQ[result],Return[result]];
 result=result/.back/.SP[a_,b_]:>SP@@Sort[{a,b}];
 If[!FreeQ[result,VacuumDenominator|FeAmMatch2`FORM`fmFamily|Alternatives@@(Symbol["FeAmMatch2`FORM`"<>SymbolName[#]]&/@qd)],Return[fail["VacuumNormalization","A propagator or quadratic form was not normalized."]]];
 <|"Expression"->result,"Stage"->"vacuum_families_normalized","Job"->job,"PhysicalCoefficients"->Null|>
];
