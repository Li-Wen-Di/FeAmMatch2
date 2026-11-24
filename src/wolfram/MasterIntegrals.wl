(* Analytic master coefficient tables. FORM performs rational epsilon expansion
   and products, after factor-wise precision checks. No finite matching oracle. *)
ClearAll[masterDefinition,masterSeries,epsilonTermText];
masterDefinition[vi_VacuumIntegral]:=Module[{a=List@@vi,m},
 If[Length[a]===2&&a[[1]]===1,Return[<|"MinimumOrder"->-1,"AvailableOrder"->Infinity,"Branch"->"tadpole"|>]];
 If[Length[a]=!=6||Take[a,3]=!={1,1,1},Return[fail["MasterIntegral","Reduce the integral before Laurent expansion."]]];
 m=Select[Drop[a,3],!TrueQ[PossibleZeroQ[#]]&];
 If[Length[m]===1,Return[<|"MinimumOrder"->-2,"AvailableOrder"->Infinity,"Branch"->"00M"|>]];
 If[Length[m]===3&&AllTrue[m,TrueQ[PossibleZeroQ[#-First[m]]]&],Return[<|"MinimumOrder"->-2,"AvailableOrder"->0,"Branch"->"MMM"|>]];
 fail["MasterMassBranch","Current evaluated tables cover 00M and MMM. Reduce 0MM to tadpoles; unsupported branches remain explicit."]
];

masterSeries[vi_VacuumIntegral,e_,mu_,order_Integer]:=Module[{a=List@@vi,definition,m,l,depth,logGamma,normalization},
 definition=masterDefinition[vi];If[FailureQ[definition],Return[definition]];
 m=First[Select[If[Length[a]===2,{a[[2]]},Drop[a,3]],!TrueQ[PossibleZeroQ[#]]&]];
 l=Log[m^2/mu^2];depth=Max[0,order-definition["MinimumOrder"]];
 Switch[definition["Branch"],
  "tadpole",
   logGamma=-e*l+Sum[(-1)^j Zeta[j]e^j/j,{j,2,depth}];
   normalization=m^2/(e*(1-e)),
  "00M",
   logGamma=-2e*l+Sum[((-2)^j+(-1)^j+1) Zeta[j]e^j/j,{j,2,depth}];
   normalization=m^2/(2e^2(1-e)(1-2e)),
  "MMM",
   logGamma=-2e*l+Sum[2*(-1)^j Zeta[j]e^j/j,{j,2,depth}];
   normalization=m^2*(3/e^2-6Sqrt[3]Clausen2[2Pi/3])/(2(1-e)(1-2e))];
 Normal[Series[normalization*Exp[logGamma],{e,0,order}]]
];

(* Rational denominators factor into regulator-only and constant parts.
   Constant residues, including logs and special constants, are opaque scalar
   coefficients to FORM. No rational epsilon expansion is done by this compiler. *)
epsilonTermText[coefficient_,e_,rules_]:=Catch[Module[{parts,strings={},factors,free,dependent,numerator,poly},
 parts=If[Head[Apart[coefficient,e]]===Plus,List@@Apart[coefficient,e],{Apart[coefficient,e]}];
 Do[
  factors=If[Head[Factor[Denominator[Together[part]]]]===Times,List@@Factor[Denominator[Together[part]]],{Factor[Denominator[Together[part]]]}];
  free=Times@@Select[factors,FreeQ[#,e]&];dependent=Times@@Select[factors,!FreeQ[#,e]&];
  If[!AllTrue[Cases[dependent,_Symbol,Infinity],#===e&],Throw[fail["RegulatorDenominator","Regulator-dependent denominators must factor from physical parameters."],"EpsilonCompilerFailure"]];
  numerator=Expand[part*free*dependent];
  If[!PolynomialQ[numerator,e],Throw[fail["RegulatorNumerator","Cannot compile regulator coefficient."],"EpsilonCompilerFailure"]];
  poly=CoefficientRules[numerator,{e}];
  Do[AppendTo[strings,formText[(Last[row]/free)/.rules]<>"*fmRat("<>formText[e^First[First[row]]/.rules]<>","<>formText[dependent/.rules]<>")"],{row,poly}],
  {part,parts}];
 If[strings==={},"0","("<>StringRiffle[strings,"+"]<>")"]
],"EpsilonCompilerFailure"];

LaurentVacuum[expression_,e_Symbol,mu_,order_Integer,config_Association]:=Module[
 {expr=expression,integrals,definitions,budget,slots,poly,rows,table,constantAtoms,scalarSymbols,
  constants,rules,back,strings={},coefficient,source,job,result,component,branches,minimumOverall,expansionDepth},
 expr=expr/.{VacuumIntegral[_,0]->0,VacuumIntegral[1,1,1,0,0,0]->0};
 expr=expr/.D->4-2e;
 integrals=DeleteDuplicates[Cases[expr,_VacuumIntegral,{0,Infinity}]];
 definitions=Association[Table[vi->masterDefinition[vi],{vi,integrals}]];
 If[AnyTrue[Values[definitions],FailureQ],Return[SelectFirst[Values[definitions],FailureQ]]];
 budget=PlanLaurentPrecision[expr,definitions,e,order];If[FailureQ[budget],Return[budget]];
 If[budget["Status"]==="precision_insufficient",Return[Failure["MasterPrecision",<|"MessageTemplate"->"Master coefficients are unavailable at the automatically required order.","Budget"->budget|>]]];
 table=Association[Table[vi->masterSeries[vi,e,mu,budget["Requests"][ToString[vi,InputForm]]],{vi,integrals}]];
 If[AnyTrue[Values[table],FailureQ],Return[SelectFirst[Values[table],FailureQ]]];
 slots=Table[Unique["masterSlot"],Length[integrals]];
 poly=Expand[expr/.Thread[integrals->slots]];
 rows=If[slots==={},{{}->poly},CoefficientRules[poly,slots]];
 minimumOverall=Min[0,Sequence@@Table[laurentValuation[Last[row],e]+If[slots==={},0,Total[First[row]*(Lookup[#,"MinimumOrder"]&/@Values[definitions])]],{row,rows}]];
 expansionDepth=Max[0,order-minimumOverall+1];
 (* Aliases include exact roots, scalar products, logs and Clausen constants. *)
 constantAtoms=DeleteDuplicates[Cases[{expr,Values[table]},x:(Log[_]|Zeta[_]|Clausen2[_]|SP[_,_]|Power[_,Rational[_,_]])/;FreeQ[x,e],Infinity]];
 scalarSymbols=DeleteDuplicates[Cases[{expr,Values[table]},x_Symbol/;!MemberQ[Join[{e,VacuumIntegral,SP,Clausen2},slots],x],Infinity]];
 constants=Join[constantAtoms,Complement[scalarSymbols,Cases[constantAtoms,_Symbol,Infinity]]];
 (* Keeping symbol aliases as well as function aliases allows outer algebraic factors. *)
 constants=DeleteDuplicates[Join[constantAtoms,scalarSymbols]];
 rules=Join[MapIndexed[#1->Symbol["fmConstant"<>ToString[First[#2]]]&,constants],{e->Symbol["fmEpsilon"]}];
 back=rules/.Rule[x_,y_]:>Rule[Symbol["FeAmMatch2`FORM`"<>SymbolName[y]],x];
 Do[
  component=If[slots==={},Last[row],Last[row]*Times@@MapThread[If[#2===0,1,#1^#2]&,{Values[table],First[row]}]];
  (* Master tables are known Laurent polynomials. Apart separates their finite
     factors from exact rational reduction coefficients; FORM executes expansion. *)
  coefficient=epsilonTermText[component,e,rules];If[FailureQ[coefficient],Return[coefficient]];
  AppendTo[strings,coefficient],{row,rows}];
 source="Off Statistics;\nSymbols fmEpsilon,fmImag"<>
   If[constants=!={},","<>StringRiffle[formText/@(constants/.rules),","],""]<>";\nCF fmRat;\nPolyRatFun fmRat;\nL Result="<>
   If[strings==={},"0",StringRiffle[strings,"+"]]<>";\n.sort\nPolyRatFun fmRat(expand,fmEpsilon,"<>ToString[expansionDepth]<> ");\n.sort\nPolyRatFun;\nid fmRat(x?)=x;\nif (count(fmEpsilon,1)>"<>ToString[order]<>") discard;\n.sort\nFormat Mathematica;\n#write <result.out> \"%E\",Result\n.end\n";
 (* x is only a wildcard in the final one-argument polynomial wrapper. *)
 source=StringReplace[source,"Symbols fmEpsilon,fmImag"->"Symbols fmEpsilon,fmImag,x"];
 job=RunFORMTask[source,config];If[FailureQ[job],Return[job]];
 result=readExpression[FileNameJoin[{job["directory"],"result.out"}]];If[FailureQ[result],Return[result]];
 result=result/.back;
 <|"Expression"->result,"Stage"->"MSbar_laurent_evaluated","Order"->order,"Precision"->budget,
  "FORMExpansionDepth"->expansionDepth,"Job"->job,"PhysicalCoefficients"->Null|>
];
