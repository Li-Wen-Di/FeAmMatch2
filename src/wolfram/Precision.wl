(* Backwards Laurent budget propagation for sums of integral products. *)
ClearAll[laurentValuation];
laurentValuation[x_,e_]:=Module[{n,d},
 If[TrueQ[x===0],Return[Infinity]];
 {n,d}=NumeratorDenominator[Together[x]];
 If[!PolynomialQ[n,e]||!PolynomialQ[d,e],Return[fail["PrecisionCoefficient","Laurent coefficient bounds require a rational function of epsilon."]]];
 Min[First/@CoefficientRules[n,{e}]]-Min[First/@CoefficientRules[d,{e}]]
];

PlanLaurentPrecision[expression_,definitions_Association,e_Symbol,target_Integer:0]:=Catch[Module[
 {integrals=Keys[definitions],slots,polynomial,terms,requests=<||>,evidence={},powers,coefficient,
  valuation,minimums,required,available,issues={},entry},
 If[!AllTrue[Values[definitions],AssociationQ[#]&&IntegerQ[Lookup[#,"MinimumOrder",Missing[]]]&],Return[fail["PrecisionDefinitions","Every integral needs an integer MinimumOrder."]]];
 slots=Table[Unique["precisionIntegral"],Length[integrals]];
 polynomial=Expand[expression/.Thread[integrals->slots]];
 If[slots=!={}&&!PolynomialQ[polynomial,slots],Return[fail["PrecisionPolynomial","Integral dependence must be polynomial."]]];
 minimums=Lookup[#,"MinimumOrder"]&/@Values[definitions];
 terms=If[slots==={},{{}->polynomial},CoefficientRules[polynomial,slots]];
 Do[
  powers=First[term];coefficient=Last[term];
  valuation=laurentValuation[coefficient,e];If[FailureQ[valuation],Throw[valuation,"LaurentPrecisionStage"]];
  Do[If[powers[[i]]>0,
   required=target-valuation-Total[powers*minimums]+minimums[[i]];
   entry=<|"Integral"->ToString[integrals[[i]],InputForm],"RequiredOrder"->required,
     "CoefficientMinimumOrder"->valuation,"OtherFactorMinimumOrder"->Total[powers*minimums]-minimums[[i]],"TargetOrder"->target|>;
   AppendTo[evidence,entry];
   AssociateTo[requests,ToString[integrals[[i]],InputForm]->Max[required,Lookup[requests,ToString[integrals[[i]],InputForm],-Infinity]]]],
   {i,Length[integrals]}],{term,terms}];
 Do[
  required=Lookup[requests,ToString[integrals[[i]],InputForm],Missing["Unused"]];
  available=Lookup[definitions[integrals[[i]]],"AvailableOrder",Infinity];
  If[!MissingQ[required]&&available<required,AppendTo[issues,<|"Integral"->ToString[integrals[[i]],InputForm],"RequiredOrder"->required,"AvailableOrder"->available|>]],
  {i,Length[integrals]}];
 <|"Schema"->"feammatch2.laurent-budget.v1","Status"->If[issues==={},"precision_sufficient","precision_insufficient"],
   "Requests"->requests,"Derivation"->evidence,"Gaps"->issues|>
],"LaurentPrecisionStage"];
