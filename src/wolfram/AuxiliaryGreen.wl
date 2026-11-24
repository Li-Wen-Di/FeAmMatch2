(* Precision closure generated from the local action, not a phi8 coefficient table. *)
BuildAuxiliaryGreenBasis[task_Association,dimension_Integer]:=Catch[Module[
 {eft=task["EFT"],eliminated,action,terms,operators,augmented,parameters,coefficients,lag,candidate,coefficient,trial,response,rank,added={},newTask,tree,identity,i=0},
 If[dimension<task["Request"]["MaxDimension"],Throw[fail["AuxiliaryDimension","Auxiliary precision cannot be shallower than the requested action."],"FM2TheoryStage"]];
 eliminated=classicalHeavyAction[task["UV"],dimension];If[FailureQ[eliminated],Throw[eliminated,"FM2TheoryStage"]];
 action=eliminated["Lagrangian"]/.(Reverse/@task["LightFieldMap"]);terms=ExtractScalarTerms[Join[eft,<|"Lagrangian"->action|>]];If[FailureQ[terms],Throw[terms,"FM2TheoryStage"]];
 operators=DeleteDuplicates[(Times@@(If[#[[2]]==={},#[[1]],ScalarJet[#[[1]],#[[2]]]]&/@#["Jets"]))&/@Select[terms,#["OperatorDimension"]>task["Request"]["MaxDimension"]&]];
 parameters=eft["Parameters"];coefficients=eft["WilsonCoefficients"];lag=eft["Lagrangian"];
 rank=BuildOperatorResponses[eft]["Rank"];identity=StringTake[theoryIdentity[{task["Identity"],dimension}],12];
 Do[i++;coefficient=Symbol["FeAmMatch2`Auxiliary"<>identity<>"`A"<>ToString[i]];
  candidate=Join[parameters,<|coefficient-><|"Dimension"->4-First[ExtractScalarTerms[Join[eft,<|"Lagrangian"->operator|>]]]["OperatorDimension"],"Value"->1/101|>|>];
  trial=LoadEFTModel[Join[eft,<|"Parameters"->candidate,"WilsonCoefficients"->Append[coefficients,coefficient],"Lagrangian"->lag+coefficient operator|>]];If[FailureQ[trial],Throw[trial,"FM2TheoryStage"]];
  response=BuildOperatorResponses[trial];If[FailureQ[response],Throw[response,"FM2TheoryStage"]];
  If[response["Rank"]>rank,rank=response["Rank"];parameters=candidate;coefficients=Append[coefficients,coefficient];lag+=coefficient operator;AppendTo[added,<|"Coefficient"->coefficient,"Operator"->operator|>]],{operator,operators}];
 augmented=LoadEFTModel[Join[eft,<|"Parameters"->parameters,"WilsonCoefficients"->coefficients,"Lagrangian"->lag|>]];If[FailureQ[augmented],Throw[augmented,"FM2TheoryStage"]];
 (* This is an internal precision task, not a request for physical output at
    dimension eight. Keep the public dimension-six capability check strict. *)
 newTask=Join[task,<|"EFT"->augmented,"Request"->Join[task["Request"],<|"MaxDimension"->dimension|>],
   "Identity"->theoryIdentity[{task["Identity"],augmented["Identity"],dimension,"auxiliary-precision"}]|>];
 tree=MatchTheoryTree[newTask];If[FailureQ[tree],Throw[tree,"FM2TheoryStage"]];
 <|"Stage"->"tree_auxiliary_basis_complete","Task"->newTask,"Theory"->augmented,"Tree"->tree,"AddedOperators"->added,"UserCoefficients"->eft["WilsonCoefficients"],"Dimension"->dimension,
  "Scope"->"Independent tree-induced auxiliary operators modulo momentum conservation/IBP. This is not a general loop-generated higher-dimensional Green-basis completeness certificate."|>
],"FM2TheoryStage"];
