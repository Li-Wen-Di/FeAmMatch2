(* Exact local vertices and data-driven off-shell coefficient responses. *)
ClearAll[localMonomialVertex,independentExternalMomenta,canonicalMomentumPolynomial,scalarResponseRows];
localMonomialVertex[row_Association,external_List,momenta_List]:=Catch[Module[{jets=row["Jets"],different,types,positions,assignments,choices,weight,labels,pairs,term,result=0,map},
 If[Sort[row["Fields"]]=!=Sort[external],Throw[0,"FM2TheoryStage"]];
 different=Select[Range[Length[jets]],jets[[#,2]]=!={}&];
 types=DeleteDuplicates[external];
 weight=Times@@Table[Factorial[Count[external,f]-Count[jets[[different,1]],f]],{f,types}];
 If[different==={},Throw[row["Coefficient"]*Times@@(Factorial/@Values[Counts[external]]),"FM2TheoryStage"]];
 choices=Table[With[{ds=Select[different,jets[[#,1]]===f&],ps=Flatten[Position[external,f]]},
  Thread[ds->#]&/@Permutations[ps,{Length[ds]}]],{f,types}];
 assignments=Flatten[#,1]&/@Tuples[choices];
 labels=DeleteDuplicates[Flatten[jets[[All,2]]]];
 Do[
  map=Association[assignment];
  term=Times@@Table[pairs=Flatten[Table[ConstantArray[slot,Count[jets[[slot,2]],label]],{slot,different}]];
    If[Length[pairs]=!=2,Throw[fail["ScalarContractions","Vertex contraction is not a Lorentz scalar."],"FM2TheoryStage"]];
    SP[momenta[[map[pairs[[1]]]]],momenta[[map[pairs[[2]]]]]],{label,labels}];
  result+=term,{assignment,assignments}];
 Expand[(-I)^row["DerivativeOrder"]*weight*row["Coefficient"]*result]
],"FM2TheoryStage"];
ScalarLocalVertex[theory_Association,external_List,momenta_List]:=Catch[Module[{terms,result},
 If[Length[external]=!=Length[momenta]||!AllTrue[momenta,Head[#]===Symbol&],Throw[fail["VertexMomenta","Supply one distinct symbolic momentum for each external scalar."],"FM2TheoryStage"]];
 terms=Lookup[theory,"Terms",ExtractScalarTerms[theory]];If[FailureQ[terms],Throw[terms,"FM2TheoryStage"]];
 result=localMonomialVertex[#,external,momenta]&/@Select[terms,#["FieldDegree"]>0&];
 If[AnyTrue[result,FailureQ],Throw[SelectFirst[result,FailureQ],"FM2TheoryStage"]];Expand[Total[result]]
],"FM2TheoryStage"];
independentExternalMomenta[n_Integer]:=Table[Symbol["FeAmMatch2`Kinematics`p"<>ToString[i]],{i,n}];
canonicalMomentumPolynomial[expr_,momenta_List]:=Catch[Module[{replacement,result},
 If[momenta==={},Throw[expr,"FM2TheoryStage"]];
 replacement=Last[momenta]->-Total[Most[momenta]];
 result=expr/.replacement;
 expandScalarProducts[result,Most[momenta]]//Expand
],"FM2TheoryStage"];

BuildOperatorResponses[eft_Association]:=Catch[Module[{terms,coefficients,channels,rows={},matrix={},known={},momenta,vertex,variables,parts,powers,value,response,names,rank},
 If[Lookup[eft,"Kind",""]=!="EFT",Throw[fail["EFTObject","Load an EFT Green action before constructing responses."],"FM2TheoryStage"]];
 terms=eft["Terms"];coefficients=eft["WilsonCoefficients"];
 channels=DeleteDuplicates[Sort[#]&/@Select[Lookup[terms,"Fields"],Length[#]>0&]];
 Do[
  momenta=independentExternalMomenta[Length[channel]];
  vertex=ScalarLocalVertex[eft,channel,momenta];If[FailureQ[vertex],Throw[vertex,"FM2TheoryStage"]];
  vertex=canonicalMomentumPolynomial[vertex,momenta];
  variables=DeleteDuplicates[Cases[vertex,_SP,{0,Infinity}]];
  parts=If[variables==={},{{}->vertex},CoefficientRules[vertex,variables]];
  Do[powers=First[part];value=Last[part];response=D[value,#]&/@coefficients;
   AppendTo[matrix,response];AppendTo[known,Expand[value-response.coefficients]];
   AppendTo[rows,<|"ExternalFields"->channel,"Momenta"->momenta,"Variables"->variables,"Powers"->powers,"MomentumDegree"->2Total[powers]|>],{part,parts}],{channel,channels}];
 rank=If[matrix==={},0,MatrixRank[matrix]];
 <|"Schema"->"feammatch2.operator-response.v1","EFTIdentity"->eft["Identity"],"Coefficients"->coefficients,"Rows"->rows,
  "Matrix"->matrix,"Known"->known,"Rank"->rank,"NullSpace"->If[matrix==={},IdentityMatrix[Length[coefficients]],NullSpace[matrix]],
  "Channels"->channels,"Identity"->theoryIdentity[{eft["Identity"],rows,matrix,known}]|>
],"FM2TheoryStage"];
CheckLinearDependence[eft_Association]:=Catch[Module[{response=BuildOperatorResponses[eft]},
 If[FailureQ[response],Throw[response,"FM2TheoryStage"]];
 <|"Independent"->(response["Rank"]===Length[response["Coefficients"]]),"Rank"->response["Rank"],
  "Coefficients"->response["Coefficients"],"NullSpace"->response["NullSpace"],"Response"->response|>
],"FM2TheoryStage"];

SolveExactSystem[matrix_List,rhs_List,variables_List]:=Catch[Module[{rank,augmented,indices={},selected={},solution,residuals,trial},
 If[Length[matrix]=!=Length[rhs]||!AllTrue[matrix,Length[#]===Length[variables]&],Throw[fail["ResponseShape","Matrix, right-hand side and coefficient counts differ."],"FM2TheoryStage"]];
 If[matrix==={},Throw[fail["ResponseRank","No response equations were provided."],"FM2TheoryStage"]];
 rank=MatrixRank[matrix];
 If[rank<Length[variables],Throw[Failure["ResponseRank",<|"MessageTemplate"->"The supplied off-shell responses do not determine every coefficient.","Rank"->rank,"NullSpace"->NullSpace[matrix],"Variables"->variables|>],"FM2TheoryStage"]];
 Do[trial=Append[selected,matrix[[i]]];If[MatrixRank[trial]>Length[selected],selected=trial;AppendTo[indices,i]],{i,Length[matrix]}];
 solution=Quiet[Check[LinearSolve[selected,rhs[[indices]]],$Failed]];If[solution===$Failed,Throw[fail["ResponseSolve","Exact response system could not be solved."],"FM2TheoryStage"]];
 residuals=Factor/@Expand[matrix.solution-rhs];
 If[AnyTrue[residuals,#=!=0&],Throw[Failure["ResponseResidual",<|"MessageTemplate"->"Unused response equations have nonzero residuals.","Residuals"->residuals,"SelectedRows"->indices,"Solution"->Thread[variables->solution]|>],"FM2TheoryStage"]];
 <|"Rules"->Thread[variables->solution],"Values"->solution,"Rank"->rank,"SelectedRows"->indices,"Residuals"->residuals|>
],"FM2TheoryStage"];
