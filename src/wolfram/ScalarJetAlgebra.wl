(* Local jet calculus. Jet atoms are independent coordinates during differentiation. *)
ClearAll[scalarJetAtom,scalarJetRestore,scalarActionTheory,scalarLoopSeries,scalarLocalRows,scalarWeightAction];
ScalarJet[f_,ind_List]/;!OrderedQ[ind]:=ScalarJet[f,Sort[ind]];
TotalScalarDerivative[expression_,mu_Symbol,fields_List]:=Catch[Module[{jets,atoms,rules,back,atomic,result},
 jets=DeleteDuplicates[Cases[expression,_ScalarJet,{0,Infinity}]];atoms=Table[Unique["jetAtom"],Length[jets]];
 rules=Thread[jets->atoms];back=Thread[atoms->jets];atomic=expression/.rules;
 result=Sum[D[atomic,f]ScalarJet[f,{mu}],{f,fields}]+Sum[D[atomic,atoms[[i]]]ScalarJet[jets[[i,1]],Append[jets[[i,2]],mu]],{i,Length[jets]}];
 Expand[result/.back]
],"FM2TheoryStage"];
SubstituteScalarFields[action_,rules_List,fields_List,dimension_:Infinity]:=Catch[Module[{terms,rows,replacements,base,labels},
 terms=If[Head[Expand[action]]===Plus,List@@Expand[action],{Expand[action]}];
 rows=scalarTermData[#,fields]&/@terms;If[AnyTrue[rows,FailureQ],Throw[SelectFirst[rows,FailureQ],"FM2TheoryStage"]];
 Expand[Total[Table[
  replacements=Table[base=jet[[1]]/.rules;
   (* Each original field occurrence is substituted independently. Contracted
      dummy labels inside its replacement must never be shared with a second
      copy of that replacement, e.g. H_classical^2. *)
   labels=DeleteDuplicates[Flatten[Cases[base,ScalarJet[_,ind_List]:>ind,{0,Infinity}]]];
   base=base/.Thread[labels->Table[Unique["localDummy"],Length[labels]]];
   If[IntegerQ[dimension],base=scalarJetDimensionTruncate[base,fields,dimension-Length[jet[[2]]]]];
   Fold[TotalScalarDerivative[#1,#2,fields]&,base,jet[[2]]],{jet,row["Jets"]}];
  If[IntegerQ[dimension],Fold[scalarJetDimensionTruncate[#1 #2,fields,dimension]&,row["Coefficient"],replacements],
   row["Coefficient"]Times@@replacements],{row,rows}]]]
],"FM2TheoryStage"];
(* Polynomial projection avoids Series differentiating very large local actions.
   The fallback retains the previous series semantics for rational/nonpolynomial
   scaling dependence. The auxiliary variable is evaluated at one only here. *)
scaledPolynomialCut[expression_,scale_Symbol,order_Integer]:=If[PolynomialQ[expression,scale],
 Total[Last/@Select[CoefficientRules[expression,{scale}],First[First[#]]<=order&]],
 Expand[Normal[Series[expression,{scale,0,order}]]/.scale->1]
];
scalarJetDimensionTruncate[action_,fields_List,dimension_Integer]:=Module[{rho=Unique["operatorDimension"]},
 scaledPolynomialCut[action/.Join[{ScalarJet[f_,ind_]:>rho^(Length[ind]+1)ScalarJet[f,ind]},Thread[fields->rho fields]],rho,dimension]
];
scalarActionTheory[theory_,action_]:=Catch[Module[{new=Join[theory,<|"Lagrangian"->Expand[action]|>],terms},
 terms=ExtractScalarTerms[new];If[FailureQ[terms],Throw[terms,"FM2TheoryStage"]];
 new=Join[new,<|"Lagrangian"->scalarCanonicalAction[terms]|>];terms=ExtractScalarTerms[new];Join[new,<|"Terms"->terms|>]
],"FM2TheoryStage"];
scalarCanonicalAction[terms_List]:=Expand[Total[Function[row,Module[{jets=row["Jets"],labels,canonical,possibilities},
 labels=DeleteDuplicates[Flatten[If[jets==={},{},jets[[All,2]]]]];
 canonical=Table[Symbol["FeAmMatch2`Lorentz`mu"<>ToString[i]],{i,Length[labels]}];
 possibilities=Table[Times@@(If[#[[2]]==={},#[[1]],ScalarJet[#[[1]],#[[2]]]]&/@(jets/.Thread[labels->permutation])),{permutation,Permutations[canonical]}];
 row["Coefficient"] First[Sort[possibilities]]]]/@terms]];
scalarLoopSeries[x_,h_,n_]:=Expand[Normal[Series[x,{h,0,n}]]];
scalarWeightAction[theory_,t_]:=Total[(#["Coefficient"] t^Max[0,#["OperatorDimension"]-4] Times@@(ScalarJet[#[[1]],#[[2]]]&/@#["Jets"]))&/@theory["Terms"]]/.ScalarJet[f_,{}]:>f;
scalarLocalRows[theory_,action_]:=Catch[Module[{local,channels,rows={},momenta,vertex,variables,parts},
 local=scalarActionTheory[theory,action];If[FailureQ[local],Throw[local,"FM2TheoryStage"]];
 channels=DeleteDuplicates[Sort/@Select[Lookup[local["Terms"],"Fields"],Length[#]>0&]];
 Do[momenta=independentExternalMomenta[Length[channel]];
  vertex=canonicalMomentumPolynomial[ScalarLocalVertex[local,channel,momenta],momenta];
  variables=DeleteDuplicates[Cases[vertex,_SP,{0,Infinity}]];
  parts=If[variables==={},{vertex},Last/@CoefficientRules[vertex,variables]];
  rows=Join[rows,DeleteCases[Expand/@parts,0]],{channel,channels}];rows
],"FM2TheoryStage"];

(* Classical heavy EOM, simultaneous iteration in field/derivative weight.
   Only the UV canonical free kernel is assumed; no phi4/phi6 channel is built in. *)
classicalHeavyAction[uv_Association,maxdim_Integer]:=Catch[Module[{fields,heavy,light,masses,interaction,potential,solutions,rho,truncate,next,mu,action},
 fields=uv["FieldSymbols"];
 heavy=Lookup[Select[uv["Fields"],#["Role"]==="heavy"&],"Symbol"];light=Complement[fields,heavy];
 masses=Association[Table[f["Symbol"]->f["Mass"],{f,uv["Fields"]}]];
 interaction=scalarActionTheory[uv,uv["Lagrangian"]-freeScalarAction[uv]];If[FailureQ[interaction],Throw[interaction,"FM2TheoryStage"]];
 If[AnyTrue[interaction["Terms"],#["DerivativeOrder"]>0&],Throw[fail["TreeUVCapability","This classical EOM backend requires canonical scalar kinetic terms and a polynomial nonderivative UV interaction."],"FM2TheoryStage"]];
 potential=-interaction["Lagrangian"];
 If[AnyTrue[heavy,TrueQ[masses[#]===0]&]||AnyTrue[interaction["Terms"],#["FieldDegree"]<3&&#["Coefficient"]=!=0&],Throw[fail["TreeVacuumCapability","Expand about a stationary tree vacuum with diagonal declared masses before using this backend."],"FM2TheoryStage"]];
 rho=Unique["localWeight"];mu=Unique["eomLorentz"];
 truncate[x_]:=scalarJetDimensionTruncate[x,light,maxdim];
 solutions=Thread[heavy->0];
 Do[mu=Unique["eomLorentz"];next=Table[With[{f=f},f->scalarActionTheory[<|"FieldSymbols"->light|>,truncate[-(SubstituteScalarFields[D[potential,f],solutions,fields,maxdim]+TotalScalarDerivative[TotalScalarDerivative[f/.solutions,mu,light],mu,light])/masses[f]^2]]["Lagrangian"]],{f,heavy}];
  If[next===solutions,Break[]];solutions=next,{maxdim+1}];
 action=truncate[SubstituteScalarFields[uv["Lagrangian"],solutions,fields,maxdim]];
 action=scalarActionTheory[<|"FieldSymbols"->light|>,action];If[FailureQ[action],Throw[action,"FM2TheoryStage"]];
 <|"HeavySolution"->solutions,"Lagrangian"->action["Lagrangian"],"Dimension"->maxdim|>
],"FM2TheoryStage"];
MatchTheoryTree[task_Association]:=Catch[Module[{eft=task["EFT"],eliminated,action,response,rhs,solve,eftAction,residual},
 eliminated=classicalHeavyAction[task["UV"],task["Request"]["MaxDimension"]];If[FailureQ[eliminated],Throw[eliminated,"FM2TheoryStage"]];
 action=eliminated["Lagrangian"]/.(Reverse/@task["LightFieldMap"]);
 response=BuildOperatorResponses[eft];If[FailureQ[response],Throw[response,"FM2TheoryStage"]];
 rhs=Table[With[{row=row},With[{v=canonicalMomentumPolynomial[ScalarLocalVertex[scalarActionTheory[eft,action],row["ExternalFields"],row["Momenta"]],row["Momenta"]]},
  If[row["Variables"]==={},v,Fold[Coefficient[#1,First[#2],Last[#2]]&,v,Transpose[{row["Variables"],row["Powers"]}]]]]],{row,response["Rows"]}];
 solve=SolveExactSystem[response["Matrix"],Expand[rhs-response["Known"]],response["Coefficients"]];If[FailureQ[solve],Throw[solve,"FM2TheoryStage"]];
 eftAction=eft["Lagrangian"]/.solve["Rules"];residual=scalarLocalRows[eft,Expand[action-eftAction]];If[FailureQ[residual],Throw[residual,"FM2TheoryStage"]];
 If[residual=!={},Throw[Failure["GreenCoverage",<|"MessageTemplate"->"The user Green action does not cover the tree hard action.","Residuals"->residual|>],"FM2TheoryStage"]];
 <|"Stage"->"tree_green_complete","Rules"->solve["Rules"],"Lagrangian"->eftAction,"HeavySolution"->eliminated["HeavySolution"],"RawEffectiveAction"->action,"Residuals"->residual,"ResponseSolve"->solve|>
],"FM2TheoryStage"];
