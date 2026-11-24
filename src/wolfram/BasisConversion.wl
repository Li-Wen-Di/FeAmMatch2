(* Full-action scalar field transformations. All response equations are derived
   from user actions; there are no process-specific coefficient identities. *)
ClearAll[scalarKineticMatrix,scalarTransformationCandidates,solveScalarTransformation];
scalarKineticMatrix[theory_,action_]:=Catch[Module[{fields=theory["FieldSymbols"],p=Unique["kineticMomentum"],q=Unique["kineticMomentum"],local},
 local=scalarActionTheory[theory,action];If[FailureQ[local],Throw[local,"FM2TheoryStage"]];
 Table[Coefficient[expandScalarProducts[ScalarLocalVertex[local,{a,b},{p,q}]/.q->-p,{p}],SP[p,p]],{a,fields},{b,fields}]
],"FM2TheoryStage"];
CanonicalizeFields[theory_Association,action_,h_Symbol,loops_Integer:2]:=Catch[Module[{k,k0,s0,a,b,s,fields=theory["FieldSymbols"],rules,transformed,residual},
 k=scalarKineticMatrix[theory,action];If[FailureQ[k],Throw[k,"FM2TheoryStage"]];
 k=Map[scalarLoopSeries[#,h,loops]&,k,{2}];k0=k/.h->0;
 If[!DiagonalMatrixQ[k0]||AnyTrue[Diagonal[k0],#===0&],Throw[fail["TreeKineticMatrix","This symbolic normalization backend requires a nonzero diagonal tree kinetic matrix. Rotate the tree fields in the input first."],"FM2TheoryStage"]];
 s0=DiagonalMatrix[1/Sqrt[#]&/@Diagonal[k0]];
 a=Map[Coefficient[#,h,1]&,Simplify[Transpose[s0].k.s0],{2}];
 b=Map[Coefficient[#,h,2]&,Simplify[Transpose[s0].k.s0],{2}];
 s=s0.(IdentityMatrix[Length[fields]]-h a/2+h^2(3a.a/8-b/2));s=Map[scalarLoopSeries[#,h,loops]&,s,{2}];
 rules=Thread[fields->s.fields];transformed=scalarLoopSeries[SubstituteScalarFields[action,rules,fields],h,loops];
 residual=Map[Simplify[scalarLoopSeries[#,h,loops]]&,Transpose[s].k.s-IdentityMatrix[Length[fields]],{2}];
 If[AnyTrue[Flatten[residual],#=!=0&],Throw[Failure["KineticResidual",<|"MessageTemplate"->"Kinetic matrix normalization failed.","Residuals"->residual|>],"FM2TheoryStage"]];
 <|"Lagrangian"->transformed,"FieldRules"->rules,"Matrix"->s,"OriginalKineticMatrix"->k,"Residuals"->residual|>
],"FM2TheoryStage"];

scalarTransformationCandidates[fields_List,weight_Integer]:=Catch[Module[{monomials,rows={},mu=Unique["fieldMapLorentz"],variable},
 monomials=Flatten[Table[Times@@#&/@Tuples[fields,{degree}],{degree,2,1+weight}]]//DeleteDuplicates;
 Do[AppendTo[rows,<|"Field"->fields[[i]],"Expression"->monomial,"Variable"->Unique["nonlinearFieldMap"]|>],{i,Length[fields]},{monomial,monomials}];
 (* A symmetric linear map fixes the otherwise arbitrary field rotations. *)
 Do[variable=Unique["symmetricFieldMap"];AppendTo[rows,<|"Field"->fields[[i]],"Expression"->fields[[j]],"Variable"->variable|>];
  If[i=!=j,AppendTo[rows,<|"Field"->fields[[j]],"Expression"->fields[[i]],"Variable"->variable|>]],{i,Length[fields]},{j,i,Length[fields]}];
 If[weight>=2,Do[AppendTo[rows,<|"Field"->a,"Expression"->ScalarJet[b,{mu,mu}],"Variable"->Unique["derivativeFieldMap"]|>],{a,fields},{b,fields}]];
 rows
],"FM2TheoryStage"];
DeriveBasisConversion[green_Association,physical_Association,request_Association:<||>]:=Catch[Module[{dimension=Lookup[request,"MaxDimension",6],fields=green["FieldSymbols"],mapping,candidates},
 If[dimension>6||dimension<4,Throw[fail["ConversionDimension","The scalar local-field-redefinition backend currently supports dimensions four through six."],"FM2TheoryStage"]];
 mapping=Lookup[request,"PhysicalFieldMap",Thread[physical["FieldSymbols"]->fields]];
 If[Length[physical["FieldSymbols"]]=!=Length[fields]||Sort[Last/@mapping]=!=Sort[fields],Throw[fail["PhysicalFieldMap","Supply a bijective target-to-Green field map."],"FM2TheoryStage"]];
 If[!TrueQ[CheckLinearDependence[physical]["Independent"]],Throw[fail["PhysicalBasisDependent","The target local physical action contains dependent coefficient directions."],"FM2TheoryStage"]];
 candidates=Association[Table[w->scalarTransformationCandidates[fields,w],{w,1,dimension-4}]];
 <|"Schema"->"feammatch2.basis-conversion.v1","Green"->green,"Physical"->physical,"PhysicalFieldMap"->mapping,"Candidates"->candidates,
  "Request"->Join[<|"MaxDimension"->dimension,"Loops"->2,"LoopSymbol"->modelSymbol["h"]|>,request],
  "Identity"->theoryIdentity[{green["Identity"],physical["Identity"],request}],
  "Convention"->"Symmetric linear field maps; free transformation directions are set to zero only if every target coefficient is unique."|>
],"FM2TheoryStage"];

solveScalarTransformation[equations_List,physical_List,fieldVariables_List]:=Catch[Module[{variables=Join[physical,fieldVariables],sol,unfixed,physicalValues,fieldValues,residuals},
 sol=Quiet[Solve[Thread[equations==0],variables]];
 If[sol==={}||!ListQ[sol],Throw[Failure["BasisCoverage",<|"MessageTemplate"->"The target action and allowed local field redefinitions do not span the Green action.","Equations"->equations|>],"FM2TheoryStage"]];
 sol=First[sol];unfixed=Select[variables,FreeQ[First/@sol,#]&];physicalValues=Simplify[physical/.sol];
 If[!FreeQ[physicalValues,Alternatives@@unfixed],Throw[Failure["BasisConvention",<|"MessageTemplate"->"The field convention leaves physical coefficients undetermined.","Unfixed"->unfixed,"PhysicalValues"->physicalValues|>],"FM2TheoryStage"]];
 sol=Join[sol,Thread[unfixed->0]];residuals=Simplify[equations/.sol];
 If[AnyTrue[residuals,#=!=0&],Throw[fail["ConversionResidual","Full local-action response equations did not close."],"FM2TheoryStage"]];
 <|"Rules"->sol,"UnfixedFieldGauge"->unfixed,"Residuals"->residuals|>
],"FM2TheoryStage"];

applyBasisConversionExact[conversion_Association,rules_List]:=Catch[Module[{green=conversion["Green"],physical=conversion["Physical"],request=conversion["Request"],fields,t,h,loops,maxweight,weighted,leading,normal,action,target,coeffs,known,physicalValues,newVariables,active,correction,candidates,fieldVars,delta,transformed,equations,sol,fieldMaps={},reports={},desired,residual},
 fields=green["FieldSymbols"];t=Unique["eftWeight"];h=request["LoopSymbol"];loops=request["Loops"];maxweight=request["MaxDimension"]-4;
 coeffs=physical["WilsonCoefficients"];target=physical["Lagrangian"]/.conversion["PhysicalFieldMap"];
 weighted=scalarWeightAction[green,t]/.rules;leading=Coefficient[Expand[weighted],t,0];
 normal=CanonicalizeFields[green,leading,h,loops];If[FailureQ[normal],Throw[normal,"FM2TheoryStage"]];
 action=scalarLoopSeries[SubstituteScalarFields[weighted,normal["FieldRules"],fields],h,loops];
 physicalValues=ConstantArray[0,Length[coeffs]];
 known=target/.Thread[coeffs->0];
 Do[
  active=Select[Range[Length[coeffs]],Max[0,-physical["Parameters"][coeffs[[#]]]["Dimension"]]<=w&];
  newVariables=Table[Unique["physicalWeightCoefficient"],Length[active]];
  correction=Total[MapThread[#1 D[target,coeffs[[#2]]]&,{newVariables,active}]];
  candidates=If[w===0,{},conversion["Candidates"][w]];fieldVars=DeleteDuplicates[Lookup[candidates,"Variable",{}]];
  delta=Table[f->(f+t^w Total[(#["Variable"]#["Expression"]&/@Select[candidates,#["Field"]===f&])]),{f,fields}];
  transformed=If[w===0,action,Expand[Normal[Series[SubstituteScalarFields[action,delta,fields],{t,0,maxweight}]]]];
  transformed=scalarActionTheory[green,scalarLoopSeries[transformed,h,loops]];
  If[FailureQ[transformed],Throw[transformed,"FM2TheoryStage"]];transformed=transformed["Lagrangian"];
  desired=known+Total[MapThread[#1 D[target,#2]&,{physicalValues,coeffs}]];
  equations=scalarLocalRows[green,Expand[Coefficient[transformed-desired,t,w]-correction]];
  If[FailureQ[equations],Throw[equations,"FM2TheoryStage"]];
  sol=solveScalarTransformation[equations,newVariables,fieldVars];If[FailureQ[sol],Throw[sol,"FM2TheoryStage"]];
  Do[physicalValues[[active[[j]]]]+=t^w(newVariables[[j]]/.sol["Rules"]),{j,Length[active]}];
  action=Expand[transformed/.sol["Rules"]];If[w>0,AppendTo[fieldMaps,delta/.sol["Rules"]]];AppendTo[reports,<|"Weight"->w,"Solve"->sol|>],{w,0,maxweight}];
 desired=known+Total[MapThread[#1 D[target,#2]&,{physicalValues,coeffs}]];
 residual=scalarLocalRows[green,Expand[Normal[Series[action-desired,{t,0,maxweight}]]]];If[FailureQ[residual],Throw[residual,"FM2TheoryStage"]];
 If[residual=!={},Throw[Failure["PhysicalActionResidual",<|"MessageTemplate"->"The transformed full action differs from the physical target.","Residuals"->residual|>],"FM2TheoryStage"]];
 physicalValues=scalarLoopSeries[#,h,loops]&/@(physicalValues/.t->1);
 <|"Stage"->"physical_complete","Rules"->Thread[coeffs->physicalValues],"Lagrangian"->(physical["Lagrangian"]/.Thread[coeffs->physicalValues]),
  "KineticNormalization"->normal,"WeightedFieldMaps"->fieldMaps,"WeightSymbol"->t,"Solves"->reports,"Residuals"->residual,"BasisIdentity"->physical["Identity"]|>
],"FM2TheoryStage"];

(* Prove the map with independent short coefficient symbols before substituting
   long loop expressions. This preserves exact low-loop products while keeping
   the local-action linear solve independent of expression size. *)
ApplyBasisConversion[conversion_Association,rules_List]:=Catch[Module[
 {green=conversion["Green"],physical=conversion["Physical"],h=conversion["Request"]["LoopSymbol"],loops=conversion["Request"]["Loops"],coefficients,values,kinetic,kineticCoefficients,formal={},back={},value,part,symbol,compiled,resultRules,normal,fieldMaps,identity},
 coefficients=green["WilsonCoefficients"];
 If[Sort[First/@rules]=!=Sort[coefficients]||Length[rules]=!=Length[coefficients],Throw[fail["ConversionInputs","Supply exactly one matching value for every input Green coefficient."],"FM2TheoryStage"]];
 values=coefficients/.rules;
 If[!FreeQ[values,_SP|_ScalarJet]||AnyTrue[green["FieldSymbols"],!FreeQ[values,#]&],Throw[fail["NonlocalCoefficient","Matching coefficient values must not contain external kinematics or scalar fields."],"FM2TheoryStage"]];
 kinetic=scalarKineticMatrix[green,green["Lagrangian"]];If[FailureQ[kinetic],Throw[kinetic,"FM2TheoryStage"]];
 kineticCoefficients=Select[coefficients,!FreeQ[kinetic,#]&];identity=StringTake[conversion["Identity"],12];
 Do[value=0;
  Do[part=Coefficient[scalarLoopSeries[values[[i]],h,loops],h,n];
   If[TrueQ[part===0]||(n===0&&MemberQ[kineticCoefficients,coefficients[[i]]]),value+=h^n part,
    symbol=Symbol["FeAmMatch2`Conversion"<>identity<>"`g"<>ToString[i]<>"L"<>ToString[n]];AppendTo[back,symbol->part];value+=h^n symbol],{n,0,loops}];
  AppendTo[formal,coefficients[[i]]->value],{i,Length[coefficients]}];
 compiled=applyBasisConversionExact[conversion,formal];If[FailureQ[compiled],Throw[compiled,"FM2TheoryStage"]];
 resultRules=(First[#]->scalarLoopSeries[Last[#]/.back,h,loops])&/@compiled["Rules"];
 normal=compiled["KineticNormalization"]/.back;fieldMaps=compiled["WeightedFieldMaps"]/.back;
 Join[compiled,<|"Rules"->resultRules,"Lagrangian"->(physical["Lagrangian"]/.resultRules),"KineticNormalization"->normal,"WeightedFieldMaps"->fieldMaps,
  "FormalCoefficientMap"->compiled["Rules"],"CoefficientSubstitutions"->back,"FormalGreenInputs"->formal,
  "MapDerivation"->"Full-action field transformations solved with independent loop coefficients, followed by exact substitution and loop truncation. Tree kinetic constraints are kept exact."|>]
],"FM2TheoryStage"];
