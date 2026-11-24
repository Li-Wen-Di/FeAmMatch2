(* Independent scalar actions. There is no process/model-name dispatch here. *)
ClearAll[theoryInput,readTheoryFile,readFeynRulesTheory,fieldName,theoryIdentity,normalizeScalarAction,scalarTermData,inferParameterDimensions];
fieldName[s_Symbol]:=SymbolName[Unevaluated[s]];
theoryIdentity[x_]:=IntegerString[Hash[ToString[x,InputForm],"SHA256"],16,64];
validateComponentMetadata[data_,terms_]:=Catch[Module[{fields=data["Fields"],symmetries,charges,sign,symbols},
 If[Lookup[data,"IndexSpaces",{}]=!={}||AnyTrue[fields,Function[field,
   Lookup[field,"Multiplicity",1]=!=1||AnyTrue[{"Indices","IndexSpaces","Representations","FlavorIndices"},Function[key,Lookup[field,key,{}]=!={}]]]],
  Throw[fail["ComponentCapability","Expand internal multiplets into real scalar components. Open indices, representation tensors and implicit multiplicities are not executable in this version."],"FM2TheoryStage"]];
 symmetries=Lookup[data,"Symmetries",{}];
 If[!ListQ[symmetries],Throw[fail["SymmetryMetadata","Symmetries must be a list of explicit Z2 charge declarations."],"FM2TheoryStage"]];
 symbols=Join[Lookup[fields,"Symbol"],Keys[data["Parameters"]]];
 Do[If[!AssociationQ[symmetry]||Lookup[symmetry,"Type",""]=!="Z2"||!AssociationQ[Lookup[symmetry,"Charges",None]],
   Throw[fail["SymmetryCapability","Only explicit component Z2 charges are checked here; other group metadata needs a representation backend."],"FM2TheoryStage"]];
  charges=symmetry["Charges"];
  If[Complement[Keys[charges],symbols]=!={}||!AllTrue[Values[charges],MemberQ[{-1,1},#]&],Throw[fail["SymmetryCharges","Z2 charges must map declared field or parameter symbols to +1 or -1."],"FM2TheoryStage"]];
  Do[sign=Times@@(Lookup[charges,#,1]&/@row["Fields"]);
   sign*=Times@@Table[Lookup[charges,p,1]^Exponent[row["Coefficient"],p],{p,Keys[data["Parameters"]]}];
   If[sign=!=1,Throw[fail["SymmetryViolation","The supplied local action violates a declared component Z2 symmetry."],"FM2TheoryStage"]],{row,terms}],{symmetry,symmetries}];True
],"FM2TheoryStage"];
readTheoryFile[path_String]:=Catch[Module[{absolute=ExpandFileName[path],context,value},
 If[!FileExistsQ[absolute],Throw[fail["TheoryFile","Theory input file does not exist."],"FM2TheoryStage"]];
 context="FeAmMatch2`Input"<>StringTake[IntegerString[FileHash[absolute,"SHA256"],16,64],16]<>"`";
 value=Block[{$Context=context,$ContextPath={"FeAmMatch2`","System`"}},Quiet[Check[Get[absolute],$Failed]]];
 If[value===$Failed,Throw[fail["TheoryRead","Theory input could not be evaluated."],"FM2TheoryStage"]];
 If[AssociationQ[value],Throw[Join[value,<|"Source"-><|"Path"->absolute,"SHA256"->IntegerString[FileHash[absolute,"SHA256"],16,64]|>|>],"FM2TheoryStage"]];
 If[FileExtension[absolute]==="fr",Throw[readFeynRulesTheory[absolute,context],"FM2TheoryStage"]];
 fail["TheoryRead","A native theory file must evaluate to an Association."]
],"FM2TheoryStage"];
readFeynRulesTheory[path_,context_]:=Catch[Module[{classes,pars,lag,fields={},parameters=<||>,wc,roles,classRules,lookup,mass,name,role,symbol,dim,benchmark,p},
 lookup[rules_,key_,default_]:=With[{hits=Cases[rules,Rule[k_Symbol,v_]/;SymbolName[k]===key:>v,{1}]},If[hits==={},default,First[hits]]];
 classes=ToExpression[context<>"M$ClassesDescription"];pars=ToExpression[context<>"M$Parameters"];lag=ToExpression[context<>"Ltot"];
 If[!ListQ[classes]||!ListQ[pars],Throw[fail["FeynRulesInput","Expected M$ClassesDescription, M$Parameters and Ltot."],"FM2TheoryStage"]];
 Do[
  If[Head[row]=!=Equal||Length[row]=!=2||!ListQ[row[[2]]],Throw[fail["FeynRulesClasses","Unsupported FeynRules class declaration."],"FM2TheoryStage"]];
  classRules=row[[2]];symbol=lookup[classRules,"ClassName",Missing[]];
  If[Head[symbol]=!=Symbol||!TrueQ[lookup[classRules,"SelfConjugate",False]]||lookup[classRules,"Indices",{}]=!={},Throw[fail["TheoryCapability","This action adapter currently supports real scalar components without open internal indices."],"FM2TheoryStage"]];
  If[SymbolName[Head[row[[1]]]]=!="S",Throw[fail["TheoryCapability","Only scalar FeynRules classes are executable in this adapter."],"FM2TheoryStage"]];
  name=SymbolName[symbol];mass=lookup[classRules,"Mass",0];If[ListQ[mass],benchmark=If[Length[mass]>1,mass[[2]],1];mass=First[mass],benchmark=1];
  role=lookup[classRules,"FullName","light"];If[!MemberQ[{"heavy","light"},role],Throw[fail["FieldRole","Use FullName -> heavy/light in a FeynRules scalar model."],"FM2TheoryStage"]];
  AppendTo[fields,<|"Name"->name,"Symbol"->symbol,"Role"->role,"Mass"->mass,"Real"->True,"Spin"->0|>];
  If[Head[mass]===Symbol,AssociateTo[parameters,mass-><|"Dimension"->1,"Value"->benchmark|>]],{row,classes}];
 Do[
  If[Head[row]=!=Equal||Length[row]=!=2||Head[row[[1]]]=!=Symbol||!ListQ[row[[2]]],Throw[fail["FeynRulesParameters","Unsupported FeynRules parameter declaration."],"FM2TheoryStage"]];
  p=row[[1]];dim=lookup[row[[2]],"MassDimension",Automatic];benchmark=lookup[row[[2]],"Value",1/7];
  If[TrueQ[lookup[row[[2]],"ComplexParameter",False]],Throw[fail["TheoryCapability","The current real-scalar action adapter requires real independent parameters."],"FM2TheoryStage"]];
  If[!KeyExistsQ[parameters,p],AssociateTo[parameters,p-><|"Dimension"->dim,"Value"->benchmark|>]],{row,pars}];
 wc=ToExpression[context<>"FM2$WilsonCoefficients"];If[!ListQ[wc],wc={}];
 <|"Schema"->"feammatch2.theory.v2","Name"->ToExpression[context<>"M$ModelName"],"Fields"->fields,"Parameters"->parameters,
  "WilsonCoefficients"->wc,"Lagrangian"->lag,"Source"-><|"Path"->path,"SHA256"->IntegerString[FileHash[path,"SHA256"],16,64]|>|>
],"FM2TheoryStage"];

normalizeScalarAction[expr_,fields_List]:=Catch[Module[{result=expr},
 result=FixedPoint[Function[x,x/.{
  (HoldPattern[h_Symbol[f_,mu_]]/;MemberQ[{"del","DC"},SymbolName[h]]&&MemberQ[fields,f]):>ScalarJet[f,{mu}],
  (HoldPattern[h_Symbol[ScalarJet[f_,ind_List],mu_]]/;MemberQ[{"del","DC"},SymbolName[h]]):>ScalarJet[f,Append[ind,mu]]}],result,32];
 Expand[result]
],"FM2TheoryStage"];

scalarTermData[term_,fields_List]:=Catch[Module[{factors,coefficient=1,jets={},base,power,indices,counts},
 factors=If[Head[term]===Times,List@@term,{term}];
 Do[
  {base,power}=If[Head[factor]===Power,List@@factor,{factor,1}];
  If[MemberQ[fields,base]||Head[base]===ScalarJet,
   If[!IntegerQ[power]||power<0,Throw[fail["ScalarMonomial","Fields must have nonnegative integer powers."],"ScalarTerms"]];
   If[MemberQ[fields,base],jets=Join[jets,ConstantArray[{base,{}},power]],
    If[Length[base]=!=2||!MemberQ[fields,base[[1]]]||!ListQ[base[[2]]]||!AllTrue[base[[2]],Head[#]===Symbol&],Throw[fail["ScalarJet","Declare every differentiated field and use symbolic Lorentz labels."],"ScalarTerms"]];
    jets=Join[jets,ConstantArray[List@@base,power]]],
   If[!FreeQ[factor,Alternatives@@Join[fields,{ScalarJet}]],Throw[fail["ScalarMonomial","Unsupported field dependence in local scalar action."],"ScalarTerms"]];coefficient*=factor],{factor,factors}];
 indices=Flatten[If[jets==={},{},jets[[All,2]]]];counts=Counts[indices];
 If[!AllTrue[Values[counts],#===2&],Throw[fail["ScalarContractions","Every Lorentz label in a scalar monomial must occur exactly twice."],"ScalarTerms"]];
 <|"Coefficient"->coefficient,"Jets"->jets,"Fields"->If[jets==={},{},jets[[All,1]]],"DerivativeOrder"->Length[indices],
  "FieldDegree"->Length[jets],"OperatorDimension"->Length[jets]+Length[indices]|>
],"ScalarTerms"];

ExtractScalarTerms[theory_Association]:=Catch[Module[{fields=Lookup[theory,"FieldSymbols",{}],lag=Lookup[theory,"Lagrangian",Missing[]],terms,result},
 If[fields==={}||MissingQ[lag],Throw[fail["TheoryObject","Load a scalar action before extracting terms."],"FM2TheoryStage"]];
 terms=If[Head[Expand[lag]]===Plus,List@@Expand[lag],{Expand[lag]}];
 result=scalarTermData[#,fields]&/@terms;If[AnyTrue[result,FailureQ],Throw[SelectFirst[result,FailureQ],"FM2TheoryStage"]];result
],"FM2TheoryStage"];

inferParameterDimensions[terms_,parameters_Association]:=Catch[Module[{unknown,vars,rules,equations,solution,dims,coefficient,powers},
 unknown=Select[Keys[parameters],Lookup[parameters[#],"Dimension",Automatic]===Automatic&];
 vars=Table[Unique["parameterDimension"],Length[unknown]];rules=Thread[unknown->vars];
 dims=Association[Table[p->If[MemberQ[unknown,p],p/.rules,Lookup[parameters[p],"Dimension",Automatic]],{p,Keys[parameters]}]];
 equations=Table[coefficient=row["Coefficient"];powers=Exponent[coefficient,#]&/@Keys[parameters];
  If[!FreeQ[coefficient/Times@@MapThread[Power,{Keys[parameters],powers}],Alternatives@@Keys[parameters]],Throw[fail["ParameterDimensions","Action coefficients must have homogeneous parameter dimensions."],"FM2TheoryStage"]];
  Total[powers*Values[dims]]+row["OperatorDimension"]==4,{row,terms}];
 If[unknown==={},Throw[If[And@@(TrueQ/@equations),parameters,fail["ActionDimension","Every local action term must have total mass dimension four."]],"FM2TheoryStage"]];
 solution=Quiet[Solve[equations,vars]];
 If[Length[solution]=!=1||!FreeQ[vars/.First[solution],Alternatives@@vars],Throw[fail["ParameterDimensions","Supply MassDimension metadata for dimensionally undetermined parameters."],"FM2TheoryStage"]];
 Association[Table[p->Join[parameters[p],<|"Dimension"->(dims[p]/.First[solution])|>],{p,Keys[parameters]}]]
],"FM2TheoryStage"];

theoryInput[input_,kind_String]:=Catch[Module[{data,fields,parameters,lag,wc,roles,symbols,normalized,terms,inferred,source,declared,metadata},
 data=If[StringQ[input],readTheoryFile[input],input];If[FailureQ[data],Throw[data,"FM2TheoryStage"]];
 If[!AssociationQ[data]||Lookup[data,"Schema",""]=!="feammatch2.theory.v2",Throw[fail["TheorySchema","Expected feammatch2.theory.v2 action metadata."],"FM2TheoryStage"]];
 fields=Lookup[data,"Fields",{}];parameters=Lookup[data,"Parameters",<||>];lag=Lookup[data,"Lagrangian",Missing[]];wc=Lookup[data,"WilsonCoefficients",{}];
 If[fields==={}||!AssociationQ[parameters]||MissingQ[lag]||!AllTrue[fields,AssociationQ[#]&&Head[Lookup[#,"Symbol",Missing[]]]===Symbol&&MemberQ[{"heavy","light"},Lookup[#,"Role",""]]&&TrueQ[Lookup[#,"Real",True]]&&Lookup[#,"Spin",0]===0&],Throw[fail["TheoryInput","Supply real scalar components, masses, roles, parameters and a local Lagrangian."],"FM2TheoryStage"]];
 symbols=Lookup[fields,"Symbol"];If[Length[DeleteDuplicates[symbols]]=!=Length[symbols]||Length[DeleteDuplicates[Lookup[fields,"Name",fieldName/@symbols]]]=!=Length[symbols]||Intersection[symbols,Keys[parameters]]=!={},Throw[fail["TheorySymbols","Field names, field symbols and parameter identities must be distinct."],"FM2TheoryStage"]];
 If[kind==="EFT"&&(AnyTrue[fields,#["Role"]==="heavy"&]||wc==={}||!AllTrue[wc,KeyExistsQ[parameters,#]&]),Throw[fail["EFTCoefficients","An EFT needs light fields and an explicit list of declared independent Wilson coefficients."],"FM2TheoryStage"]];
 If[kind==="UV"&&!AnyTrue[fields,#["Role"]==="heavy"&],Throw[fail["UVHeavyFields","A UV matching model must declare a heavy field."],"FM2TheoryStage"]];
 normalized=normalizeScalarAction[lag,symbols];
 data=Join[data,<|"Kind"->kind,"FieldSymbols"->symbols,"Lagrangian"->normalized,"WilsonCoefficients"->wc|>];
 terms=ExtractScalarTerms[data];If[FailureQ[terms],Throw[terms,"FM2TheoryStage"]];
 metadata=validateComponentMetadata[data,terms];If[FailureQ[metadata],Throw[metadata,"FM2TheoryStage"]];
 declared=DeleteDuplicates[Cases[Lookup[terms,"Coefficient"],s_Symbol/;Context[s]=!="System`",Infinity]];
 If[Complement[declared,Keys[parameters]]=!={},Throw[fail["UndeclaredParameters","Every action coefficient symbol needs parameter metadata."],"FM2TheoryStage"]];
 inferred=inferParameterDimensions[terms,parameters];If[FailureQ[inferred],Throw[inferred,"FM2TheoryStage"]];
 If[kind==="EFT"&&!PolynomialQ[normalized,wc],Throw[fail["EFTLinearity","The Green action must be polynomial in its declared Wilson coefficients."],"FM2TheoryStage"]];
 If[kind==="EFT"&&AnyTrue[CoefficientRules[normalized,wc],Total[First[#]]>1&],Throw[fail["EFTLinearity","Use independent coefficients linearly multiplying Green operators."],"FM2TheoryStage"]];
 Join[data,<|"Parameters"->inferred,"Terms"->terms,"Identity"->theoryIdentity[{kind,fields,inferred,normalized,wc,Lookup[data,"Symmetries",{}]}]|>]
],"FM2TheoryStage"];
LoadUVModel[input_]:=theoryInput[input,"UV"];
LoadEFTModel[input_]:=theoryInput[input,"EFT"];

CreateMatchingTask[uvInput_,eftInput_,request_Association:<||>]:=Catch[Module[{uv,eft,uvLight,eftLight,mapping,byName,loops,dimension,parameterWeights,heavy,potential,heavyMasses,lightMasses},
 If[Complement[Keys[request],{"Loops","MaxDimension","PhysicalBasis","Scheme","Tadpole","Vacuum","ParameterWeights","LightFieldMap","PhysicalFieldMap","LoopSymbol"}]=!={},Throw[fail["RequestKeys","Unknown matching request field. Use the single current action-input workflow."],"FM2TheoryStage"]];
 uv=If[AssociationQ[uvInput]&&Lookup[uvInput,"Kind",""]==="UV",uvInput,LoadUVModel[uvInput]];
 If[FailureQ[uv],Throw[uv,"FM2TheoryStage"]];eft=If[AssociationQ[eftInput]&&Lookup[eftInput,"Kind",""]==="EFT",eftInput,LoadEFTModel[eftInput]];If[FailureQ[eft],Throw[eft,"FM2TheoryStage"]];
 loops=Lookup[request,"Loops",2];dimension=Lookup[request,"MaxDimension",6];
 If[!MemberQ[{4,5,6},dimension],Throw[fail["DimensionCapability","The audited automatic physical conversion supports dimensions four through six."],"FM2TheoryStage"]];
 If[Lookup[request,"Scheme","MSbar"]=!="MSbar"||!MemberQ[{"MSbar","heavy_tadpole_free"},Lookup[request,"Tadpole","MSbar"]]||Lookup[request,"Vacuum","origin"]=!="origin",Throw[fail["TheorySchemeCapability","The independent-action runner implements MSbar at the stationary origin; supported tadpole conventions are MSbar and the explicitly restricted heavy_tadpole_free scheme."],"FM2TheoryStage"]];
 If[Lookup[request,"Tadpole","MSbar"]==="heavy_tadpole_free",heavy=#["Symbol"]&/@Select[uv["Fields"],#["Role"]==="heavy"&];potential=uv["Lagrangian"]/.ScalarJet[_,_]->0;
  If[AnyTrue[heavy,Expand[D[potential,#]/.Thread[heavy->0]]=!=0&],Throw[fail["TadpoleSchemeCapability","The heavy_tadpole_free adapter requires a vanishing classical heavy source. Nonzero classical heavy solutions currently require MSbar tadpoles."],"FM2TheoryStage"]]];
 parameterWeights=Lookup[request,"ParameterWeights",<||>];
 If[!AssociationQ[parameterWeights]||!AllTrue[Values[parameterWeights],IntegerQ[#]&&#>=0&],Throw[fail["ParameterWeights","ParameterWeights must map declared UV parameters to nonnegative integer light weights."],"FM2TheoryStage"]];
 parameterWeights=Association[KeyValueMap[Function[{key,value},With[{symbol=If[StringQ[key],SelectFirst[Keys[uv["Parameters"]],SymbolName[#]===key&,Missing[]],key]},If[!KeyExistsQ[uv["Parameters"],symbol],Throw[fail["ParameterWeights","An undeclared UV parameter was assigned a light weight."],"FM2TheoryStage"]];symbol->value]],parameterWeights]];
 heavyMasses=DeleteDuplicates[#["Mass"]&/@Select[uv["Fields"],#["Role"]==="heavy"&]];lightMasses=DeleteDuplicates[#["Mass"]&/@Select[uv["Fields"],#["Role"]==="light"&]];
 If[Intersection[heavyMasses,lightMasses]=!={},Throw[fail["HeavyLightMassSeparation","Heavy and light fields need separate mass representatives for the hard expansion."],"FM2TheoryStage"]];
 If[loops===2&&Length[heavyMasses]>1,Throw[fail["TwoLoopMassCapability","The current independent-action two-loop path supports one heavy threshold. Multiple heavy thresholds need an additional master-integral validation gate."],"FM2TheoryStage"]];
 If[AnyTrue[Intersection[Keys[parameterWeights],lightMasses],parameterWeights[#]=!=1&]||AnyTrue[Intersection[Keys[parameterWeights],heavyMasses],parameterWeights[#]=!=0&],Throw[fail["MassWeights","Hard expansion fixes light masses to weight one and the heavy threshold to weight zero."],"FM2TheoryStage"]];
 If[!IntegerQ[loops]||!MemberQ[Range[0,2],loops]||!IntegerQ[dimension]||dimension<4,Throw[fail["MatchingTarget","Declare loop order 0,1,2 and a local dimension cutoff at least four."],"FM2TheoryStage"]];
 uvLight=Select[uv["Fields"],#["Role"]==="light"&];eftLight=eft["Fields"];
 byName=Association[Table[Lookup[f,"Name",SymbolName[f["Symbol"]]]->f["Symbol"],{f,uvLight}]];
 mapping=Lookup[request,"LightFieldMap",Automatic];
 If[mapping===Automatic,mapping=Table[With[{name=Lookup[f,"Name",SymbolName[f["Symbol"]]]},If[!KeyExistsQ[byName,name],Throw[fail["LightFieldMap","Provide LightFieldMap when UV/EFT field names differ."],"FM2TheoryStage"]];f["Symbol"]->byName[name]],{f,eftLight}]];
 If[!ListQ[mapping]||Sort[First/@mapping]=!=Sort[Lookup[eftLight,"Symbol"]]||Sort[Last/@mapping]=!=Sort[Lookup[uvLight,"Symbol"]],Throw[fail["LightFieldMap","The EFT light-field map must bijectively cover the UV light fields."],"FM2TheoryStage"]];
 With[{resolvedRequest=Join[<|"Loops"->loops,"MaxDimension"->dimension,"PhysicalBasis"->Automatic,"Scheme"->"MSbar","Tadpole"->"MSbar","Vacuum"->"origin"|>,request,<|"ParameterWeights"->parameterWeights|>]},
 <|"Schema"->"feammatch2.matching-task.v2","UV"->uv,"EFT"->eft,"Request"->resolvedRequest,
  "LightFieldMap"->mapping,"Identity"->theoryIdentity[{uv["Identity"],eft["Identity"],resolvedRequest,mapping}],"Stage"->"input_frozen"|>]
],"FM2TheoryStage"];
