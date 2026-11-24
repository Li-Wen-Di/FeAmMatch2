(* ABC4EFT is an actual basis provider, not a table of known scalar examples.
   Current adapter: real scalar components with no open internal indices, d<=6. *)
ClearAll[scalarExactParities,scalarTypeAllowed,translateABCScalar];
scalarExactParities[eft_]:=Catch[Module[{fields=eft["FieldSymbols"],action=eft["Lagrangian"],rules},
 Select[Rest[Subsets[fields]],Function[odd,rules=Join[Thread[odd->-odd],{ScalarJet[f_,ind_]/;MemberQ[odd,f]:>-ScalarJet[f,ind]}];
  Expand[SubstituteScalarFields[action,Thread[fields->(fields/.Thread[odd->-odd])],fields]-action]===0]]
],"FM2TheoryStage"];
scalarTypeAllowed[powers_,fields_,parities_]:=AllTrue[parities,Function[odd,EvenQ[Sum[If[MemberQ[odd,fields[[i]]],powers[[i]],0],{i,Length[fields]}]]]];
translateABCScalar[expression_,aliases_List]:=Catch[Module[{result=expression,label},
 result=result/.HoldPattern[h_[x_,___]]/;Head[h]===Symbol&&SymbolName[h]==="ContractDelta":>x;
 result=result/.{
  HoldPattern[HoldForm[Subscript[_,mu_] f_Symbol]]:>ScalarJet[f,{Symbol["FeAmMatch2`ABCLorentz`mu"<>IntegerString[Hash[mu],16]]}],
  HoldPattern[HoldForm[Superscript[_,mu_] f_Symbol]]:>ScalarJet[f,{Symbol["FeAmMatch2`ABCLorentz`mu"<>IntegerString[Hash[mu],16]]}]};
 result=result/.s_Symbol/;KeyExistsQ[Association[aliases],SymbolName[s]]:>Association[aliases][SymbolName[s]];
 If[!FreeQ[result,_HoldForm|_DisplayForm],Throw[fail["ABCOperatorSyntax","ABC4EFT returned a scalar tensor expression not supported by this adapter."],"FM2TheoryStage"]];Expand[result]
],"FM2TheoryStage"];
ResolvePhysicalBasis[eft_Association,request_Association,directory_String,config_Association]:=Catch[Module[
 {choice=Lookup[request,"PhysicalBasis",Automatic],dimension=Lookup[request,"MaxDimension",6],fields=eft["FieldSymbols"],parities,powers,types,aliases,aliasRules,modelNames,script,resultFile,source,job,raw,operators={},evidence={},translated,matrix,high={},low={},wc={},parameters,coefficient,action,mu=Unique["physicalLorentz"],physical,out=ExpandFileName[directory],abc,wolfram,type,normalization,index=0},
 If[choice=!=Automatic,Throw[If[AssociationQ[choice]&&Lookup[choice,"Kind",""]==="EFT",choice,LoadEFTModel[choice]],"FM2TheoryStage"]];
 If[dimension>6||Length[fields]>4,Throw[fail["ABCCapability","The automatic component-scalar adapter currently supports at most four real singlets and dimension six. Supply a supported explicit physical action otherwise."],"FM2TheoryStage"]];
 abc=Lookup[config,"ABC4EFTDirectory",Missing[]];wolfram=Lookup[config,"WolframExecutable",Missing[]];
 If[!StringQ[abc]||!FileExistsQ[FileNameJoin[{abc,"Kernel","init.m"}]]||!StringQ[wolfram]||!FileExistsQ[wolfram],Throw[fail["ABCDependency","Declare ABC4EFTDirectory and WolframExecutable for automatic physical-basis generation."],"FM2TheoryStage"]];
 If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
 parities=scalarExactParities[eft];modelNames=Table["fmfield"<>ToString[i],{i,Length[fields]}];aliases=Thread[modelNames->fields];
 types=Flatten[Table[Table[<|"Powers"->p,"Derivatives"->d,"Dimension"->n+d|>,{p,Select[Tuples[Range[0,n],Length[fields]],Total[#]===n&&scalarTypeAllowed[#,fields,parities]&]}],{n,4,dimension},{d,0,dimension-n,2}],2];
 types=Select[types,#["Dimension"]>4&];
 script=FileNameJoin[{out,"abc-basis.wl"}];resultFile=FileNameJoin[{out,"abc-raw.wl"}];
 source="$HistoryLength=0;Get["<>ToString[FileNameJoin[{ExpandFileName[abc],"Kernel","init.m"}],InputForm]<>"];\nABC4EFT`ModelIni[Global`fmModel];\n"<>
  "hermitianKey=SelectFirst[First/@Options[ABC4EFT`AddField],SymbolName[#]===\"Hermitian\"&];\n"<>
  StringJoin[Table["With[{key=hermitianKey},ABC4EFT`AddField[Global`fmModel,"<>ToString[name,InputForm]<>",0,{},key->True]];\n",{name,modelNames}]]<>
  "workingKey=SelectFirst[First/@Options[ABC4EFT`GetBasisForType],SymbolName[#]===\"Working\"&];\ndesymKey=SelectFirst[First/@Options[ABC4EFT`GetBasisForType],SymbolName[#]===\"DeSym\"&];\nresults={};\n"<>
  StringJoin[Table[type=Times@@MapThread[Power,{modelNames,row["Powers"]}] "D"^row["Derivatives"];
    "AppendTo[results,With[{wk=workingKey,dk=desymKey},TimeConstrained[ABC4EFT`GetBasisForType[Global`fmModel,"<>ToString[type,InputForm]<>",wk->True,dk->False],60,$Failed]]];\n",{row,types}]]<>
  "Put[results,"<>ToString[resultFile,InputForm]<>"];If[FreeQ[results,$Failed],Print[\"FM2_ABC_SUCCESS\"],Exit[2]];\n";
 Export[script,source,"Text"];job=runExternal[{wolfram,"-script",script},out,"abc"];If[FailureQ[job],Throw[job,"FM2TheoryStage"]];
 If[!FileExistsQ[resultFile]||!StringContainsQ[job["StandardOutput"],"FM2_ABC_SUCCESS"],Throw[fail["ABCOutput","ABC4EFT did not confirm a complete basis result."],"FM2TheoryStage"]];
 raw=Get[resultFile];If[!ListQ[raw]||Length[raw]=!=Length[types],Throw[fail["ABCOutput","ABC4EFT basis result has the wrong number of types."],"FM2TheoryStage"]];
 Do[
  If[!AssociationQ[raw[[i]]],Throw[fail["ABCOutput","A basis type returned no supported Association."],"FM2TheoryStage"]];
  If[raw[[i]]===<||>,AppendTo[evidence,<|"Type"->types[[i]],"Rank"->0|>];Continue[]];
  If[!KeyExistsQ[raw[[i]],"m-basis"]||!KeyExistsQ[raw[[i]],"Kpm"],Throw[fail["ABCWorkingOutput","Expected actual Working m-basis and Kpm from ABC4EFT."],"FM2TheoryStage"]];
  translated=translateABCScalar[#,aliases]&/@raw[[i]]["m-basis"];If[AnyTrue[translated,FailureQ],Throw[SelectFirst[translated,FailureQ],"FM2TheoryStage"]];
  matrix=raw[[i]]["Kpm"];If[!MatrixQ[matrix]||Dimensions[matrix][[2]]=!=Length[translated],Throw[fail["ABCMatrix","ABC physical-to-monomial matrix has inconsistent dimensions."],"FM2TheoryStage"]];
  operators=Join[operators,Expand/@(matrix.translated)];AppendTo[evidence,<|"Type"->types[[i]],"Rank"->Length[matrix]|>],{i,Length[types]}];
 (* Canonical kinetic terms and all symmetry-allowed low-dimensional potential
    structures are managed here; they are not inferred from an ABC massless EOM. *)
 low=Flatten[Table[Times@@MapThread[Power,{fields,p}] /Times@@(Factorial/@p),{n,1,4},{p,Select[Tuples[Range[0,n],Length[fields]],Total[#]===n&&scalarTypeAllowed[#,fields,parities]&]}]];
 parameters=Association[Table[f["Mass"]-><|"Dimension"->1,"Value"->1|>,{f,eft["Fields"]}]];
 action=Total[ScalarJet[#,{mu}]^2/2&/@fields];
 Do[index++;coefficient=Symbol["FeAmMatch2`Physical"<>StringTake[eft["Identity"],12]<>"`C"<>ToString[index]];
  translated=ExtractScalarTerms[Join[eft,<|"Lagrangian"->operator|>]];If[FailureQ[translated],Throw[translated,"FM2TheoryStage"]];
  normalization=If[MemberQ[low,operator],1,Times@@(Factorial/@Values[Counts[First[translated]["Fields"]]])];
  AppendTo[wc,coefficient];AssociateTo[parameters,coefficient-><|"Dimension"->4-First[translated]["OperatorDimension"],"Value"->1/13|>];action-=coefficient operator/normalization,{operator,Join[low,operators]}];
 physical=LoadEFTModel[<|"Schema"->"feammatch2.theory.v2","Name"->"ABCPhysical","Fields"->eft["Fields"],"Parameters"->parameters,"WilsonCoefficients"->wc,"Lagrangian"->action|>];
 If[FailureQ[physical],Throw[physical,"FM2TheoryStage"]];
 physical=Join[physical,<|"Provider"-><|"Name"->"ABC4EFT","Directory"->ExpandFileName[abc],"InitSHA256"->IntegerString[FileHash[FileNameJoin[{abc,"Kernel","init.m"}],"SHA256"],16,64],"Queries"->evidence,"RawResult"->resultFile,"ExactSignParities"->parities,
 "Scope"->"Real scalar singlets, dimension<=6; massless derivative types with <=3 legs vanish by momentum conservation and are excluded before querying. Non-discrete internal symmetries require an explicit component basis."|>|>];
 Put[physical,FileNameJoin[{out,"physical.wl"}]];physical
],"FM2TheoryStage"];
