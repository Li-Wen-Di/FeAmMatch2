(* Independent UV/EFT FeynRules export; no model-specific vertex assumptions. *)
ClearAll[theoryFRText,ufoScalarPolynomial,validateTheoryUFO,freeScalarAction,interactionTheory,completeLocalUFOContacts,ufoMonomialText];
freeScalarAction[theory_]:=Catch[Module[{mu=Unique["freeLorentz"]},
 Total[Table[ScalarJet[f["Symbol"],{mu}]^2/2-f["Mass"]^2*f["Symbol"]^2/2,{f,Select[theory["Fields"],!TrueQ[Lookup[#,"Auxiliary",False]]&]}]]
],"FM2TheoryStage"];
interactionTheory[theory_]:=Catch[Module[{lag=Expand[theory["Lagrangian"]-freeScalarAction[theory]],result},
 result=Join[theory,<|"Lagrangian"->lag|>];Join[result,<|"Terms"->ExtractScalarTerms[result]|>]
],"FM2TheoryStage"];
ufoMonomialText[variables_,powers_,momenta_]:=Catch[Module[{factors={},counter=0,legs},
 Do[
  legs=(First[FirstPosition[momenta,#]]&/@(List@@variables[[i]]));
  Do[counter++;AppendTo[factors,"P(-"<>ToString[counter]<>","<>ToString[legs[[1]]]<>")*P(-"<>ToString[counter]<>","<>ToString[legs[[2]]]<>")"],{powers[[i]]}],{i,Length[variables]}];
 If[factors==={},"1",StringRiffle[factors,"*"]]
],"FM2TheoryStage"];
completeLocalUFOContacts[theory_,ufo_,fieldAliases_,parameterAliases_]:=Catch[Module[
 {interaction=interactionTheory[theory],channels,momenta,vertex,variables,parts,lorentzText="",couplingText="",vertexText="",records={},structures,couplings,counter=0,id,coefficient,lorentzName,couplingName,fieldsText,exprText},
 channels=DeleteDuplicates[Sort/@Select[Lookup[theory["Terms"],"Fields"],0<Length[#]<=2&]];
 Do[
  momenta=independentExternalMomenta[Length[channel]];
  vertex=canonicalMomentumPolynomial[ScalarLocalVertex[interaction,channel,momenta],momenta];If[vertex===0,Continue[]];
  variables=DeleteDuplicates[Cases[vertex,_SP,{0,Infinity}]];parts=If[variables==={},{{}->vertex},CoefficientRules[vertex,variables]];
  structures={};couplings={};
  Do[
   counter++;lorentzName="FM2L"<>ToString[counter];couplingName="FM2GC"<>ToString[counter];
   coefficient=Last[part]/.parameterAliases;
   exprText=StringReplace[formText[I*coefficient],{"fmImag"->"complex(0,1)","^"->"**"}];
   lorentzText=lorentzText<>"\n"<>lorentzName<>" = Lorentz(name='"<>lorentzName<>"', spins="<>ToString[ConstantArray[1,Length[channel]],InputForm]<> ", structure='"<>ufoMonomialText[variables,First[part],momenta]<>"')\n";
   couplingText=couplingText<>"\n"<>couplingName<>" = Coupling(name='"<>couplingName<>"', value='"<>exprText<>"', order={'QED':1})\n";
   AppendTo[structures,"L."<>lorentzName];AppendTo[couplings,"(0,"<>ToString[Length[couplings]]<>"):C."<>couplingName],{part,parts}];
  fieldsText=StringRiffle[("P."<>SymbolName[#/.fieldAliases])&/@channel,","];
  vertexText=vertexText<>"\nFM2V"<>ToString[Length[records]+1]<>" = Vertex(name='FM2V"<>ToString[Length[records]+1]<>"', particles=["<>fieldsText<>"], color=['1'], lorentz=["<>StringRiffle[structures,","]<>"], couplings={"<>StringRiffle[couplings,","]<>"})\n";
  AppendTo[records,<|"Fields"->channel,"InteractionVertexWithoutI"->vertex,"Origin"->"generic functional derivative of action minus declared free kernel"|>],{channel,channels}];
 (* Mathematica list delimiters are changed only in the generated Python syntax. *)
 lorentzText=StringReplace[lorentzText,{"spins={"->"spins=[","}, structure"->"], structure"}];
 Do[Export[FileNameJoin[{ufo,file}],Import[FileNameJoin[{ufo,file}],"Text"]<>text,"Text"],{pair,{{"lorentz.py",lorentzText},{"couplings.py",couplingText},{"vertices.py",vertexText}}},{file,{First[pair]}},{text,{Last[pair]}}];
 Put[records,FileNameJoin[{DirectoryName[ufo],"local-contact-completion.wl"}]];records
],"FM2TheoryStage"];
theoryFRText[expr_,rules_List]:=Catch[Module[{x=expr/.rules},
 x=x/.ScalarJet[f_,indices_List]:>Fold[frDerivative,f,indices];
 StringReplace[ToString[x,InputForm],{"FeAmMatch2`Private`"->"","frDerivative"->"del"}]
],"FM2TheoryStage"];
ufoScalarPolynomial[text_String,momenta_List]:=Catch[Module[{expr=ParseScalarExpression[text],indices,rules,next},
 If[FailureQ[expr],Throw[expr,"FM2TheoryStage"]];
 If[AnyTrue[Cases[expr,_ufoMomentum,Infinity],!Between[#[[2]],{1,Length[momenta]}]&],Throw[fail["UFOLorentz","Invalid particle leg in UFO momentum tensor."],"FM2TheoryStage"]];
 indices=DeleteDuplicates[Cases[expr,ufoMomentum[i_,_]:>i,Infinity]];
 rules=Thread[indices->Table[Unique["lorentzIndex"],Length[indices]]];
 expr=expr/.ufoMomentum[i_,leg_]:>momentumComponent[momenta[[leg]],i/.rules];
 next=Expand[expr]//.{momentumComponent[a_,i_]^2:>SP[a,a],momentumComponent[a_,i_]*momentumComponent[b_,i_]:>SP[a,b]};
 If[!FreeQ[next,_momentumComponent],Throw[fail["UFOLorentz","UFO scalar Lorentz indices did not close."],"FM2TheoryStage"]];
 next
],"FM2TheoryStage"];
validateTheoryUFO[theory_,metadata_,fieldAliases_,parameterAliases_]:=Catch[Module[
 {fieldBack=Reverse/@fieldAliases,parameterBack=Reverse/@parameterAliases,checks={},actual,expected,fields,momenta,structure,coupling,color,key,seen={},row,interaction=interactionTheory[theory],expectedChannels,missing},
 Do[
  fields=(modelSymbol/@vertex["ordered_fields"])/.fieldBack;
  If[!AllTrue[fields,MemberQ[theory["FieldSymbols"],#]&],Throw[fail["UFOField","Unexpected field identity in exported UFO."],"FM2TheoryStage"]];
  momenta=independentExternalMomenta[Length[fields]];actual=0;
  Do[
   color=vertex["color"][[term["color_index"]+1]];
   If[color=!="1",Throw[fail["UFOColor","Open internal tensors require an indexed action adapter."],"FM2TheoryStage"]];
   structure=metadata["lorentz"][vertex["lorentz"][[term["lorentz_index"]+1]]];
   If[!AllTrue[structure["spins"],#===1&],Throw[fail["UFOSpin","Only real scalar Lorentz tensors are supported here."],"FM2TheoryStage"]];
   structure=ufoScalarPolynomial[structure["structure"],momenta];If[FailureQ[structure],Throw[structure,"FM2TheoryStage"]];
   coupling=ParseScalarExpression[metadata["couplings"][term["coupling"]]]/.parameterBack;
   If[FailureQ[coupling],Throw[coupling,"FM2TheoryStage"]];actual+=coupling*structure,{term,vertex["terms"]}];
  expected=I*ScalarLocalVertex[interaction,fields,momenta];
  actual=canonicalMomentumPolynomial[actual,momenta];expected=canonicalMomentumPolynomial[expected,momenta];
  AppendTo[checks,<|"Fields"->(fieldName/@fields),"Residual"->Factor[actual-expected]|>];
  AppendTo[seen,Sort[fields]],{vertex,metadata["vertices"]}];
 If[AnyTrue[checks,#["Residual"]=!=0&],Throw[Failure["UFOVertexMismatch",<|"MessageTemplate"->"Actual UFO derivative vertices differ from the input local action.","Checks"->checks|>],"FM2TheoryStage"]];
 expectedChannels=Select[DeleteDuplicates[Sort/@Select[Lookup[theory["Terms"],"Fields"],Length[#]>0&]],
  canonicalMomentumPolynomial[ScalarLocalVertex[interaction,#,independentExternalMomenta[Length[#]]],independentExternalMomenta[Length[#]]]=!=0&];
 missing=Complement[expectedChannels,DeleteDuplicates[seen]];
 If[missing=!={},Throw[Failure["UFOMissingVertices",<|"MessageTemplate"->"A nonzero local interaction channel is absent from the UFO.","MissingChannels"->missing|>],"FM2TheoryStage"]];
 <|"Passed"->True,"Checks"->checks,"ExportedChannels"->DeleteDuplicates[seen],"MissingLocalChannels"->missing|>
],"FM2TheoryStage"];

exportTheoryUFONew[theory_Association,directory_String,configuration_Association]:=Catch[Module[
 {config=DefaultToolchain[configuration],out=ExpandFileName[directory],fields=theory["Fields"],parameters=theory["Parameters"],
  fieldAliases,parameterAliases,indices,indexAliases,rules,classes={},rows={},mass,p,benchmark,source,script,ufo,job,metadata,validation,aliases,identity,marker,prior,files},
 If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
 If[!AllTrue[Lookup[config,{"WolframExecutable","PythonExecutable"},{}],StringQ[#]&&FileExistsQ[#]&]||!KeyExistsQ[config,"FeynRulesDirectory"],Throw[fail["TheoryExportToolchain","Configure Wolfram, Python and FeynRules."],"FM2TheoryStage"]];
 fieldAliases=MapIndexed[#1["Symbol"]->modelSymbol["fmf"<>ToString[First[#2]]]&,fields];
 parameterAliases=MapIndexed[#1->modelSymbol["fmp"<>ToString[First[#2]]]&,Keys[parameters]];
 indices=DeleteDuplicates[Flatten[Cases[theory["Lagrangian"],ScalarJet[_,ii_List]:>ii,Infinity]]];
 indexAliases=MapIndexed[#1->modelSymbol["fmi"<>ToString[First[#2]]]&,indices];
 rules=Join[fieldAliases,parameterAliases,indexAliases];
 Do[
  mass=f["Mass"];benchmark=If[KeyExistsQ[parameters,mass],Lookup[parameters[mass],"Value",1],0];
  AppendTo[classes,"S["<>ToString[i]<>"] == {ClassName -> "<>SymbolName[Last[fieldAliases[[i]]]]<>", SelfConjugate -> True, Mass -> {"<>
    theoryFRText[mass,rules]<> ","<>ToString[benchmark,InputForm]<>"}, Width -> 0, FullName -> \""<>f["Role"]<>"\"}"],
  {i,Length[fields]},{f,{fields[[i]]}}];
 Do[
  benchmark=Lookup[parameters[p],"Value",1/7];
  If[!NumericQ[benchmark],benchmark=1/7];
  AppendTo[rows,SymbolName[p/.parameterAliases]<>" == {ParameterType -> External, ComplexParameter -> False, Value -> "<>ToString[benchmark,InputForm]<>", BlockName -> FM2INPUT, OrderBlock -> "<>ToString[i]<>", InteractionOrder -> {QED,1}}"],
  {i,Length[parameters]},{p,{Keys[parameters][[i]]}}];
 ufo=FileNameJoin[{out,"TheoryUFO"}];
 source="M$ModelName=\"TheoryUFO\";\nM$Information={Authors->{\"FeAmMatch2\"},Version->\"action-v2\"};\nM$InteractionOrderHierarchy={{QED,1}};\n"<>
  "M$ClassesDescription={"<>StringRiffle[classes,",\n"]<>"};\nM$Parameters={"<>StringRiffle[rows,",\n"]<>"};\nLtot:="<>theoryFRText[theory["Lagrangian"],rules]<>";\n";
 (* Exported symbols live in the FeynRules child kernel; strip our alias context. *)
 source=StringReplace[source,"FeAmMatch2`Model`"->""];
 Export[FileNameJoin[{out,"Theory.fr"}],source,"Text"];
 (* Wolfram source literals must use InputForm, not UTF-8 JSON byte strings. *)
 script="FR$Parallel=False;\nSymbol[\"FeynRules`Commutator\"];Symbol[\"FeynRules`MatrixSymbol\"];\n$FeynRulesPath="<>ToString[config["FeynRulesDirectory"],InputForm,CharacterEncoding->"ASCII"]<>";\nSetDirectory[$FeynRulesPath];Get[\"FeynRules.m\"];\nLoadModel["<>ToString[FileNameJoin[{out,"Theory.fr"}],InputForm,CharacterEncoding->"ASCII"]<>"];\nWriteUFO[Ltot,Output->"<>ToString[ufo,InputForm,CharacterEncoding->"ASCII"]<>"];\nIf[FileExistsQ["<>ToString[FileNameJoin[{ufo,"vertices.py"}],InputForm,CharacterEncoding->"ASCII"]<>"],Print[\"FM2_ACTION_UFO_SUCCESS\"],Exit[2]];Exit[];\n";
 Export[FileNameJoin[{out,"export-ufo.wls"}],StringReplace[script,"\\/"->"/"],"Text"];
 job=runExternal[{config["WolframExecutable"],"-script",FileNameJoin[{out,"export-ufo.wls"}]},out,"feynrules"];If[FailureQ[job],Throw[job,"FM2TheoryStage"]];
 If[!StringContainsQ[job["StandardOutput"],"FM2_ACTION_UFO_SUCCESS"],Throw[fail["TheoryUFO","FeynRules did not confirm action UFO output."],"FM2TheoryStage"]];
 completeLocalUFOContacts[theory,ufo,fieldAliases,parameterAliases];
 job=runPythonHelper["normalize-ufo",<|"directory"->ufo,"vertex_order_unit"->True,"merge_scalar_vertices"->True|>,Sort[FileNames["*.py",ufo]],out,"ufo-normalization",config];If[FailureQ[job],Throw[job,"FM2TheoryStage"]];
 metadata=Import[FileNameJoin[{out,"ufo-metadata.json"}],"RawJSON"];
 validation=validateTheoryUFO[theory,metadata,fieldAliases,parameterAliases];
 Put[validation,FileNameJoin[{out,"vertex-validation.wl"}]];If[FailureQ[validation],Throw[validation,"FM2TheoryStage"]];
 aliases=<|"Fields"->fieldAliases,"Parameters"->parameterAliases,"LorentzIndices"->indexAliases|>;Put[aliases,FileNameJoin[{out,"aliases.wl"}]];
 <|"Stage"->"action_ufo_exported","Directory"->out,"UFO"->ufo,"Aliases"->aliases,"Validation"->validation,"TheoryIdentity"->theory["Identity"]|>
],"FM2TheoryStage"];

(* A cache entry is accepted only if its action, tools, implementation and every
   consumed UFO/metadata artifact still have the recorded content identities. *)
ExportTheoryUFO[theory_Association,directory_String,configuration_Association]:=Module[
 {out=ExpandFileName[directory],config=resolveToolchain[configuration],key,manifestPath,payloadPath,manifest,files,result,frFiles,toolFiles},
 manifestPath=FileNameJoin[{out,"action-export-cache.json"}];payloadPath=FileNameJoin[{out,"action-export-cache.wl"}];
 frFiles=Sort[FileNames["*.m",Lookup[config,"FeynRulesDirectory",out<>"/absent"],Infinity]];
 toolFiles=Select[Lookup[config,{"WolframExecutable","PythonExecutable"},{}],StringQ[#]&&FileExistsQ[#]&];
 key=theoryIdentity[{KeyTake[theory,{"Lagrangian","Fields","Parameters","WilsonCoefficients"}],packageSourceSnapshot[],
   Table[{f,toolHash[f]},{f,Join[frFiles,toolFiles]}],out}];
 manifest=If[FileExistsQ[manifestPath],Quiet[Check[Import[manifestPath,"RawJSON"],$Failed]],$Failed];
 If[AssociationQ[manifest]&&Lookup[manifest,"key",None]===key&&AssociationQ[Lookup[manifest,"files",None]]&&
   Length[manifest["files"]]>0&&FileExistsQ[payloadPath]&&Lookup[manifest,"payload_sha256",None]===toolHash[payloadPath]&&
   And@@KeyValueMap[FileExistsQ[#1]&&toolHash[#1]===#2&,manifest["files"]],
  result=Quiet[Check[Get[payloadPath],$Failed]];
  If[AssociationQ[result]&&Lookup[result,"TheoryIdentity",None]===theory["Identity"],Return[Join[result,<|"Cache"->"content_verified_hit"|>]]]];
 result=exportTheoryUFONew[theory,out,config];If[FailureQ[result],Return[result]];
 files=Join[Sort[FileNames["*.py",result["UFO"]]],FileNameJoin[{out,#}]&/@{"aliases.wl","ufo-metadata.json","vertex-validation.wl","local-contact-completion.wl","Theory.fr"}];
 Put[result,payloadPath];
 Export[manifestPath,<|"key"->key,"payload_sha256"->toolHash[payloadPath],"files"->Association[Table[f->toolHash[f],{f,files}]]|>,"RawJSON"];
 Join[result,<|"Cache"->"recomputed"|>]
];
