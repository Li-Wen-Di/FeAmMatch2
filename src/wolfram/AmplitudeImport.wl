(* Restricted external expression parser and scalar graph-amplitude import. *)
ClearAll[modelSymbol];
modelSymbol[name_String]:=Symbol["FeAmMatch2`Model`"<>StringReplace[name,"_"->"$u"]];

ParseScalarExpression[text_String,atomRules_List:{}]:=Catch[Module[
 {tokens,pos=1,parseSum,parseProduct,parseUnary,parsePower,parseAtom,peek,take,expect,token,args,value},
 tokens=StringCases[text,RegularExpression["[A-Za-z][A-Za-z0-9_]*|(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?|\\*\\*|[+*/^(),-]"]];
 If[StringJoin[tokens]=!=StringReplace[text,WhitespaceCharacter..->""],Return[fail["ExpressionSyntax","Unrecognized expression token."]]];
 peek[]:=If[pos<=Length[tokens],tokens[[pos]],"END"];
 take[]:=If[pos<=Length[tokens],tokens[[pos++]],Throw[fail["ExpressionSyntax","Unexpected end of expression."],"ScalarParser"]];
 expect[t_]:=If[take[]=!=t,Throw[fail["ExpressionSyntax","Unbalanced expression."],"ScalarParser"]];
 parseAtom[]:=Module[{tok=take[],a={},f,value,dot},
  If[tok==="(",f=parseSum[];expect[")"];Return[f]];
  If[StringMatchQ[tok,RegularExpression["[0-9].*|\\.[0-9].*"]],
   (* Integer digit strings are handled without executing a general expression. *)
   f=StringSplit[ToLowerCase[tok],"e"];dot=StringPosition[f[[1]],"."];
   value=FromDigits[StringReplace[f[[1]],"."->""]]/10^If[dot==={},0,StringLength[f[[1]]]-dot[[1,1]]];
   If[Length[f]>1,value*=10^(If[StringStartsQ[f[[2]],"-"],-1,1]*FromDigits[StringTrim[f[[2]],"+"|"-"]])];Return[value]];
  If[!StringMatchQ[tok,RegularExpression["[A-Za-z][A-Za-z0-9_]*"]],Throw[fail["ExpressionSyntax","Expected a number or identifier."],"ScalarParser"]];
  If[peek[]==="(",
   take[];a={};If[peek[]=!=")",AppendTo[a,parseSum[]];While[peek[]===",",take[];AppendTo[a,parseSum[]]]];expect[")"];
   Switch[tok,"Den",If[Length[a]=!=3,Throw[fail["ExpressionSyntax","Den requires three arguments."],"ScalarParser"]];Return[Apply[Den,a]],
    "SP",If[Length[a]=!=2,Throw[fail["ExpressionSyntax","SP requires two arguments."],"ScalarParser"]];Return[If[MemberQ[a,0],0,Apply[SP,a]]],
    "FV",If[Length[a]=!=2||Head[a[[2]]]=!=Symbol,Throw[fail["ExpressionSyntax","FV requires a momentum and an index label."],"ScalarParser"]];Return[If[a[[1]]===0,0,Apply[momentumComponent,a]]],
    "P",If[Length[a]=!=2||!AllTrue[a,IntegerQ],Throw[fail["ExpressionSyntax","UFO P requires two integer indices."],"ScalarParser"]];Return[Apply[ufoMomentum,a]],
    _,Throw[fail["ExpressionSyntax","Unknown external function: "<>tok],"ScalarParser"]]];
  Switch[tok,"I"|"im",I,"D",D,"eps"|"ep",modelSymbol["eps"],_,modelSymbol[tok]/.atomRules]];
 parsePower[]:=Module[{v=parseAtom[]},If[MemberQ[{"^","**"},peek[]],take[];v=v^parseUnary[]];v];
 parseUnary[]:=Switch[peek[],"+",take[];parseUnary[],"-",take[];-parseUnary[],_,parsePower[]];
 parseProduct[]:=Module[{v={parseUnary[]},op},While[MemberQ[{"*","/"},peek[]],op=take[];AppendTo[v,If[op==="*",parseUnary[],1/parseUnary[]]]];Times@@v];
 parseSum[]:=Module[{v={parseProduct[]},op},While[MemberQ[{"+","-"},peek[]],op=take[];AppendTo[v,If[op==="+",parseProduct[],-parseProduct[]]]];Plus@@v];
 value=parseSum[];If[peek[]=!="END",Return[fail["ExpressionSyntax","Trailing expression token."]]];value
],"ScalarParser"];

ImportScalarAmplitude[record_Association,atomRules_List:{}]:=Catch[Module[{required,dens,powers,colors,lorentz,expr,rules,new},
 required={"n_loop","loop_den_list","loop_den_xpt_list","amp_color_list","amp_lorentz_list","signed_symmetry_factor","couplingfactor"};
 If[!And@@(KeyExistsQ[record,#]&/@required),Return[fail["AmplitudeMetadata","Missing FeAmGen amplitude data."]]];
 {dens,powers,colors,lorentz}=Lookup[record,{"loop_den_list","loop_den_xpt_list","amp_color_list","amp_lorentz_list"}];
 If[!MemberQ[{0,1,2},record["n_loop"]]||Length[dens]=!=Length[powers]||Length[colors]=!=Length[lorentz]||!AllTrue[powers,IntegerQ[#]&&#>0&],Return[fail["AmplitudeMetadata","Unsupported loops or inconsistent amplitude lists."]]];
 dens=ParseScalarExpression[#,atomRules]&/@dens;colors=ParseScalarExpression[#,atomRules]&/@colors;lorentz=ParseScalarExpression[#,atomRules]&/@lorentz;
 If[AnyTrue[Join[dens,colors,lorentz],FailureQ]||!AllTrue[dens,Head[#]===Den&],Return[fail["AmplitudeExpression","Invalid amplitude expression."]]];
 expr=Total[colors*lorentz]*ParseScalarExpression[record["signed_symmetry_factor"]]*ParseScalarExpression[record["couplingfactor"]]*Times@@MapThread[Power,{dens,powers}];
 expr=Expand[expr]//.{momentumComponent[a_,i_]^2:>SP[a,a],momentumComponent[a_,i_]*momentumComponent[b_,i_]:>SP[a,b]};
 If[!FreeQ[expr,_momentumComponent],Return[fail["OpenLorentzIndex","A scalar amplitude contains an uncontracted Lorentz component."]]];
 rules=KeyValueMap[ParseScalarExpression[#1]->ParseScalarExpression[#2]&,Lookup[record,"model_coupling_dict",<||>]];
 If[AnyTrue[Flatten[List@@@rules],FailureQ],Return[fail["AmplitudeCouplings","Invalid coupling definition."]]];
 Do[new=expr/.rules;If[new===expr,Break[]];expr=new,{Length[rules]+1}];
 If[!FreeQ[expr,Alternatives@@(First/@rules)],Return[fail["AmplitudeCouplings","Cyclic or unresolved model coupling."]]];
 Expand[expr]
],"AmplitudeImport"];
