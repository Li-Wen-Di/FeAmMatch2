$HistoryLength=0;root=Environment["FM2_ROOT"];out=Environment["FM2_OUTPUT"];
Get[root<>"/src/wolfram/FeAmMatch2.wl"];If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
checks=<||>;check[n_,c_]:=(AssociateTo[checks,n->TrueQ[c]];Print[n," : ",checks[n]]);
Do[
 uv=LoadUVModel[root<>"/src/check/fixtures/"<>name<>"/UV.wl"];eft=LoadEFTModel[root<>"/src/check/fixtures/"<>name<>"/EFTGreen.wl"];
 check[name<>"_inputs",AssociationQ[uv]&&AssociationQ[eft]];
 task=CreateMatchingTask[uv,eft,<|"Loops"->2|>];plan=PlanMatching[task];check[name<>"_plan",AssociationQ[plan]];
 tree=MatchTheoryTree[task];Put[tree,out<>"/"<>name<>"-tree.wl"];check[name<>"_tree",AssociationQ[tree]&&tree["Residuals"]==={}];
 sym[s_]:=SelectFirst[Keys[uv["Parameters"]],SymbolName[#]===s&];
 coef[s_]:=SelectFirst[eft["WilsonCoefficients"],SymbolName[#]===s&]/.tree["Rules"];
 If[name==="mixed-components",
  check["mixed_external_channel",AnyTrue[plan["tasks"],Length[DeleteDuplicates[#["EFTFields"]]]===2&]];
  check["mixed_tree_exchange_factors",Factor[coef["cxy"]+2sym["a"]^2/sym["M"]^2]===0&&Factor[coef["d1"]-sym["a"]^2/(2sym["M"]^4)]===0&&Factor[coef["d2"]-sym["a"]^2/sym["M"]^4]===0&&Factor[coef["d3"]-sym["a"]^2/(2sym["M"]^4)]===0],
  check["odd_external_channels",MemberQ[Lookup[plan["tasks"],"legs"],3]&&MemberQ[Lookup[plan["tasks"],"legs"],5]];
  check["odd_tree_exchange_factors",Factor[coef["c5"]+10sym["a"]sym["b"]/sym["M"]^2]===0&&Factor[coef["c6"]+10sym["b"]^2/sym["M"]^2]===0&&Factor[coef["c3"]-sym["g3"]]===0]];
,{name,{"mixed-components","odd-legs"}}];
Export[out<>"/report.json",<|"passed"->And@@Values[checks],"checks"->checks,"scope"->"New input/channel planning and classical matching; no multi-light loop validation claimed."|>,"RawJSON"];Exit[If[And@@Values[checks],0,1]];
