(* Local action/channel operations shared by the single matching pipeline. *)
ClearAll[theoryPolePart,transformTheoryChannels,actionTheoryChannels];
theoryPolePart[expr_,eps_,depth_:2]:=Sum[Coefficient[Expand[expr],eps,-i]eps^-i,{i,1,depth}];
transformTheoryChannels[channels_,function_]:=Association[KeyValueMap[#1->Join[#2,<|"Expression"->function[#2]|>]&,channels]];
actionTheoryChannels[eft_,action_,template_]:=Catch[Module[{local=scalarActionTheory[eft,action]},
 If[FailureQ[local],Throw[local,"FM2TheoryStage"]];
 transformTheoryChannels[template,Function[ch,canonicalMomentumPolynomial[ScalarLocalVertex[local,ch["ExternalFields"],ch["Momenta"]],ch["Momenta"]]]]
],"FM2TheoryStage"];

