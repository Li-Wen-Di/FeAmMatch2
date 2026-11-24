$HistoryLength=0;root=Environment["FM2_ROOT"];out=Environment["FM2_OUTPUT"];
Get[root<>"/src/wolfram/FeAmMatch2.wl"];If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
checks=<||>;check[name_,condition_]:=(AssociateTo[checks,name->TrueQ[condition]];Print[name," : ",checks[name]]);
eft=LoadEFTModel[root<>"/example/example1/input/EFTGreen.fr"];f=First[eft["FieldSymbols"]];
physical=LoadEFTModel[<|"Schema"->"feammatch2.theory.v2","Name"->"IndependentPhysical","Fields"->eft["Fields"],"Parameters"-><|pm-><|"Dimension"->2|>,pl-><|"Dimension"->0|>,pc-><|"Dimension"->-2|>,eft["Fields"][[1]]["Mass"]-><|"Dimension"->1|>|>,"WilsonCoefficients"->{pm,pl,pc},"Lagrangian"->ScalarJet[f,{mu}]^2/2-pm f^2/2-pl f^4/24-pc f^6/720|>];
green=<|"a2"->u0+h u1+h^2 u2,"Z"->1+h z1+h^2 z2,"a4"->l0+h l1+h^2 l2,"a6"->v0+h v1+h^2 v2,"b"->b0+h b1+h^2 b2,"c"->c0+h c1+h^2 c2|>;
rules=Thread[eft["WilsonCoefficients"]->Lookup[green,{"a2","Z","c","a4","b","a6"}]];
conversion=DeriveBasisConversion[eft,physical,<|"Loops"->2,"LoopSymbol"->h|>];result=ApplyBasisConversion[conversion,rules];Put[result,out<>"/two-loop-conversion.wl"];
check["full_action_two_loop_conversion",AssociationQ[result]];
(* Independent analytic single-field map: normalize by powers of Z, then
   remove the two redundant dimension-six structures. No retired API. *)
{am,al,av,ab,ac}={green["a2"]/green["Z"],green["a4"]/green["Z"]^2,green["a6"]/green["Z"]^3,green["b"]/green["Z"]^2,green["c"]/green["Z"]};
expected=Normal[Series[{am+am^2 ac,al-am ab+4am al ac,av-5al ab+10al^2 ac},{h,0,2}]];
check["independent_analytic_field_map",AssociationQ[result]&&And@@Thread[Factor/@(({pm,pl,pc}/.result["Rules"])-expected)==0]];
check["all_local_responses_reconstructed",AssociationQ[result]&&result["Residuals"]==={}];
ibpAction=With[{field=f},eft["Lagrangian"]/.HoldPattern[field^3 ScalarJet[field,{a_,a_}]]:>-3field^2 ScalarJet[field,{a}]^2];
ibp=LoadEFTModel[Join[eft,<|"Lagrangian"->ibpAction|>]];
ibpResult=ApplyBasisConversion[DeriveBasisConversion[ibp,physical,<|"Loops"->2,"LoopSymbol"->h|>],rules];
check["alternative_green_IBP_representative",ibpAction=!=eft["Lagrangian"]&&AssociationQ[ibpResult]&&And@@Thread[Factor/@((Last/@result["Rules"])-(Last/@ibpResult["Rules"]))==0]];
target2=LoadEFTModel[Join[physical,<|"Lagrangian"->(physical["Lagrangian"]+pc f^6/720-pc f^6/6)|>]];
r2=ApplyBasisConversion[DeriveBasisConversion[eft,target2,<|"Loops"->2,"LoopSymbol"->h|>],rules];
check["different_physical_normalization",AssociationQ[r2]&&Factor[(pc/.r2["Rules"])-(pc/.result["Rules"])/120]===0];
fields={<|"Name"->"x","Symbol"->x,"Role"->"light","Mass"->mx|>,<|"Name"->"y","Symbol"->y,"Role"->"light","Mass"->my|>};
stub=<|"FieldSymbols"->{x,y}|>;kinetic=(1+h a11+h^2 b11)ScalarJet[x,{mu}]^2/2+(1+h a22+h^2 b22)ScalarJet[y,{mu}]^2/2+(h a12+h^2 b12)ScalarJet[x,{mu}]ScalarJet[y,{mu}];
normal=CanonicalizeFields[stub,kinetic,h,2];
check["noncommuting_two_field_kinetic_matrix",AssociationQ[normal]&&Flatten[normal["Residuals"]]==={0,0,0,0}];
check["mixed_one_loop_square_retained",AssociationQ[normal]&&Factor[Coefficient[normal["Matrix"][[1,2]],h,2]-(3(a11 a12+a12 a22)/8-b12/2)]===0];

(* Inverse of x -> x+alpha x^2, with a quartic potential: the dimension-five
   transformation squared contributes at dimension six and must cancel. *)
odd=LoadEFTModel[<|"Schema"->"feammatch2.theory.v2","Name"->"OddGreen","Fields"->{<|"Name"->"x","Symbol"->x,"Role"->"light","Mass"->mx|>},"Parameters"-><|mx-><|"Dimension"->1|>,m2-><|"Dimension"->2|>,lam-><|"Dimension"->0|>,c5-><|"Dimension"->-1|>,c6-><|"Dimension"->-2|>,d5-><|"Dimension"->-1|>,d6-><|"Dimension"->-2|>|>,"WilsonCoefficients"->{m2,lam,c5,c6,d5,d6},"Lagrangian"->ScalarJet[x,{mu}]^2/2-m2 x^2/2-lam x^4/24-c5 x^5/120-c6 x^6/720+d5 x ScalarJet[x,{mu}]^2+d6 x^2 ScalarJet[x,{mu}]^2|>];
oddPhysical=LoadEFTModel[Join[odd,<|"Lagrangian"->(odd["Lagrangian"]/.{d5->0,d6->0}),"WilsonCoefficients"->{m2,lam,c5,c6}|>]];
or=ApplyBasisConversion[DeriveBasisConversion[odd,oddPhysical,<|"Loops"->0|>],{m2->0,lam->lambda,c5->20lambda alpha,c6->180lambda alpha^2,d5->2alpha,d6->2alpha^2}];Put[or,out<>"/dimension-five-square.wl"];
check["dimension_five_field_square",AssociationQ[or]&&Factor[c5/.or["Rules"]]===0&&Factor[c6/.or["Rules"]]===0&&Factor[(lam/.or["Rules"])-lambda]===0];

(* A malformed contraction must leave a loop immediately; it cannot be turned
   into an empty channel and reported as a successful calculation. *)
bad=LoadEFTModel[Join[eft,<|"Lagrangian"->(eft["Lagrangian"]+f ScalarJet[f,{mu}])|>]];
check["invalid_jet_contraction_propagates",FailureQ[bad]];
badConversion=ApplyBasisConversion[conversion,Thread[eft["WilsonCoefficients"]->{u0,0,0,l0,0,v0}]];
check["zero_kinetic_fails_closed",FailureQ[badConversion]];
Export[out<>"/report.json",<|"passed"->And@@Values[checks],"checks"->checks|>,"RawJSON"];Exit[If[And@@Values[checks],0,1]];
