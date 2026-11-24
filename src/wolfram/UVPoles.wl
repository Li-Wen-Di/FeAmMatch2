(* Local MSbar action poles, h=1/(16 Pi^2), d=4-2 epsilon.
   Quartic tensor contractions: Osborn/Stergiou 1707.06165v7 Eq.(3.6).
   The mass/cubic extension is implemented by the same background derivatives;
   it requires separate diagram checks. No higher-dimensional EFT claim. *)
ScalarUVPoles[potential_,fields_List,e_Symbol]:=Module[
 {v=Expand[potential],n=Length[fields],hessian,massMatrix,interactionHessian,oneLoop,c1,c21,c22,quartic,gamma,z21,beta2},
 If[n===0||!PolynomialQ[v,fields]||Max[Total[First[#]]&/@CoefficientRules[v,fields]]>4,
  Return[fail["RenormalizableScalarPotential","Supply a polynomial potential of total field degree at most four."]]];
 hessian=Table[D[v,fields[[i]],fields[[j]]],{i,n},{j,n}];
 massMatrix=hessian/.Thread[fields->0];
 interactionHessian=hessian-massMatrix;
 (* Section 3.C: sum the tadpole and bubble UV residues before forming
    the local vertex. The vacuum term carries no external legs. *)
 oneLoop=<|"Vacuum"->Expand[Tr[massMatrix.massMatrix]/4],
   "Tadpole"->Expand[Tr[massMatrix.interactionHessian]/2],
   "Bubble"->Expand[Tr[interactionHessian.interactionHessian]/4]|>;
 c1=Expand[Total[Values[oneLoop]]];
 c21=Expand[-Sum[hessian[[i,j]]D[v,fields[[i]],fields[[k]],fields[[l]]]D[v,fields[[j]],fields[[k]],fields[[l]]],{i,n},{j,n},{k,n},{l,n}]/8];
 c22=Expand[Sum[hessian[[i,j]]D[c1,fields[[i]],fields[[j]]],{i,n},{j,n}]/4];
 quartic=Table[D[v,fields[[i]],fields[[j]],fields[[k]],fields[[l]]],{i,n},{j,n},{k,n},{l,n}];
 gamma=Table[Expand[Sum[quartic[[i,k,l,m]]quartic[[j,k,l,m]],{k,n},{l,n},{m,n}]/12],{i,n},{j,n}];
 z21=-gamma/2;
 beta2=Expand[4 c21+Sum[gamma[[i,j]]fields[[j]]D[v,fields[[i]]],{i,n},{j,n}]];
 <|"Schema"->"feammatch2.scalar-uv-poles.v1","Scheme"->"MSbar",
  "PotentialOneLoop"->c1/e,"PotentialTwoLoop"->c22/e^2+c21/e,
  "PotentialOneLoopByTopology"->Map[#/e&,oneLoop],
  "PotentialDoublePole"->c22,"PotentialSinglePole"->c21,
  "KineticTwoLoop"->z21/e,"AnomalousDimensionTwoLoop"->gamma,
  "BetaPotentialOneLoop"->2 c1,"BetaPotentialTwoLoop"->beta2,
  "Scope"->"Renormalizable real-scalar MSbar action. No finite tadpole shift or higher-dimensional EFT counterterms.",
  "Reference"->"https://arxiv.org/abs/1707.06165v7"|>
];
