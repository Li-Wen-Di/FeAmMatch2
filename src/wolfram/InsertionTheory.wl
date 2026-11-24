(* A local action insertion is represented explicitly in the UFO. The marker
   counts insertions independently of QGRAF's per-vertex enumeration order. *)
PrepareInsertionTheory[theory_Association,correction_]:=Catch[Module[
 {local,terms,parameters=theory["Parameters"],lag=0,quadratic=0,spurion,marker,coefficient,identity,index=0,rules={},rows={},operator,prepared,fields=theory["Fields"]},
 local=scalarActionTheory[theory,correction];If[FailureQ[local],Throw[local,"FM2TheoryStage"]];
 terms=Select[local["Terms"],#["FieldDegree"]>0&&#["Coefficient"]=!=0&];identity=StringTake[theoryIdentity[{theory["Identity"],correction}],12];
 marker=Symbol["FeAmMatch2`Insert"<>identity<>"`marker"];AssociateTo[parameters,marker-><|"Dimension"->0,"Value"->1/97|>];
 Do[index++;coefficient=Symbol["FeAmMatch2`Insert"<>identity<>"`delta"<>ToString[index]];
  operator=Times@@(If[#[[2]]==={},#[[1]],ScalarJet[#[[1]],#[[2]]]]&/@row["Jets"]);
  AssociateTo[parameters,coefficient-><|"Dimension"->4-row["OperatorDimension"],"Value"->1/101|>];
  AppendTo[rules,coefficient->row["Coefficient"]];If[row["FieldDegree"]===2,quadratic+=coefficient operator,lag+=coefficient operator];AppendTo[rows,<|"Coefficient"->coefficient,"Operator"->operator,"Dimension"->row["OperatorDimension"]|>],{row,terms}];
 spurion=If[quadratic===0,None,Symbol["FeAmMatch2`Insert"<>identity<>"`quadraticSource"]];
 If[spurion=!=None,AppendTo[fields,<|"Name"->"quadraticSource","Symbol"->spurion,"Role"->"auxiliary","Mass"->0,"Spin"->0,"Real"->True,"Auxiliary"->True,"ExternalOnly"->True,"BookkeepingDimension"->0|>]];
 prepared=scalarActionTheory[Join[theory,<|"Fields"->fields,"FieldSymbols"->Lookup[fields,"Symbol"],"Parameters"->parameters,"Identity"->theoryIdentity[{theory["Identity"],lag,quadratic,marker}]|>],theory["Lagrangian"]+marker(lag+If[spurion===None,0,spurion quadratic])];
 If[FailureQ[prepared],Throw[prepared,"FM2TheoryStage"]];
 <|"Theory"->prepared,"Marker"->marker,"QuadraticSpurion"->spurion,"CoefficientRules"->rules,"Vertices"->rows,"LocalInsertion"->(lag+quadratic),
  "InsertionCounting"->"Exactly one marker power is selected in the imported integrand before integration."|>
],"FM2TheoryStage"];

(* A double-hard Taylor expansion contains only nonnegative powers of light
   masses/momenta. Skip a whole source branch only when its common prefactor
   already exceeds the requested weight, all other vertices have nonnegative
   lower bounds, and heavy masses carry zero weight. Unknown => keep. *)
hardParameterWeightLowerBound[expression_,weights_Association]:=Module[{s=Unique["sourceWeight"],x,bound},
 x=expression/.KeyValueMap[#1->s^#2 #1&,weights];
 bound=laurentValuation[x,s];If[FailureQ[bound],Missing["UnknownWeight"],bound]
];
hardSourceWeightDecision[source_,vertices_List,theory_,process_,config_]:=Module[{weights,budget,bound,others},
 If[!TrueQ[Lookup[config,"PruneHardSourceWeights",True]],Return[<|"Skip"->False|>]];
 weights=Lookup[config,"HardLightWeights",<||>];budget=Lookup[config,"HardWeightBudget",Missing["NoBudget"]];
 If[!IntegerQ[budget]||!AssociationQ[weights]||!AllTrue[Values[weights],IntegerQ[#]&&#>=0&]||
   AnyTrue[Select[theory["Fields"],#["Role"]==="heavy"&],Lookup[weights,#["Mass"],0]=!=0&],Return[<|"Skip"->False|>]];
 bound=hardParameterWeightLowerBound[source,weights];
 If[!TrueQ[bound>budget],Return[<|"Skip"->False|>]];
 others=hardParameterWeightLowerBound[#["Response"],weights]&/@vertices;
 If[!AllTrue[others,TrueQ[#>=0]&],Return[<|"Skip"->False|>]];
 <|"Skip"->True,"SourceWeightLowerBound"->bound,"WeightBudget"->budget,
   "Reason"->"Common source prefactor exceeds the hard Taylor weight; remaining vertices and propagator expansion cannot lower it."|>
];

integrateHeavySourceInsertions[theory_,export_,interaction_,process_,out_,config_]:=Catch[Module[
 {marker=config["InsertionSymbol"],base,vertices,linear,heavy,source,orders,extra,p,generated,result,total=0,records={},cfg,decision},
 base=scalarActionTheory[theory,interaction["Lagrangian"]/.marker->0];If[FailureQ[base],Throw[base,"FM2TheoryStage"]];vertices=localVertexRows[base];
 linear=Coefficient[interaction["Lagrangian"],marker,1];
 heavy=Select[theory["Fields"],#["Role"]==="heavy"&];
 Do[source=D[linear,field["Symbol"]]/.Thread[theory["FieldSymbols"]->0];If[source===0,Continue[]];
  decision=hardSourceWeightDecision[source/field["Mass"]^2,vertices,theory,process,config];
  If[TrueQ[decision["Skip"]],
   AppendTo[records,Join[<|"Channel"->process["EFTFields"],"Order"->0,"Graphs"->0,"Insertion"->"zero-momentum-heavy-source","SourceFactor"->ToString[source/field["Mass"]^2,InputForm]|>,decision]];
   Print["Skip heavy source ",process["id"],": weight ",decision["SourceWeightLowerBound"]," > ",decision["WeightBudget"]];Continue[]];
  extra=Append[process["TheoryFields"],field["Symbol"]];
  orders=DeleteDuplicates[#["VertexOrder"]&/@vertexCombinations[vertices,theory["FieldSymbols"],extra,process["loops"]]];
  p=Join[process,<|"id"->(process["id"]<>"-source-"<>StringTake[theoryIdentity[field["Symbol"]],10]),"TheoryFields"->extra,"legs"->Length[extra],"PhysicalExternalCount"->process["legs"]|>];
  cfg=Join[config,<|"InsertionOrder"->0,"IntegrandPrefactor"->source/field["Mass"]^2|>];
  Do[generated=timedStage["GenerateInsertionTask",GenerateTheoryTask[theory,export,p,order,out,cfg]];If[FailureQ[generated],Throw[generated,"FM2TheoryStage"]];
   Print["Heavy source insertion ",p["id"]," V",order," (",generated["Graphs"]," graphs)"];
   result=timedStage["IntegrateInsertionTask",IntegrateTheoryTask[theory,export,generated,p,cfg]];If[FailureQ[result],Throw[result,"FM2TheoryStage"]];
   writeResultArtifact[generated["Directory"],"independent-source-hard",result];total+=result["LaurentExpression"];
   AppendTo[records,<|"Channel"->process["EFTFields"],"Order"->order,"Directory"->generated["Directory"],"Graphs"->generated["Graphs"],"Insertion"->"zero-momentum-heavy-source","SourceFactor"->ToString[source/field["Mass"]^2,InputForm]|>],{order,orders}],{field,heavy}];
 <|"Expression"->Expand[total],"Records"->records|>
],"FM2TheoryStage"];

ClearAll[localVertexRows,insertionVertexOrders];
localVertexRows[theory_]:=Module[{channels,rows},
 channels=DeleteDuplicates[Sort/@Select[Lookup[theory["Terms"],"Fields"],Length[#]>0&]];
 rows=Table[<|"Fields"->channel,"Degree"->Length[channel],"Response"->canonicalMomentumPolynomial[ScalarLocalVertex[theory,channel,independentExternalMomenta[Length[channel]]],independentExternalMomenta[Length[channel]]]|>,{channel,channels}];Select[rows,#["Response"]=!=0&]
];
singleInsertionOrders[base_List,insertions_List,fields_List,external_List,loops_Integer]:=Module[{orders={},rhs,bounds,counts,half,active=Select[base,#["Degree"]>2&]},
 Do[rhs=2loops+Length[external]-2-(fixed["Degree"]-2);If[rhs<0,Continue[]];bounds=Floor[rhs/(#["Degree"]-2)]&/@active;
  counts=Select[Tuples[Range[0,#]&/@bounds],Function[c,Total[c*(#["Degree"]&/@active-2)]===rhs&&
   (half=Table[Sum[c[[i]]Count[active[[i]]["Fields"],f],{i,Length[active]}]+Count[fixed["Fields"],f]-Count[external,f],{f,fields}];AllTrue[half,IntegerQ[#]&&#>=0&&EvenQ[#]&]&&connectedVertexContentQ[Append[Flatten[MapThread[ConstantArray[#2["Fields"],#1]&,{c,active}],1],fixed["Fields"]],external])]];
  orders=Join[orders,1+Total[#]&/@counts],{fixed,insertions}];Sort[DeleteDuplicates[orders]]
];
integrateQuadraticInsertions[theory_,export_,interaction_,process_,out_,config_]:=Catch[Module[
 {auxiliary,marker=config["InsertionSymbol"],source,base,vertices,quadratic,insertions,orders={},rhs,bounds,counts,half,extra,p,generated,result,total=0,records={}},
 auxiliary=Select[theory["Fields"],TrueQ[Lookup[#,"Auxiliary",False]]&];If[auxiliary==={},Throw[<|"Expression"->0,"Records"->{}|>,"FM2TheoryStage"]];
 If[Length[auxiliary]=!=1,Throw[fail["AuxiliarySource","Only one external quadratic bookkeeping source is expected."],"FM2TheoryStage"]];source=First[auxiliary]["Symbol"];
 base=scalarActionTheory[theory,interaction["Lagrangian"]/.marker->0];vertices=Select[localVertexRows[base],#["Degree"]>2&];
 quadratic=scalarActionTheory[theory,D[Coefficient[interaction["Lagrangian"],marker,1],source]];insertions=localVertexRows[quadratic];
 orders=singleInsertionOrders[vertices,insertions,DeleteCases[theory["FieldSymbols"],source],process["TheoryFields"],process["loops"]];
 extra=Append[process["TheoryFields"],source];p=Join[process,<|"id"->(process["id"]<>"-quadratic"),"TheoryFields"->extra,"legs"->Length[extra],"PhysicalExternalCount"->process["legs"]|>];
 Do[generated=timedStage["GenerateInsertionTask",GenerateTheoryTask[theory,export,p,order,out,config]];If[FailureQ[generated],Throw[generated,"FM2TheoryStage"]];
  Print["Quadratic insertion ",p["id"]," V",order," (",generated["Graphs"]," graphs)"];
  result=timedStage["IntegrateInsertionTask",IntegrateTheoryTask[theory,export,generated,p,config]];If[FailureQ[result],Throw[result,"FM2TheoryStage"]];
  writeResultArtifact[generated["Directory"],"independent-quadratic-hard",result];total+=result["LaurentExpression"];
  AppendTo[records,<|"Channel"->process["EFTFields"],"Order"->order,"Directory"->generated["Directory"],"Graphs"->generated["Graphs"],"Insertion"->"external-zero-momentum-quadratic-source"|>],{order,orders}];
 <|"Expression"->Expand[total],"Records"->records|>
],"FM2TheoryStage"];
insertionVertexOrders[interaction_Association,fields_List,external_List,loops_Integer,marker_Symbol]:=Catch[Module[
 {base,insertions,active,rhs,bounds,counts,half,orders={},fixed},
 base=scalarActionTheory[interaction,interaction["Lagrangian"]/.marker->0];If[FailureQ[base],Throw[base,"FM2TheoryStage"]];
 insertions=scalarActionTheory[interaction,Coefficient[interaction["Lagrangian"],marker,1]];If[FailureQ[insertions],Throw[insertions,"FM2TheoryStage"]];
 active=Select[localVertexRows[base],#["Degree"]>2&];insertions=Select[localVertexRows[insertions],#["Degree"]>2&&AllTrue[#["Fields"],Function[f,!MemberQ[Lookup[Select[interaction["Fields"],TrueQ[Lookup[#,"Auxiliary",False]]&],"Symbol",{}],f]]]&];
 singleInsertionOrders[active,insertions,fields,external,loops]
],"FM2TheoryStage"];
