(* FeAmGen/QGRAF worker for the sole independent-action workflow. *)
ClearAll[generateOffshellTask];
generateOffshellTask[theory_Association,inputCard_Association,output_String,task_Association,vertexOrder_Integer,config_Association]:=Module[
 {root=ExpandFileName[output],dir,card,launcher,qgraf,python,helper,scripts,shim,shimText,environment,job,selected,proc,ampdir,
  identity,marker,previous,outputs,dependencies,prior,archive,policy,response},
 dir=FileNameJoin[{root,"generated",task["id"]<>"_V"<>ToString[vertexOrder]}];
 If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
 {launcher,qgraf,python}=Lookup[config,{"JuliaLauncher","QGRAFExecutable","PythonExecutable"},Missing[]];
 If[!AllTrue[{launcher,qgraf,python},StringQ[#]&&FileExistsQ[#]&],Return[fail["GeneratorToolchain","Declare JuliaLauncher, QGRAFExecutable and PythonExecutable."]]];
 helper=FileNameJoin[{toolRoot[],"src","python","entry.py"}];scripts=FileNameJoin[{toolRoot[],"src","julia"}];
 card=inputCard;
 AssociateTo[card,"fm2_zero_momentum_compression"->Lookup[config,"ZeroMomentumCompression",card["fm2_zero_momentum_compression"]]];
 policy=<|"schema"->"feammatch2.graph-policy.v1","matching_object"->Lookup[task,"matching_object","1LPI"],"light_fields"->card["fm2_light_fields"],
  "auxiliary_external_fields"->Lookup[card,"fm2_auxiliary_external_fields",{}],
  "external_only_fields"->Lookup[card,"fm2_external_only_fields",{}],
  "tadpole_free"->card["fm2_tadpole_free"],"zero_boundary_vertices"->card["fm2_zero_boundary_vertices"],"max_zero_vertices"->card["fm2_max_zero_vertices"],
  "compress_zero_momentum"->TrueQ[card["fm2_zero_momentum_compression"]],"zero_external_momenta"->(task["momentum_order"]===0&&TrueQ[Lookup[config,"ZeroExternalForPotentialTasks",True]]),"derivative_order"->task["momentum_order"]|>;
 If[TrueQ[policy["compress_zero_momentum"]]&&(!TrueQ[policy["zero_external_momenta"]]||task["momentum_order"]>0),Return[fail["GraphCompressionPolicy","Compression is allowed only for zero-external-momentum potential tasks."]]];
 AssociateTo[card,"fm2_graph_policy"->policy];
 If[task["momentum_order"]===0&&TrueQ[Lookup[config,"ZeroExternalForPotentialTasks",True]],
  AssociateTo[card,"momentum_symmetry"->Table[{"K"<>ToString[i],"0"},{i,task["legs"]}]]];
 dependencies=Join[{FileNameJoin[{DirectoryName[$packageFile],"Generation.wl"}],helper,launcher,qgraf},
  FileNames["*.jl",scripts],helperSourceFiles[],{python}];
 dependencies=Join[dependencies,Select[FileNameJoin[{DirectoryName[launcher],#}]&/@{"Project.toml","Manifest.toml"},FileExistsQ],FileNames["*.py",FileNameJoin[{root,"models",card["model_name"]}]]];
 identity=IntegerString[Hash[{theory["Identity"],card,Lookup[config,"PythonIdentity",<||>],Lookup[config,"ExternalSourceIdentity",None],FileHash[#,"SHA256"]&/@dependencies},"SHA256"],16,64];
 marker=FileNameJoin[{dir,"generation.json"}];outputs={"amplitudes.json","graphs-selected.json","graph-decisions.jsonl","zero-momentum-groups.json","graph-selection.request.json","graph-selection.response.json","qgraf.request.json","qgraf.response.json"};
 If[FileExistsQ[marker],previous=Import[marker,"RawJSON"];
  If[Lookup[previous,"input_sha256",""]===identity&&And@@Table[FileExistsQ[FileNameJoin[{dir,f}]]&&Lookup[Lookup[previous,"output_sha256",<||>],f,""]===IntegerString[FileHash[FileNameJoin[{dir,f}],"SHA256"],16,64],{f,outputs}],
   Return[<|"Stage"->"amplitudes_generated","Directory"->dir,"Graphs"->previous["graphs"],"Task"->task["id"],"VertexOrder"->vertexOrder,"Cache"->"content_verified_hit"|>]]];
 prior=Select[FileNames["*",dir],DirectoryQ[#]&&(StringEndsQ[#,"_amplitudes"]||StringEndsQ[#,"_visuals"])&];
 If[prior=!={},archive=FileNameJoin[{dir,"previous-generations",CreateUUID[]}];CreateDirectory[archive,CreateIntermediateDirectories->True];
  Do[RenameDirectory[path,FileNameJoin[{archive,FileNameTake[path]}]],{path,prior}]];
 AssociateTo[card,"fm2_helper_task_id"->(task["id"]<>"-V"<>ToString[vertexOrder]<>"-"<>CreateUUID[])];
 (* YAML.jl rejects literal tab indentation even for JSON flow documents. *)
 Export[FileNameJoin[{dir,"process.json"}],StringReplace[ExportString[card,"RawJSON"],"\t"->"  "],"Text"];
 shim=FileNameJoin[{dir,"qgraf"}];
 shimText="#!"<>python<>"\nimport sys,json,hashlib\nfrom pathlib import Path\nsys.path.insert(0,"<>ExportString[DirectoryName[helper],"RawJSON"]<>")\nimport entry\np=Path('model.qgraf')\nr={'schema':'feammatch2.helper-request.v1','task_id':"<>ExportString[card["fm2_helper_task_id"]<>"-qgraf","RawJSON"]<>",'action':'qgraf-run','directory':str(Path.cwd()),'executable':"<>ExportString[qgraf,"RawJSON"]<>",'response_file':str(Path('qgraf.response.json').resolve()),'inputs':[{'path':str(p.resolve()),'sha256':entry.sha(p)}]}\nPath('qgraf.request.json').write_text(json.dumps(r))\nsys.argv=['entry','--request','qgraf.request.json']\nraise SystemExit(entry.main())\n";
 Export[shim,StringReplace[shimText,"\\/"->"/"],"Text"];job=runExternal[{"/bin/chmod","+x",shim},dir,"qgraf-shim"];If[FailureQ[job],Return[job]];
 environment={"QGRAF"->shim,"FEAMMATCH2_REAL_QGRAF"->qgraf,"FEAMMATCH2_PYTHON"->python,"FEAMMATCH2_GRAPH_HELPER"->helper};
 Print["FeAmGen ",task["id"]," vertex order ",vertexOrder];
 job=runExternal[{launcher,FileNameJoin[{scripts,"generate_offshell.jl"}],FileNameJoin[{dir,"process.json"}],FileNameJoin[{root,"models"}]},dir,"generation",environment];If[FailureQ[job],Return[job]];
 Do[response=validateHelperResponse[FileNameJoin[{dir,stem<>".request.json"}],FileNameJoin[{dir,stem<>".response.json"}]];If[FailureQ[response],Return[response]],{stem,{"qgraf","graph-selection"}}];
 selected=Import[FileNameJoin[{dir,"graphs-selected.json"}],"RawJSON"];
 If[selected==={},Export[FileNameJoin[{dir,"amplitudes.json"}],<|"schema"->"feammatch2.feamgen.v1","amplitudes"->{}|>,"RawJSON"],
  proc=StringRiffle[Join[Take[Join[card["incoming"],card["outgoing"]],1],{"TO"},Drop[Join[card["incoming"],card["outgoing"]],1],{ToString[task["loops"]]<>"Loop"}],"_"]<>"_amplitudes";
  ampdir=FileNameJoin[{dir,proc}];job=runExternal[{launcher,FileNameJoin[{scripts,"export.jl"}],ampdir,FileNameJoin[{dir,"amplitudes.json"}]},dir,"amplitude-export"];If[FailureQ[job],Return[job]]];
 Export[marker,<|"input_sha256"->identity,"graphs"->Length[selected],"output_sha256"->Association[Table[f->IntegerString[FileHash[FileNameJoin[{dir,f}],"SHA256"],16,64],{f,outputs}]]|>,"RawJSON"];
 <|"Stage"->"amplitudes_generated","Directory"->dir,"Graphs"->Length[selected],"Task"->task["id"],"VertexOrder"->vertexOrder|>
];
