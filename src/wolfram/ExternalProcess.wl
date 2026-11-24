(* External process execution; physics lives in the stage modules. *)
ClearAll[runExternal];
runExternal[command_List,directory_String,stem_String,environment_:{}]:=Module[{r,inherited},
 inherited=Select[(#->Environment[#])&/@{"PATH","HOME","TMPDIR","JULIA_DEPOT_PATH","JULIA_LOAD_PATH","FEAMMATCH2_JULIA"},StringQ[Last[#]]&];
 r=Check[RunProcess[command,All,ProcessDirectory->directory,ProcessEnvironment->Normal[Join[Association[inherited],Association[environment]]]],$Failed];
 If[r===$Failed,Return[fail["ExternalProcess","Could not start external program."]]];
 Export[FileNameJoin[{directory,stem<>".stdout.log"}],r["StandardOutput"],"Text"];
 Export[FileNameJoin[{directory,stem<>".stderr.log"}],r["StandardError"],"Text"];
 If[r["ExitCode"]!=0,Return[Failure["ExternalProcess",<|"MessageTemplate"->"External program returned a nonzero status.","Command"->command,"Directory"->directory,"ExitCode"->r["ExitCode"]|>]]];r
];

