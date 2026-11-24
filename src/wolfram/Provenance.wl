(* Source identity is separate from numerical/physics acceptance. *)
packageSourceSnapshot[]:=Module[{root=toolRoot[],files},
 files=Join[FileNames["*.wl",FileNameJoin[{root,"src","wolfram"}]],FileNames["*.wls",FileNameJoin[{root,"src","wolfram"}]],
  helperSourceFiles[],FileNames["*.jl",FileNameJoin[{root,"src","julia"}]],
  Select[FileNames["*",FileNameJoin[{root,"packages","opiter"}],Infinity],FileType[#]===File&],
  Select[FileNameJoin[{root,#}]&/@{"PacletInfo.wl","src/Kernel/init.m","src/wolfram/run-example.wls","packages/assets-manifest.json","packages/build.sh","packages/Project.toml","packages/Manifest.toml"},FileExistsQ]];
 Association[Table[StringTrim[StringDrop[f,StringLength[root]],"/"|"\\"]->toolHash[f],{f,Sort[files]}]]
];
loadedSourceCurrentQ[]:=TrueQ[$loadedWolframSources===Association[Table[FileNameTake[f]->toolHash[f],{f,Sort[FileNames["*.wl",DirectoryName[$packageFile]]]}]]];
externalSourceSnapshot[config_]:=Module[{files={},directories,feamgen},
 directories=Select[Lookup[config,{"FeynRulesDirectory","ABC4EFTDirectory"},{}],StringQ[#]&&DirectoryQ[#]&];
 files=Flatten[Table[FileNames[pattern,dir,Infinity],{dir,directories},{pattern,{"*.m","*.wl","*.jl","Project.toml","Manifest.toml"}}]];
 feamgen=Lookup[config,"FeAmGenSourceDirectory",None];
 If[StringQ[feamgen]&&DirectoryQ[feamgen],files=Join[files,FileNames["*.jl",FileNameJoin[{feamgen,"src"}],Infinity],Select[FileNameJoin[{feamgen,#}]&/@{"Project.toml","Manifest.toml"},FileExistsQ]]];
 files=Union[files,Select[Lookup[config,{"SunsetHeader","FORMExecutable","WolframExecutable","PythonExecutable","JuliaLauncher","QGRAFExecutable"},{}],StringQ[#]&&FileExistsQ[#]&]];
 Association[Table[f->toolHash[f],{f,Sort[files]}]]
];
