(* Atomic result artifacts and content identities. *)
ClearAll[writeResultArtifact];
writeResultArtifact[directory_,name_,value_]:=Module[{path=FileNameJoin[{directory,name<>".wl"}],temporary},
 If[!DirectoryQ[directory],CreateDirectory[directory,CreateIntermediateDirectories->True]];
 temporary=path<>".tmp-"<>CreateUUID[];Put[value,temporary];RenameFile[temporary,path,OverwriteTarget->True];
 Export[FileNameJoin[{directory,name<>".identity.json"}],<|"sha256"->IntegerString[FileHash[path,"SHA256"],16,64]|>,"RawJSON"];path];

(* Timings are observations of this execution; nested events overlap. *)
SetAttributes[timedStage,HoldRest];
timedStage[name_String,expression_]:=Module[{started=AbsoluteTime[],value},
 value=expression;
 If[ListQ[$performanceEvents],AppendTo[$performanceEvents,<|"Name"->name,"StartedSeconds"->N[started-$performanceStarted],"ElapsedSeconds"->N[AbsoluteTime[]-started]|>]];
 value
];
