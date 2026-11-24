(* Authoritative configuration and preflight: no Python is needed to inspect dependencies. *)
ClearAll[toolRoot,toolHash,findTool,resolveToolchain,toolProcess,helperSourceFiles,runPythonHelper];
toolRoot[]:=DirectoryName[$packageFile,3];
toolHash[path_String]:=IntegerString[FileHash[path,"SHA256"],16,64];
findTool[names_List]:=Module[{dirs,paths},
 dirs=StringSplit[Replace[Environment["PATH"],Except[_String]->""],If[$OperatingSystem==="Windows",";",":"]];
 paths=Flatten[Table[FileNameJoin[{dir,name}],{name,names},{dir,dirs}]];
 SelectFirst[paths,FileExistsQ[#]&&FileType[#]===File&,Missing["NotFound"]]
];
resolveToolchain[configuration_Association]:=Module[{config=configuration,env,commands,value,p,local},
 env=<|"WolframExecutable"->"FEAMMATCH2_WOLFRAM","FORMExecutable"->"FEAMMATCH2_FORM","PythonExecutable"->"FEAMMATCH2_PYTHON",
 "JuliaLauncher"->"FEAMMATCH2_JULIA_LAUNCHER","QGRAFExecutable"->"FEAMMATCH2_QGRAF","FeynRulesDirectory"->"FEYNRULES_DIRECTORY","ABC4EFTDirectory"->"FEAMMATCH2_ABC4EFT","SunsetHeader"->"FEAMMATCH2_SUNSET"|>;
 KeyValueMap[If[!KeyExistsQ[config,#1]&&StringQ[Environment[#2]]&&Environment[#2]=!="",AssociateTo[config,#1->Environment[#2]]]&,env];
 local=<|"FORMExecutable"->{"bin","form"},"QGRAFExecutable"->{"bin","qgraf"},
  "JuliaLauncher"->{"run-julia.sh"},"FeynRulesDirectory"->{"FeynRules"},"ABC4EFTDirectory"->{"ABC4EFT"}|>;
 KeyValueMap[Function[{key,parts},With[{path=FileNameJoin[Join[{toolRoot[],"packages"},parts]]},
   If[!KeyExistsQ[config,key]&&FileExistsQ[path],AssociateTo[config,key->path]]]],local];
 config=DefaultToolchain[config];
 commands=<|"WolframExecutable"->{"WolframKernel","MathKernel","math"},"FORMExecutable"->{"form","tform"},
 "PythonExecutable"->{"python3"},"JuliaLauncher"->{"run-julia.sh"},"QGRAFExecutable"->{"qgraf"}|>;
 KeyValueMap[If[!KeyExistsQ[config,#1],p=findTool[#2];If[StringQ[p],AssociateTo[config,#1->p]]]&,commands];
 If[!KeyExistsQ[config,"WolframExecutable"],p=SelectFirst[{FileNameJoin[{$InstallationDirectory,"MacOS","WolframKernel"}],
 "/Applications/Wolfram.app/Contents/MacOS/WolframKernel","/Applications/Mathematica.app/Contents/MacOS/WolframKernel"},FileExistsQ,Missing[]];
 If[StringQ[p],AssociateTo[config,"WolframExecutable"->p]]];
 Do[value=Lookup[config,key,Missing[]];If[StringQ[value]&&value=!="",
  If[MemberQ[Keys[commands],key]&&!StringContainsQ[value,{"/","\\"}],p=findTool[{value}];If[StringQ[p],value=p]];
  AssociateTo[config,key->ExpandFileName[value]]],{key,Join[Keys[commands],{"FeynRulesDirectory","ABC4EFTDirectory","OPITeRDirectory","SunsetHeader"}]}];
 config
];
toolProcess[command_List]:=Quiet[Check[TimeConstrained[RunProcess[command],60,$Failed],$Failed]];
helperSourceFiles[]:=Sort[FileNames["*.py",FileNameJoin[{toolRoot[],"src","python"}]]];

validateHelperResponse[path_String,responsePath_String]:=Module[{request,response,valid,artifacts,required,sources},
 request=Quiet[Check[Import[path,"RawJSON"],$Failed]];response=Quiet[Check[Import[responsePath,"RawJSON"],$Failed]];
 If[!AssociationQ[request]||!AssociationQ[response],Return[fail["HelperResponse","Missing or malformed helper request/response."]]];
 valid=Lookup[request,"schema",""]==="feammatch2.helper-request.v1"&&StringQ[Lookup[request,"task_id",Missing[]]]&&
 Lookup[response,"schema",""]==="feammatch2.helper-response.v1"&&Lookup[response,"task_id",""]===request["task_id"]&&
 Lookup[response,"action",""]===request["action"]&&Lookup[response,"request_sha256",""]===toolHash[path]&&Lookup[response,"status",""]==="ok";
 artifacts=Lookup[response,"artifacts",Missing[]];
 valid=valid&&ListQ[artifacts]&&AllTrue[artifacts,AssociationQ[#]&&StringQ[Lookup[#,"path",Missing[]]]&&
 FileExistsQ[#["path"]]&&Lookup[#,"sha256",""]===toolHash[#["path"]]&];
 required=Switch[request["action"],"capabilities",{},"normalize-ufo",{"ufo-metadata.json","ufo-normalization.json"},
 "select-graphs",{"graphs-selected.json","zero-momentum-groups.json","graph-decisions.jsonl"},"qgraf-run",{"model.qgraf"},_,{"unsupported action"}];
 If[ListQ[artifacts],valid=valid&&Complement[required,FileNameTake/@Lookup[artifacts,"path",{}]]==={}];
 sources=Association[Map[FileNameTake[#]->toolHash[#]&,helperSourceFiles[]]];
 valid=valid&&Lookup[response,"helper_sources",<||>]===sources;
 If[!TrueQ[valid],Return[Failure["HelperContract",<|"MessageTemplate"->"Helper response identity, status or artifacts failed validation.","Request"->path,"Response"->responsePath|>]]];
 response
];
runPythonHelper[action_String,payload_Association,inputs_List,directory_String,stem_String,config_Association]:=Module[
 {request,path,responsePath,helper,job,response,artifacts,valid},
 If[!DirectoryQ[directory],CreateDirectory[directory,CreateIntermediateDirectories->True]];
 path=FileNameJoin[{ExpandFileName[directory],stem<>".request.json"}];responsePath=FileNameJoin[{ExpandFileName[directory],stem<>".response.json"}];
 request=Join[payload,<|"schema"->"feammatch2.helper-request.v1","task_id"->(stem<>"-"<>CreateUUID[]),"action"->action,
 "response_file"->responsePath,"inputs"->Map[<|"path"->ExpandFileName[#],"sha256"->toolHash[#]|>&,inputs]|>];
 Export[path,request,"RawJSON"];
 helper=FileNameJoin[{toolRoot[],"src","python","entry.py"}];
 job=runExternal[{config["PythonExecutable"],helper,"--request",path},ExpandFileName[directory],stem];
 If[FailureQ[job],Return[job]];
 response=validateHelperResponse[path,responsePath];If[FailureQ[response],Return[response]];
 If[!AssociationQ[response],Return[fail["HelperResponse","Missing or malformed helper response."]]];
 valid=Lookup[response,"schema",""]==="feammatch2.helper-response.v1"&&Lookup[response,"task_id",""]===request["task_id"]&&
 Lookup[response,"action",""]===action&&Lookup[response,"request_sha256",""]===toolHash[path]&&Lookup[response,"status",""]==="ok";
 artifacts=Lookup[response,"artifacts",Missing[]];
 valid=valid&&ListQ[artifacts]&&AllTrue[artifacts,AssociationQ[#]&&StringQ[Lookup[#,"path",Missing[]]]&&
 FileExistsQ[#["path"]]&&Lookup[#,"sha256",""]===toolHash[#["path"]]&];
 If[!TrueQ[valid],Return[Failure["HelperContract",<|"MessageTemplate"->"Helper response identity, status or artifacts failed validation.","Request"->path,"Response"->responsePath|>]]];
 Join[response,<|"RequestFile"->path,"ResponseFile"->responsePath|>]
];

Options[CheckToolchain]={"Mode"->"full","Probe"->True,"RequireABC4EFT"->False};
CheckToolchain[configuration_Association:<||>,OptionsPattern[]]:=Module[
 {config=resolveToolchain[configuration],mode=OptionValue["Mode"],probe=TrueQ[OptionValue["Probe"]],checks=<||>,required,
 keys={"WolframExecutable","FORMExecutable","PythonExecutable","JuliaLauncher","QGRAFExecutable"},path,ok,message,r,files,missing,assets,expected,damaged,
 directory,capability,version,commands,report,rawSettings,maxN,maxE,tables},
 If[!MemberQ[{"full","plan"},mode],Return[fail["ToolchainMode","Mode must be full or plan."]]];
 required=If[mode==="plan",{"WolframExecutable"},Join[keys,{"FeynRulesDirectory","OPITeRDirectory","SunsetHeader"}]];
 If[TrueQ[OptionValue["RequireABC4EFT"]],AppendTo[required,"ABC4EFTDirectory"]];
 Do[path=Lookup[config,key,Missing[]];ok=StringQ[path]&&FileExistsQ[path];message=If[ok,"available","missing; configure "<>key];
  If[ok&&MemberQ[keys,key],ok=FileType[path]===File;
   If[ok&&$OperatingSystem=!="Windows",r=toolProcess[{"/bin/test","-x",path}];ok=AssociationQ[r]&&r["ExitCode"]===0];
   message=If[ok,"executable available","not an executable file"]];
  If[ok&&key==="FeynRulesDirectory",ok=DirectoryQ[path]&&FileExistsQ[FileNameJoin[{path,"FeynRules.m"}]];message=If[ok,"FeynRules.m available","missing FeynRules.m"]];
  If[ok&&key==="ABC4EFTDirectory",ok=DirectoryQ[path]&&FileExistsQ[FileNameJoin[{path,"Kernel","init.m"}]];message=If[ok,"ABC4EFT init.m available; physical-basis queries run before diagram generation","missing Kernel/init.m"]];
  If[ok&&key==="SunsetHeader",ok=FileType[path]===File;message=If[ok,"sunset header available","must be a file"]];
  If[ok&&key==="OPITeRDirectory",
   files={"opiter.frm","tenred.prc","tenredisym.prc","PrtCanonicalize.prc","gamma_algebra.h","opitersettings.dat"};
   missing=Select[files,!FileExistsQ[FileNameJoin[{path,#}]]&];
   If[!MemberQ[missing,"opitersettings.dat"],rawSettings=Import[FileNameJoin[{path,"opitersettings.dat"}],"Text"];
    maxN=StringCases[rawSettings,RegularExpression["#define\\s+MaxN\\s+\"([0-9]+)\""]->"$1"];
    maxE=StringCases[rawSettings,RegularExpression["#define\\s+MaxE\\s+\"([0-9]+)\""]->"$1"];
    If[Length[maxN]===1&&Length[maxE]===1,maxN=FromDigits[First[maxN]];maxE=FromDigits[First[maxE]];
     tables=Join[("projectors/Prt_table"<>ToString[#]<>".h"&)/@Range[2,maxN,2],
       ("grammatrix/invgram_table"<>ToString[#]<>".frm"&)/@Range[2,maxE],{"projectors/projectors_2_16.frm"},If[maxN>=16,{"projectors/projectors_18_20.frm"},{}]];
     missing=Join[missing,Select[tables,!FileExistsQ[FileNameJoin[{path,#}]]&]],AppendTo[missing,"valid MaxN/MaxE"]]];
   ok=missing==={};message=If[ok,"OPITeR tables available","missing: "<>StringRiffle[missing,", "]]];
  AssociateTo[checks,key-><|"passed"->TrueQ[ok],"status"->If[TrueQ[ok],"passed","failed"],"path"->If[StringQ[path],path,Null],"message"->message|>],{key,required}];
 If[mode==="full",assets=Quiet[Check[Import[FileNameJoin[{toolRoot[],"packages","assets-manifest.json"}],"RawJSON"],$Failed]];expected=<||>;
  If[AssociationQ[assets],
   If[config["OPITeRDirectory"]===DefaultToolchain[]["OPITeRDirectory"],expected=Association[KeyValueMap[FileNameJoin[{toolRoot[],"packages","opiter",#1}]->#2&,assets["opiter"]["files"]]]];
   If[config["SunsetHeader"]===DefaultToolchain[]["SunsetHeader"],AssociateTo[expected,DefaultToolchain[]["SunsetHeader"]->assets["sunset"]["sha256"]]]];
  damaged=If[AssociationQ[assets],Select[Keys[expected],!FileExistsQ[#]||toolHash[#]=!=expected[#]&],{"asset manifest"}];
  AssociateTo[checks,"BundledAssetIntegrity"-><|"passed"->(damaged==={}),"status"->If[damaged==={},"passed","failed"],"files_checked"->Length[expected],"message"->If[damaged==={},"bundled hashes verified",StringRiffle[damaged,", "]]|>]];
 If[mode==="full"&&probe,
  commands=<|"FORMRuntime"->{"FORMExecutable",{"-v"},None},
    "JuliaPackages"->{"JuliaLauncher",{"--startup-file=no","-e","using FeAmGen, JLD2, YAML; println(\"FEAMMATCH2_PACKAGES_OK\"); println(\"FEAMMATCH2_FEAMGEN_ROOT=\", dirname(dirname(pathof(FeAmGen))))"},"FEAMMATCH2_PACKAGES_OK"}|>;
  KeyValueMap[Function[{name,spec},
   If[TrueQ[checks[spec[[1]]]["passed"]],r=toolProcess[Prepend[spec[[2]],config[spec[[1]]]]];
    ok=AssociationQ[r]&&r["ExitCode"]===0&&(spec[[3]]===None||StringContainsQ[r["StandardOutput"],spec[[3]]]);
    If[name==="JuliaPackages"&&TrueQ[ok],With[{locations=StringCases[r["StandardOutput"],"FEAMMATCH2_FEAMGEN_ROOT="~~p:Except["\n"|"\r"]..:>p]},If[Length[locations]===1,AssociateTo[config,"FeAmGenSourceDirectory"->First[locations]]]]];
    AssociateTo[checks,name-><|"passed"->TrueQ[ok],"status"->If[TrueQ[ok],"passed","failed"],"message"->If[AssociationQ[r],r["StandardOutput"]<>r["StandardError"],"process failed or timed out"]|>],
    AssociateTo[checks,name-><|"passed"->False,"status"->"not_run","message"->"executable unavailable"|>]]],commands];
  If[TrueQ[checks["PythonExecutable"]["passed"]],
   r=toolProcess[{config["PythonExecutable"],"-c","import ast,json,hashlib,fractions,subprocess,pathlib,sys; assert sys.version_info >= (3,9); print(sys.version)"}];
   ok=AssociationQ[r]&&r["ExitCode"]===0;version=If[AssociationQ[r],r["StandardOutput"]<>r["StandardError"],"Python probe failed"];
   If[ok,directory=CreateDirectory[];capability=runPythonHelper["capabilities",<||>,{},directory,"capabilities",config];
    ok=!FailureQ[capability];If[ok,AssociateTo[config,"PythonIdentity"-><|"version"->version,"capabilities"->capability["statistics"]|>]];
    DeleteDirectory[directory,DeleteContents->True]];
   AssociateTo[checks,"PythonCapabilities"-><|"passed"->TrueQ[ok],"status"->If[TrueQ[ok],"passed","failed"],"message"->version|>],
   AssociateTo[checks,"PythonCapabilities"-><|"passed"->False,"status"->"not_run","message"->"Python unavailable; other checks still completed"|>]]];
 report=<|"schema"->"feammatch2.toolchain-check.v2","implementation"->"native_Wolfram","passed"->And@@Lookup[Values[checks],"passed"],
 "mode"->mode,"runtime_probes_requested"->probe,"config"->config,"checks"->checks|>;
 If[!TrueQ[report["passed"]],Failure["MissingDependencies",<|"MessageTemplate"->"Dependency checks failed; Report contains every completed check.","Report"->report|>],report]
];
