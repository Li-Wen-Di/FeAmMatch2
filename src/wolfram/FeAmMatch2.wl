(* Mathematica stage adapters with bundled FORM reduction assets. *)
BeginPackage["FeAmMatch2`"];
ScalarJet::usage="ScalarJet[field,{mu,...}] is an inert commuting derivative of a real scalar; repeated Lorentz labels are contracted.";
LoadUVModel::usage="LoadUVModel[file or association] loads an independent scalar UV action and its field/parameter metadata.";
LoadEFTModel::usage="LoadEFTModel[file or association] loads a user-defined EFT Green action and independent Wilson coefficients.";
CreateMatchingTask::usage="CreateMatchingTask[uv,eft,request] freezes independent theories, the light-field map, and explicit matching precision.";
ExtractScalarTerms::usage="ExtractScalarTerms[theory] derives local scalar monomials and derivative contractions from the input action.";
ScalarLocalVertex::usage="ScalarLocalVertex[theory,externalFields,momenta] differentiates the local scalar action, including all identical-field factorials.";
BuildOperatorResponses::usage="BuildOperatorResponses[eft] derives the exact off-shell response matrix from the supplied EFT action.";
CheckLinearDependence::usage="CheckLinearDependence[eft] reports exact rank, null space, and coefficient identities of the supplied basis.";
SolveExactSystem::usage="SolveExactSystem[matrix,rhs,variables] solves an exact overdetermined response system and checks every equation.";
ExportTheoryUFO::usage="ExportTheoryUFO[theory,directory,config] exports an independent scalar action with FeynRules and validates all scalar momentum structures.";
GenerateTheoryTask::usage="GenerateTheoryTask[theory,export,task,vertexOrder,directory,config] generates a data-driven off-shell process with FeAmGen/QGRAF.";
TotalScalarDerivative::usage="TotalScalarDerivative[localExpression,index,fields] differentiates a scalar jet polynomial by the product rule.";
SubstituteScalarFields::usage="SubstituteScalarFields[action,fieldRules,fields,dimension:Infinity] applies a local field redefinition to the entire scalar action. An integer dimension projects polynomial products during substitution.";
MatchTheoryTree::usage="MatchTheoryTree[task] eliminates canonical massive scalar fields and solves the supplied Green responses without a process-specific projection.";
DeriveBasisConversion::usage="DeriveBasisConversion[green,physical,request] builds the scalar local-field-redefinition problem; it does not read user reduction rules.";
ApplyBasisConversion::usage="ApplyBasisConversion[conversion,coefficientRules] solves the weighted full-action field transformation and returns the physical action and coefficients.";
CanonicalizeFields::usage="CanonicalizeFields[theory,action,h,loops] derives the kinetic matrix and its inverse square root through the requested loop order.";
ResolvePhysicalBasis::usage="ResolvePhysicalBasis[eft,request,directory,config] resolves an explicit physical action or runs ABC4EFT in an isolated kernel for real scalar singlets.";
IntegrateTheoryTask::usage="IntegrateTheoryTask[theory,export,generation,task,config] integrates actual off-shell scalar amplitudes with all independent external momenta preserved.";
ComputeTheoryAmplitudes::usage="ComputeTheoryAmplitudes[task,loop,directory,config] generates and integrates response-driven UV all-hard channels.";
ProjectTheoryAmplitudes::usage="ProjectTheoryAmplitudes[eft,channelResults,knownAction] solves the full exact local response system and rejects unsupported residual structures.";
MatchTheoryOneLoop::usage="MatchTheoryOneLoop[task,directory,config] computes UV hard matching and scalar Hessian local pole directions.";
BuildAuxiliaryGreenBasis::usage="BuildAuxiliaryGreenBasis[task,dimension] derives an independent auxiliary local tree basis needed for higher precision; original user coefficients remain identified separately.";
PrepareInsertionTheory::usage="PrepareInsertionTheory[theory,localCorrection] constructs marked perturbative scalar vertices, including tadpoles and quadratic vertices, from a local action.";
MatchTheoryTwoLoop::usage="MatchTheoryTwoLoop[task,directory,config] assembles UV double-hard, counterterm insertions and local pole directions.";
FieldDescriptor::usage="FieldDescriptor[association] validates extensible field metadata; it does not enable non-scalar execution.";
RunFORMTask::usage="RunFORMTask[source,config] runs one FORM task in an isolated directory, returning logs, status and provenance.";
ReduceSunset::usage="ReduceSunset[indices,masses,config] invokes the existing exact-D FORM sunset recurrences and closes tadpole boundaries.";
ReduceVacuumTensor::usage="ReduceVacuumTensor[polynomial,loops,external,config] invokes OPITeR for vacuum tensor reduction; numerator uses SP[a,b].";
VacuumIntegral::usage="VacuumIntegral[a,b,c,m1,m2,m3] or VacuumIntegral[a,m] denotes a reduced scalar integral, not its Laurent value.";
SP::usage="SP[a,b] is a symmetric scalar product in stage interchange data.";
Den::usage="Den[momentum,mass,0] denotes one scalar propagator 1/(momentum^2-mass^2+i0).";
VacuumDenominator::usage="VacuumDenominator[route,mass,power] retains a vacuum propagator; routes 1,2,3 are q1,q2,q1+q2.";
HardExpand::usage="HardExpand[expression,loops,external,heavy,light,order,config] executes the all-hard Taylor expansion in FORM.";
ReduceHardTensor::usage="ReduceHardTensor[expression,loops,external,config] applies OPITeR to a hard amplitude while preserving its vacuum denominators.";
NormalizeVacuumFamilies::usage="NormalizeVacuumFamilies[expression,loops,external,config] executes mixed-mass partial fractions, numerator cancellation and vacuum family mapping in FORM.";
PlanLaurentPrecision::usage="PlanLaurentPrecision[expression,integralDefinitions,epsilon,targetOrder] derives factor-wise Laurent requests, including poles in exact reduction coefficients and other factors.";
ReduceVacuumIntegrals::usage="ReduceVacuumIntegrals[expression,config] reduces sunset, tadpole and negative-index boundary sectors in exact D using FORM and OPITeR.";
ScalarUVPoles::usage="ScalarUVPoles[potential,fields,epsilon] returns MSbar one/two-loop action counterterms for a nonderivative renormalizable real-scalar potential; finite tadpole changes remain separate.";
EvaluateHardAmplitude::usage="EvaluateHardAmplitude[expression,loops,external,heavy,light,order,config] chains FORM hard expansion, OPITeR tensors, FORM family normalization and exact-D IBP.";
LaurentVacuum::usage="LaurentVacuum[expression,epsilon,scale,order,config] checks exact-D integral precision and executes the common MSbar Laurent combination in FORM.";
Clausen2::usage="Clausen2[theta] denotes Im[PolyLog[2,Exp[I theta]]] for real theta.";
ParseScalarExpression::usage="ParseScalarExpression[string] parses a restricted arithmetic and Den/SP language without evaluating external source code.";
ImportScalarAmplitude::usage="ImportScalarAmplitude[record] constructs an exact scalar FeAmGen integrand from lossless JSON metadata.";
PlanMatching::usage="PlanMatching[task] derives scalar process and vertex-order tasks, retaining the conservative dimension-six dependency closure.";
RunMatching::usage="RunMatching[task,directory,config] runs the independent UV/Green action workflow and exports both Green and physical results.";
DefaultToolchain::usage="DefaultToolchain[config] supplies relocatable bundled OPITeR and sunset paths; explicit overrides take priority.";
CheckToolchain::usage="CheckToolchain[config] resolves tool paths and checks external dependencies before generation. Options: Mode -> full/plan, Probe -> True/False.";
Begin["`Private`"];
$packageFile=$InputFileName;
$loadedWolframSources=Association[Table[FileNameTake[f]->IntegerString[FileHash[f,"SHA256"],16,64],{f,Sort[FileNames["*.wl",DirectoryName[$packageFile]]]}]];

DefaultToolchain[config_Association:<||>]:=Join[<|
 "OPITeRDirectory"->FileNameJoin[{DirectoryName[$packageFile,3],"packages","opiter","opiter"}],
 "SunsetHeader"->FileNameJoin[{DirectoryName[$packageFile,3],"packages","sunset","proc.h"}]|>,config];
ClearAll[fail, formText, readExpression];
fail[tag_,message_]:=Failure[tag,<|"MessageTemplate"->message|>];

FieldDescriptor[data_Association]:=Module[{spin,name,role},
 name=Lookup[data,"name",""];role=Lookup[data,"role",""];
 spin=Lookup[data,"spin","0"];
 If[!StringQ[name]||!StringMatchQ[name,RegularExpression["[A-Za-z][A-Za-z0-9]*"]],Return[fail["FieldName","Use an ASCII field identifier."]]];
 If[!MemberQ[{"heavy","light"},role],Return[fail["FieldRole","Declare a heavy or light field."]]];
 If[!MemberQ[{0,1/2,1,"0","1/2","1"},spin],Return[fail["Spin","Metadata supports spin 0, 1/2 or 1."]]];
 Join[<|"spin"->"0","statistics"->"commuting","self_conjugate"->True,
        "index_spaces"->{},"representations"-><||>|>,
      If[MemberQ[{1/2,"1/2"},spin],<|"statistics"->"anticommuting"|>,<||>],data]
];

formText[x_Integer]:=ToString[x,InputForm];
formText[x_Rational]:="("<>ToString[Numerator[x]]<>"/"<>ToString[Denominator[x]]<>")";
formText[x_Complex]:="("<>formText[Re[x]]<>"+fmImag*"<>formText[Im[x]]<>")";
formText[x_Symbol]:=SymbolName[x];
formText[x_Plus]:="("<>StringRiffle[formText/@(List@@x),"+"]<>")";
formText[x_Times]:="("<>StringRiffle[formText/@(List@@x),"*"]<>")";
formText[Power[x_,n_Integer]]:="("<>formText[x]<>")^("<>ToString[n]<>")";
formText[SP[a_,b_]]:="("<>formText[a]<>"."<>formText[b]<>")";
formText[VacuumDenominator[r_,m_,n_]]:="fmVD("<>StringRiffle[formText/@{r,m,n},","]<>")";
formText[x_VacuumIntegral]:="F("<>StringRiffle[formText/@(List@@x),","]<>")";
formText[_]:=$Failed;

RunFORMTask[source_String,config_Association]:=Module[
 {exe,root,job,assets,result,manifest,paths,name,path,cacheActive,key,cached,started=AbsoluteTime[],output,valid},
 exe=Lookup[config,"FORMExecutable",Missing["FORMExecutable"]];
 root=Lookup[config,"WorkingDirectory",Missing["WorkingDirectory"]];
 If[!StringQ[exe]||!FileExistsQ[exe]||!StringQ[root],Return[fail["Configuration","Declare FORMExecutable and WorkingDirectory."]]];
 If[!DirectoryQ[root],CreateDirectory[root,CreateIntermediateDirectories->True]];
 assets=Lookup[config,"Assets",<||>];
 If[!AssociationQ[assets],Return[fail["Assets","Assets must map safe local names to files or directories."]]];
 paths=<||>;
 Do[
  If[!StringQ[name]||StringContainsQ[name,".."]||StringContainsQ[name,"/"]||StringContainsQ[name,"\\"],Return[fail["Assets","Asset names must be local basenames."]]];
  path=assets[name];
  If[!StringQ[path]||!FileExistsQ[path],Return[fail["Assets","A declared external asset does not exist."]]];
  AssociateTo[paths,name-><|"source"->ExpandFileName[path],"sha256"->If[DirectoryQ[path],
    Association[Map[Function[f,StringDrop[f,StringLength[ExpandFileName[path]]+1]->IntegerString[FileHash[f,"SHA256"],16,64]],FileNames["*",ExpandFileName[path],Infinity]//Select[#,FileType[#]===File&]&]],
    IntegerString[FileHash[path,"SHA256"],16,64]]|>],
  {name,Keys[assets]}];
 (* Reuse only exact FORM source/tool/asset requests from this RunMatching call.
    The original output and job source must still match their recorded hashes.
    No persistent reference results or model-dependent integral tables are used. *)
 cacheActive=TrueQ[Lookup[config,"ReuseFORMTasks",True]]&&AssociationQ[$formTaskCache]&&StringContainsQ[source,"#write <result.out>"];
 key=IntegerString[Hash[{source,ExpandFileName[exe],FileHash[exe,"SHA256"],paths,TrueQ[Lookup[config,"RetainAssetCopies",True]]},"SHA256"],16,64];
 If[cacheActive&&KeyExistsQ[$formTaskCache,key],cached=$formTaskCache[key];
  output=FileNameJoin[{cached["Manifest"]["directory"],"result.out"}];
  valid=FileExistsQ[output]&&FileExistsQ[FileNameJoin[{cached["Manifest"]["directory"],"job.frm"}]]&&
   IntegerString[FileHash[output,"SHA256"],16,64]===cached["OutputSHA256"]&&
   IntegerString[FileHash[FileNameJoin[{cached["Manifest"]["directory"],"job.frm"}],"SHA256"],16,64]===cached["Manifest"]["source_sha256"];
  If[TrueQ[valid],If[AssociationQ[$formExecutionStats],AssociateTo[$formExecutionStats,"Hits"->($formExecutionStats["Hits"]+1)]];
   Return[Join[cached["Manifest"],<|"Cache"->"same_run_verified_hit","OutputSHA256"->cached["OutputSHA256"]|>]]]];
 job=FileNameJoin[{ExpandFileName[root],"form-"<>CreateUUID[]}];CreateDirectory[job];
 Do[path=assets[name];If[DirectoryQ[path],CopyDirectory[path,FileNameJoin[{job,name}]],CopyFile[path,FileNameJoin[{job,name}]]],{name,Keys[assets]}];
 Export[FileNameJoin[{job,"job.frm"}],source,"Text"];
 result=RunProcess[{ExpandFileName[exe],"job.frm"},All,ProcessDirectory->job];
 If[!AssociationQ[result],Return[fail["Process","FORM failed to start."]]];
 Export[FileNameJoin[{job,"stdout.log"}],result["StandardOutput"],"Text"];
 Export[FileNameJoin[{job,"stderr.log"}],result["StandardError"],"Text"];
 manifest=<|"schema"->"feammatch2.form-stage.v1","directory"->job,
   "adapter_sha256"->IntegerString[Hash[Import[$packageFile,"Text"]<>StringJoin[Import[#,"Text"]&/@FileNames["*.wl",DirectoryName[$packageFile]]],"SHA256"],16,64],
   "exit_code"->result["ExitCode"],"source_sha256"->IntegerString[FileHash[FileNameJoin[{job,"job.frm"}],"SHA256"],16,64],
   "executable"->ExpandFileName[exe],"executable_sha256"->IntegerString[FileHash[exe,"SHA256"],16,64],"assets"->paths|>;
 AssociateTo[manifest,"asset_copies_retained"->TrueQ[Lookup[config,"RetainAssetCopies",True]]];
 Export[FileNameJoin[{job,"manifest.json"}],manifest,"RawJSON"];
 If[!TrueQ[Lookup[config,"RetainAssetCopies",True]],Do[
   path=FileNameJoin[{job,name}];
   If[DirectoryQ[path],DeleteDirectory[path,DeleteContents->True],DeleteFile[path]],{name,Keys[assets]}]];
 If[result["ExitCode"]!=0,Return[Failure["FORMFailed",<|"MessageTemplate"->"FORM returned a nonzero exit code.","Directory"->job,"ExitCode"->result["ExitCode"]|>]]];
 If[AssociationQ[$formExecutionStats],AssociateTo[$formExecutionStats,{"Executed"->($formExecutionStats["Executed"]+1),"ExecutionSeconds"->($formExecutionStats["ExecutionSeconds"]+N[AbsoluteTime[]-started])}]];
 output=FileNameJoin[{job,"result.out"}];
 If[cacheActive&&FileExistsQ[output],AssociateTo[$formTaskCache,key-><|"Manifest"->manifest,"OutputSHA256"->IntegerString[FileHash[output,"SHA256"],16,64]|>]];
 manifest
];

readExpression[path_]:=Module[{text,held},
 If[!FileExistsQ[path],Return[fail["Output","FORM did not write the declared output."]]];
 text=StringReplace[Import[path,"Text"],{"\n"->"","\r"->"",";"->""}];
 held=Block[{$Context="FeAmMatch2`FORM`",$ContextPath={"System`"}},Quiet[Check[ToExpression[text,InputForm,HoldComplete],$Failed]]];
 If[held===$Failed,Return[fail["OutputSyntax","FORM expression could not be read."]]];
 (* The only evaluated input here is our own FORM-generated output. *)
 ReleaseHold[held/.{FeAmMatch2`FORM`rat[x_]:>x,FeAmMatch2`FORM`F->VacuumIntegral,
   FeAmMatch2`FORM`rat->Divide,FeAmMatch2`FORM`ddts->SP,FeAmMatch2`FORM`fmVD->VacuumDenominator,FeAmMatch2`FORM`fmImag->I,Dot->SP}]
];

ReduceSunset[idx_List,masses_List,config_Association]:=Module[
 {a=idx,m=masses,perms,permutation,proc,lambda,header,symbols,rules,back,mtxt,source,job,expr,nmax,cfg},
 If[Length[a]!=3||Length[m]!=3||!AllTrue[a,IntegerQ[#]&&#>=0&],Return[fail["Indices","Supply three nonnegative integer powers and three masses."]]];
 If[And@@(TrueQ[PossibleZeroQ[#]]&/@m),Return[<|"Expression"->0,"Stage"->"scaleless_vacuum","PhysicalCoefficients"->Null|>]];
 If[AnyTrue[m,NumericQ[#]&&(!TrueQ[Im[#]==0]||TrueQ[#<0])&],Return[fail["Masses","Numeric masses must be real and nonnegative."]]];
 perms=Permutations[Range[3]];proc="GeneralTwoLoopRecursion";
 permutation=SelectFirst[perms,TrueQ[PossibleZeroQ[m[[#[[1]]]]]]&&TrueQ[PossibleZeroQ[m[[#[[2]]]]-m[[#[[3]]]]]]&&!TrueQ[PossibleZeroQ[m[[#[[2]]]]]]&,Missing[]];
 If[!MissingQ[permutation],proc="OneEqZeroCollinearTwoLoopRecursion",
  lambda=Expand[m[[1]]^4+m[[2]]^4+m[[3]]^4-2(m[[1]]^2 m[[2]]^2+m[[1]]^2 m[[3]]^2+m[[2]]^2 m[[3]]^2)];
  If[TrueQ[PossibleZeroQ[lambda]],
   permutation=SelectFirst[perms,!AnyTrue[m[[#]],TrueQ[PossibleZeroQ[#]]&]&&TrueQ[PossibleZeroQ[m[[#[[3]]]]-m[[#[[1]]]]-m[[#[[2]]]]]]&,Missing[]];
   If[MissingQ[permutation],Return[fail["DegenerateMasses","Degenerate mass relation is outside the supplied recurrences."]]];
   proc="NonEqZeroCollinearTwoLoopRecursion",
   permutation={1,2,3}]];
 a=a[[permutation]];m=m[[permutation]];
 header=Lookup[DefaultToolchain[config],"SunsetHeader",Missing[]];
 If[!StringQ[header]||!FileExistsQ[header],Return[fail["SunsetHeader","Bundled sunset header is missing, or SunsetHeader override is invalid."]]];
 symbols=DeleteDuplicates[Cases[m,_Symbol,Infinity]];
 rules=MapIndexed[#1->Symbol["fmMass"<>ToString[First[#2]]]&,symbols];
 back=rules/.Rule[x_,y_]:>Rule[Symbol["FeAmMatch2`FORM`"<>SymbolName[y]],x];
 mtxt=formText/@(m/.rules);
 If[!FreeQ[mtxt,$Failed],Return[fail["MassSyntax","Masses must be exact numbers or polynomial expressions in mass symbols."]]];
 nmax=Max[4,Total[a]+1];
 source="#include sunset-proc.h\n#define N \""<>ToString[nmax]<>"\"\nCF F,InvDelta;\nS a1,a2,a3,m1,m2,m3,D,invdelta,eps,inveps"<>
  If[Length[rules]>0,","<>StringRiffle[SymbolName/@(Last/@rules),","],""]<>";\nset Pos2ToN:2,...,"<>ToString[Max[4,nmax]]<>";\nset Pos3ToN:3,...,"<>ToString[Max[4,nmax]]<>";\nOff Statistics;\nL Result=F("<>
  StringRiffle[Join[ToString/@a,mtxt],","]<>");\n"<>
  If[Min[a]>0,"#call "<>proc<>"\n",""]<>
  "#call OneLoopRecursion\nid F(0,m1?)=0;\n.sort\nFormat Mathematica;\n#write <result.out> \"%E\",Result\n.end\n";
 cfg=Join[config,<|"Assets"-><|"sunset-proc.h"->header|>|>];job=RunFORMTask[source,cfg];
 If[FailureQ[job],Return[job]];
 expr=readExpression[FileNameJoin[{job["directory"],"result.out"}]];
 If[FailureQ[expr],Return[expr]];expr=expr/.back;
 If[!FreeQ[expr,VacuumIntegral[x_Integer,y_Integer,z_Integer,___]/;{x,y,z}!={1,1,1}],Return[fail["UnreducedIntegral","A non-master sunset remained after reduction."]]];
 <|"Expression"->expr,"Stage"->"exact_D_vacuum_reduced","Recurrence"->proc,
   "Job"->job,"PhysicalCoefficients"->Null|>
];

ReduceVacuumTensor[polynomial_,loops_List,external_List,config_Association]:=Module[
 {opiter,all,symbols,rules,back,terms,parts,slots,remaining,powers,n,pair,lp,ep,rank=0,strings={},str,source,cfg,job,expr},
 If[Length[loops]<1||Length[loops]>2||Length[DeleteDuplicates[Join[loops,external]]]!=Length[Join[loops,external]],Return[fail["Momenta","Declare one or two distinct loop momenta and disjoint external momenta."]]];
 all=Join[loops,external];
 If[!AllTrue[all,Head[#]===Symbol&],Return[fail["Momenta","Momentum names must be symbols."]]];
 parts=DeleteDuplicates[Cases[polynomial,_SP,{0,Infinity}]];
 If[parts==={},
  If[!StringQ[formText[polynomial]],Return[fail["TensorCoefficient","Unsupported scalar coefficient."]]];
  Return[<|"Expression"->polynomial,"Stage"->"exact_D_scalar_numerator","Rank"->0,"PhysicalCoefficients"->Null|>]];
 If[!AllTrue[parts,Length[#]===2&&AllTrue[List@@#,MemberQ[all,#]&]&]||!PolynomialQ[polynomial,parts],Return[fail["TensorPolynomial","Use a polynomial in scalar products of the declared momenta."]]];
 opiter=Lookup[DefaultToolchain[config],"OPITeRDirectory",Missing[]];
 If[!StringQ[opiter]||!FileExistsQ[FileNameJoin[{opiter,"opiter.frm"}]],Return[fail["OPITeR","Declare a directory containing opiter.frm."]]];
 symbols=DeleteDuplicates[Cases[{polynomial,all},x_Symbol/;x=!=D,Infinity]];
 rules=Join[MapIndexed[#1->Symbol["fmV"<>ToString[First[#2]]]&,all],
    MapIndexed[#1->Symbol["fmC"<>ToString[First[#2]]]&,Complement[symbols,all,{SP}]]];
 back=rules/.Rule[x_,y_]:>Rule[Symbol["FeAmMatch2`FORM`"<>SymbolName[y]],x];
 terms=If[Head[Expand[polynomial]]===Plus,List@@Expand[polynomial],{Expand[polynomial]}];
 Do[
  slots={};remaining=term;
  Do[
   powers=Exponent[term,pair];
   If[!IntegerQ[powers]||powers<0,Return[fail["TensorPolynomial","Tensor input must be a polynomial in scalar products."]]];
   If[Xor[MemberQ[loops,pair[[1]]],MemberQ[loops,pair[[2]]]],
    lp=If[MemberQ[loops,pair[[1]]],pair[[1]],pair[[2]]];ep=If[lp===pair[[1]],pair[[2]],pair[[1]]];
    If[!MemberQ[external,ep],Return[fail["TensorMomentum","Undeclared external momentum."]]];
    slots=Join[slots,ConstantArray[{lp,ep},powers]];remaining=remaining/pair^powers],
   {pair,Cases[term,_SP,{0,Infinity}]//DeleteDuplicates}];
  n=Length[slots];rank=Max[rank,n];
  If[n>20,Return[fail["TensorRank","Installed OPITeR projector tables support rank at most 20."]]];
  str=formText[remaining/.rules];If[!StringQ[str],Return[fail["TensorSyntax","Unsupported tensor coefficient."]]];
  AppendTo[strings,str<>"*proj("<>If[n==0,"1",StringRiffle[MapIndexed[formText[#1[[2]]/.rules]<>"(mu"<>ToString[First[#2]]<>")"&,slots],"*"]]<>")*ext()*loop("<>StringRiffle[formText/@(loops/.rules),","]<>")"<>
    StringJoin[MapIndexed["*"<>formText[#1[[1]]/.rules]<>"(mu"<>ToString[First[#2]]<>")"&,slots]]],
  {term,terms}];
 If[rank===0,Return[<|"Expression"->polynomial,"Stage"->"exact_D_scalar_numerator","Rank"->0,"PhysicalCoefficients"->Null|>]];
 source="#: IncDir opiter\n#include- opiter.frm\nOff Statistics;\nVectors "<>StringRiffle[formText/@(all/.rules),","]<>";\nSymbols fmImag;\n"<>
   If[Length[Complement[symbols,all,{SP}]]>0,"Symbols "<>StringRiffle[SymbolName/@(Complement[symbols,all,{SP}]/.rules),","]<>";\n",""]<>
   "L Result="<>StringRiffle[strings,"+" ]<>";\n#call opiter\n#call symmetrise\n"<>
   StringJoin[Flatten[Table["id ddts("<>formText[a/.rules]<>","<>formText[b/.rules]<>")="<>formText[SP[a,b]/.rules]<>";\n",{a,loops},{b,loops}]]]<>
   "id ext()=1;\nid loop(?a)=1;\n.sort\nFormat Mathematica;\n#write <result.out> \"%E\",Result\n.end\n";
 cfg=Join[config,<|"Assets"-><|"opiter"->opiter|>|>];job=RunFORMTask[source,cfg];
 If[FailureQ[job],Return[job]];expr=readExpression[FileNameJoin[{job["directory"],"result.out"}]];
 If[FailureQ[expr],Return[expr]];expr=expr/.back/.SP[a_,b_]:>SP@@Sort[{a,b}];
 <|"Expression"->expr,"Stage"->"exact_D_vacuum_tensor_reduced","Rank"->rank,"Job"->job,"PhysicalCoefficients"->Null|>
];

Get[FileNameJoin[{DirectoryName[$packageFile],"HardExpansion.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"Precision.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"VacuumReduction.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"UVPoles.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"MasterIntegrals.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"AmplitudeImport.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"Toolchain.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"ExternalProcess.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"Artifacts.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"ChannelAlgebra.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"Generation.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"TheoryInput.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"OperatorResponse.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"TheoryExport.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"MatchingPlanner.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"ScalarJetAlgebra.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"BasisConversion.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"PhysicalBasis.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"TheoryAmplitudes.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"DiagramCounterterms.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"TheoryRunner.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"AuxiliaryGreen.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"InsertionTheory.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"TheoryRenormalization.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"HardMSbar.wl"}]];
Get[FileNameJoin[{DirectoryName[$packageFile],"Provenance.wl"}]];
End[];
EndPackage[];
