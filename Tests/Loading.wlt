(* Loading Hypergraph makes no network call, and rule application still reads MultiReplace's options on first use.
   Run from the repository root with TestReport["Tests/Loading.wlt"]. The first test starts a kernel of its own; the
   other two fetch MultiReplace, so they need the internet or a cached copy. *)

$hypergraphDirectory = FileNameJoin[{
    ParentDirectory[DirectoryName[If[StringQ[$TestFileName] && $TestFileName =!= "", $TestFileName, $InputFileName]]],
    "Hypergraph"
}];

(* A fresh kernel whose user base directory is new, so that it has no resource cached, loads Hypergraph with the
   internet off and saves whether Hypergraph loaded, the messages the loading issued and the packages of Wolfram's
   resource and cloud systems it loaded, which this gives, or the process if it saved nothing. The license files are
   copied in. *)
coldOfflineLoad[] := Module[
    {userBase = CreateDirectory[], licensing = FileNameJoin[{$UserBaseDirectory, "Licensing"}], file, code, process, result},
    If[DirectoryQ[licensing], CopyDirectory[licensing, FileNameJoin[{userBase, "Licensing"}]]];
    file = FileNameJoin[{userBase, "result.wl"}];
    code = StringJoin[
        "Block[{$AllowInternet = False}, PacletDirectoryLoad[", ToString[$hypergraphDirectory, InputForm], "]; ",
        "With[{before = $Packages}, Needs[\"WolframInstitute`Hypergraph`\"]; ",
        "Put[{MemberQ[$Packages, \"WolframInstitute`Hypergraph`\"], $MessageList, ",
        "Select[Complement[$Packages, before], StringStartsQ[\"ResourceSystemClient`\" | \"FunctionResource`\" | \"CloudObject`\"]]}, ",
        ToString[file, InputForm], "]]]; Exit[]"
    ];
    process = RunProcess[{First[$CommandLine], "-noprompt", "-run", code},
        ProcessEnvironment -> Join[Association[GetEnvironment[]], <|"WOLFRAM_USERBASE" -> userBase|>]];
    result = If[FileExistsQ[file], Get[file], process];
    DeleteDirectory[userBase, DeleteContents -> True];
    result
]

VerificationTest[
    coldOfflineLoad[],
    {True, {}, {}},
    TestID -> "ColdOfflineLoad"
]

PacletDirectoryLoad[$hypergraphDirectory];
Needs["WolframInstitute`Hypergraph`"];

VerificationTest[
    SubsetQ[Keys[Options[HypergraphRule]], Keys[Options[ResourceFunction["MultiReplace"]]]],
    True,
    TestID -> "MultiReplaceOptions"
]

VerificationTest[
    Length[HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]][Hypergraph[{{a, b}, {b, c}}]]],
    4,
    TestID -> "RuleApplication"
]
