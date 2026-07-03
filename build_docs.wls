#!/usr/bin/env wolframscript
(* Build the Hypergraph paclet documentation from the literate markdown in
   docs/, writing each notebook into the standard paclet layout under
   Documentation/English/, and the paclet-repository submission notebook to
   ResourceDefinition.nb. The markdown is the source of truth; the notebooks
   are generated and gitignored.

   Run with the `wl` CLI:

       wl -f build_docs.wls
*)

(* Pin the front end appearance to Light: a headless session resolves the
   Automatic appearance to Dark, which would invert colors in rasterized
   example output. *)

Quiet @ UsingFrontEnd[CurrentValue[$FrontEnd, LightDark] = "Light"]

pacletDir = DirectoryName[$InputFileName]

$mtnSource = "/Users/swish/src/wolfram/AISkills/MarkdownToNotebook.wl"

PacletDirectoryLoad[pacletDir]

Needs["WolframInstitute`Hypergraph`"]

If[ ! NameQ["WolframInstitute`Hypergraph`Hypergraph"],
    Print["FATAL: WolframInstitute`Hypergraph` failed to load."];
    Exit[1]
]

(* Prefer the local MarkdownToNotebook checkout; fall back to the deployed
   cloud resource when it is absent. *)

mtn = If[ FileExistsQ[$mtnSource],
    Get[$mtnSource];
    MarkdownToNotebook,
    Quiet @ Check[If[! TrueQ[$CloudConnected], CloudConnect[]], Null];
    ResourceFunction["https://www.wolframcloud.com/obj/nikm/DeployedResources/Function/MarkdownToNotebook"]
]

subdir = <|
    "Symbol" -> {"Documentation", "English", "ReferencePages", "Symbols"},
    "Guide" -> {"Documentation", "English", "Guides"},
    "TechNote" -> {"Documentation", "English", "Tutorials"},
    "Overview" -> {"Documentation", "English", "Tutorials"}
|>

frontmatterTemplate[srcFile_String] := Block[{m},
    m = StringCases[Import[srcFile, "Text"],
        StartOfLine ~~ "Template:" ~~ Whitespace ... ~~ v : Except["\n"] .. :> StringTrim[v], 1];
    If[m === {}, Missing[], First[m]]
]

buildDoc[srcFile_String] := Block[{tmpl, dir, out},
    If[ ! StringStartsQ[StringTrim @ Import[srcFile, "Text"], "---"],
        Print["  (skip ", FileNameTake[srcFile], ": no frontmatter)"];
        Return[]
    ];
    tmpl = frontmatterTemplate[srcFile];
    If[ ! KeyExistsQ[subdir, tmpl],
        Print["  (skip ", FileNameTake[srcFile], ": template ", tmpl, " is not a doc page)"];
        Return[]
    ];
    dir = FileNameJoin[Prepend[subdir[tmpl], pacletDir]];
    Quiet @ CreateDirectory[dir, CreateIntermediateDirectories -> True];
    out = FileNameJoin[{dir, FileBaseName[srcFile] <> ".nb"}];
    mtn[srcFile, out];
    Print["  ", FileNameTake[srcFile], " -> ", FileNameDrop[out, FileNameDepth[pacletDir]]]
]

Print["building Hypergraph documentation"]

Scan[buildDoc, Sort @ FileNames["*.md", FileNameJoin[{pacletDir, "docs"}], Infinity]]

rd = FileNameJoin[{pacletDir, "ResourceDefinition.md"}]

If[ FileExistsQ[rd],
    mtn[rd, FileNameJoin[{pacletDir, "ResourceDefinition.nb"}]];
    Print["  ResourceDefinition.md -> ResourceDefinition.nb"]
]

Print["done"]
