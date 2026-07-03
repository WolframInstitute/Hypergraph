#!/usr/bin/env wolframscript
(* Verify the built documentation: lint the resource definition notebook the
   way the docked Check button does, run DocumentationBuild on sample doc
   pages, and confirm built pages carry evaluated Output cells. *)

Needs["DefinitionNotebookClient`"]

pacletDir = DirectoryName[$InputFileName]

(* 1. resource definition lint *)
Print["==== ResourceDefinition.nb lint ===="]
hints = UsingFrontEnd @ Block[{nbo, ds},
    CurrentValue[$FrontEnd, LightDark] = "Light";
    nbo = NotebookOpen[File[FileNameJoin[{pacletDir, "ResourceDefinition.nb"}]]];
    CurrentValue[nbo, CreateCellID] = True;
    SelectionMove[nbo, All, Notebook];
    FrontEndTokenExecute[nbo, "Save"];
    ds = Quiet @ DefinitionNotebookClient`CheckDefinitionNotebook[nbo];
    NotebookClose[nbo];
    Normal[ds]
]
Print["hints: ", Length[hints]]
Scan[Print["  [", #["Level"], "] ", #["Tag"], " ", ToString[#["Parameters"], InputForm]] &, hints]

(* 2. output-cell census over every built doc page *)
Print[""]
Print["==== output cells per built page ===="]
Scan[
    Function[nb, Block[{cells = First[Get[nb]], out, msg},
        out = Count[cells, Cell[_, "Output", ___], Infinity];
        msg = Count[cells, Cell[_, "Message" | "MSG", ___], Infinity];
        Print["  ", FileBaseName[nb], ": ", out, " outputs, ", msg, " message cells", If[msg > 0, "  <-- CHECK", ""]]
    ]],
    FileNames["*.nb", FileNameJoin[{pacletDir, "Documentation"}], Infinity]
]

(* 3. DocumentationBuild on two representative pages *)
Print[""]
Print["==== DocumentationBuild sample ===="]
Needs["DocumentationBuild`"]
Scan[
    Function[nb, Block[{built},
        built = Quiet @ UsingFrontEnd @ DocumentationBuild`DocumentationBuildNotebook[None, Get[nb]];
        Print["  ", FileBaseName[nb], ": ", If[MatchQ[built, _Notebook], "built OK (" <> ToString @ Count[built, Cell[___], Infinity] <> " cells)", "FAILED: " <> ToString[Head[built]]]]
    ]],
    {
        FileNameJoin[{pacletDir, "Documentation", "English", "ReferencePages", "Symbols", "EnumerateHypergraphRules.nb"}],
        FileNameJoin[{pacletDir, "Documentation", "English", "Guides", "HypergraphFunctionality.nb"}]
    }
]

Print["check done"]
