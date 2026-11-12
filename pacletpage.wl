(* ::Package:: *)

(* What build.wls and release.wls share: PacletPage, installed from its published resource when
   absent; the paclet's directory; and how its documentation is built. *)

$repositoryDirectory = DirectoryName[$InputFileName]

$pacletDirectory = FileNameJoin[{$repositoryDirectory, "Hypergraph"}]

If[ PacletFind["WolframInstitute/PacletPage"] === {},
    PacletInstall[ResourceObject["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"]]
]

Needs["WolframInstitute`PacletPage`"]

(* The pages are the markdown under docs/, and the definition is docs/ResourceDefinition.md. Each
   section of a symbol page, and each example between ---, runs in a clean context; a tutorial
   runs in one. Pages convert on PACLETPAGE_PARALLEL parallel kernels, four by default, or in this
   kernel alone with PACLETPAGE_PARALLEL=1. *)

$documentationOptions = {
    "Source" -> FileNameJoin[{$repositoryDirectory, "docs"}],
    "Parallel" -> Replace[Interpreter["Integer"][Environment["PACLETPAGE_PARALLEL"]], {1 -> False, Except[_Integer ? Positive] -> 4}]
}
