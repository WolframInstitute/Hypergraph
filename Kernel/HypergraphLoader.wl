Package["WolframInstitute`Hypergraph`"]

PackageImport["DocumentationSearch`"]



$PacletPath = ExpandFileName[FileNameJoin[{DirectoryName[$InputFileName], ".."}]]

Quiet[CreateDocumentationIndex[FileNameJoin[{$PacletPath, "Documentation", "English"}]]]

