Package["WolframInstitute`Hypergraph`"]

PackageImport["DocumentationSearch`"]

ClearAll["WolframInstitute`Hypergraph`*", "WolframInstitute`Hypergraph`**`*"]

$PacletPath = ExpandFileName[FileNameJoin[{DirectoryName[$InputFileName], ".."}]]

If[ $FrontEnd =!= Null,
    FrontEndExecute[FE`systemQ[FE`s_] := StringMatchQ[Quiet[Check[Context[FE`s], ""]], "System`" | "WolframInstitute`Hypergraph`"]];

    Scan[
        CopyFile[
            #,
            FileNameJoin[{$InstallationDirectory, "SystemFiles", "Components", "AutoCompletionData", "Main", "OptionValues", FileNameTake[#, -1]}],
            OverwriteTarget -> True
        ] &,
        FileNames[FileNameJoin[{PacletObject["WolframInstitute/Hypergraph"]["Location"], "AutoCompletionData", "OptionValues", "*"}]]
    ];

    Scan[
        FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]] &,
        Import[FileNameJoin[{PacletObject["WolframInstitute/Hypergraph"]["Location"], "AutoCompletionData", "specialArgFunctions.tr"}], "WL"]
    ]
]