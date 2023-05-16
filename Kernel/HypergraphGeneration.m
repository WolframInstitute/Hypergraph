Package["WolframInstitute`Hypergraph`"]

PackageExport["EnumerateHypergraphs"]
PackageExport["EnumerateOrderedHypergraphs"]



EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...}, n_Integer, opts___] :=
    Catenate[EnumerateOrderedHypergraphs[sig, {#}, opts] & /@ Range[n]]

EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...}, {n_Integer}, opts___] :=
    DeleteDuplicatesBy[
        Hypergraph[Range[n], #, opts, "EdgeSymmetry" -> "Ordered"] & /@ DeleteDuplicates[
            Catenate /@ Tuples[Subsets[Catenate[Permutations /@ Subsets[Range[n], {#[[2]]}]], {#[[1]]}] & /@ sig]
        ],
        CanonicalHypergraph
    ]


EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...},
    Optional[{s : _Integer ? Positive | Automatic : Automatic, type : True | False | None | Automatic : Automatic}, {Automatic, Automatic}],
    opts : OptionsPattern[]
] := With[{
    parallelMap = Symbol[Information[ResourceFunction["ParallelMapMonitored"], "SymbolName"]],
    maxConnectedAtoms = Symbol[Information[ResourceFunction["EnumerateWolframModelRules"], "Context"] <> "maxConnectedAtoms"],
    ruleType = Replace[type, {True -> Automatic, False -> None}]
},
    Block[{parallelMap = ParallelMap[##, DistributedContexts -> Automatic] &},
        Hypergraph[First[#], opts, "EdgeSymmetry" -> "Ordered"] & /@ ResourceFunction["EnumerateWolframModelRules"][
            sig -> {},
            {Replace[s, Automatic :> maxConnectedAtoms[sig, ruleType]], ruleType}
        ]
    ]
]


EnumerateHypergraphs[sig : {{_Integer, _Integer} ...}, n_Integer, opts___] :=
    Catenate[EnumerateHypergraphs[sig, {#}, opts] & /@ Range[n]]

EnumerateHypergraphs[sig : {{_Integer, _Integer} ...}, {n_Integer}, opts___] :=
    DeleteDuplicatesBy[
        Hypergraph[Range[n], #, opts] & /@ DeleteDuplicates[
            Catenate /@ Tuples[Subsets[Subsets[Range[n], {#[[2]]}], {#[[1]]}] & /@ sig]
        ],
        CanonicalHypergraph
    ]

EnumerateHypergraphs[sig : {{_Integer, _Integer} ...}, args___, opts : OptionsPattern[]] :=
    DeleteDuplicatesBy[EnumerateOrderedHypergraphs[sig, args, opts, "EdgeSymmetry" -> "Unordered"], CanonicalHypergraph]

