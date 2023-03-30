Package["WolframInstitute`Hypergraph`"]

PackageExport["EnumerateHypergraphs"]
PackageExport["EnumerateOrderedHypergraphs"]



EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...}, n_Integer, opts___] :=
    Catenate[EnumerateOrderedHypergraphs[sig, {#}, opts] & /@ Range[n]]

EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...}, {n_Integer}, opts___] :=
    DeleteDuplicatesBy[
        Hypergraph[Range[n], #, opts] & /@ DeleteDuplicates[
            Catenate /@ Tuples[Subsets[Catenate[Permutations /@ Subsets[Range[n], {#[[2]]}]], {#[[1]]}] & /@ sig]
        ],
        CanonicalHypergraph
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

