Package["WolframInstitute`Hypergraph`"]

PackageExport["EnumerateHypergraphs"]
PackageExport["EnumerateOrderedHypergraphs"]



EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...},
    Optional[{s : _Integer ? Positive | Automatic : Automatic, connectedQ : True | False : True, simpleQ : True | False : True}, {Automatic, True, True}],
    opts : OptionsPattern[]
] := With[{
    connType = Replace[connectedQ, {True -> Automatic, False -> None}]
},
    DeleteDuplicatesBy[
        If[simpleQ, Select[SimpleHypergraphQ], Identity][
            Hypergraph[First[#], opts, "EdgeSymmetry" -> "Ordered"] & /@ EnumerateWolframModelRules[
                sig -> {},
                {s, connType}
            ]
        ],
        CanonicalHypergraph
    ]
]

EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...}, s : _Integer ? Positive | Automatic,
    Optional[{connectedQ : True | False : True, simpleQ : True | False : True}, {True, True}],
    opts : OptionsPattern[]
] :=
    Catenate[EnumerateOrderedHypergraphs[sig, Prepend[{connectedQ, simpleQ}, #], opts] & /@ Range[
        Replace[s, Automatic :> maxConnectedAtoms[sig, Replace[connectedQ, {True -> Automatic, False -> None}]]]
    ]]

EnumerateHypergraphs[sig : {{_Integer, _Integer} ...}, args___, opts : OptionsPattern[]] :=
    EnumerateOrderedHypergraphs[sig, args, opts, "EdgeSymmetry" -> "Unordered"]

