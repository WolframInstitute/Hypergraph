Package["WolframInstitute`Hypergraph`"]

PackageExport["EnumerateHypergraphs"]
PackageExport["EnumerateOrderedHypergraphs"]

PackageExport["RandomHypergraph"]



Options[EnumerateOrderedHypergraphs] := Join[{"Simple" -> True, "Connected" -> True, "Canonical" -> True}, Options[Hypergraph]]

EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...}, opts : OptionsPattern[]] := EnumerateOrderedHypergraphs[Automatic, sig, opts]

EnumerateOrderedHypergraphs[
    s : _Integer ? Positive | Automatic : Automatic,
    sig : {{_Integer, _Integer} ...},
    opts : OptionsPattern[]
] := With[{
    simple = Replace[OptionValue["Simple"], Except[All | True | False] -> True],
    connType = Replace[OptionValue["Connected"], {True -> Automatic, Except[False] -> None}],
    canonicalQ = TrueQ[OptionValue["Canonical"]]
},
    If[canonicalQ, DeleteDuplicatesBy[CanonicalHypergraph], Identity] @
        If[connType === False, Select[Not @* ConnectedHypergraphQ], Identity] @
            Switch[simple, All, Identity, True, Select[SimpleHypergraphQ], False, Select[Not @* SimpleHypergraphQ]][
                Hypergraph[#["Input"], opts, "EdgeSymmetry" -> "Ordered", ImageSize -> Small] & /@ EnumerateHypergraphRules[
                    sig -> {},
                    If[s === Automatic, #, {s, #}] & @ Replace[connType, False -> None]
                ]
            ]
]

EnumerateOrderedHypergraphs[
    {s : _Integer ? Positive | Automatic : Automatic}, sig : {{_Integer, _Integer} ...},
    opts : OptionsPattern[]
] := Select[EnumerateOrderedHypergraphs[s, sig, opts], VertexCount[#] == s &]


Options[EnumerateHypergraphs] := Options[EnumerateOrderedHypergraphs]

EnumerateHypergraphs[sig : {{_Integer, _Integer} ...}, opts : OptionsPattern[]] := EnumerateHypergraphs[Automatic, sig, opts]

EnumerateHypergraphs[s : _Integer ? Positive | Automatic : Automatic, sig : {{_Integer, _Integer} ...}, opts : OptionsPattern[]] :=
    EnumerateOrderedHypergraphs[s, sig, opts, "EdgeSymmetry" -> "Unordered"]



Options[RandomHypergraph] := Join[{"Simple" -> False, "Connected" -> False}, Options[Hypergraph]]

RandomHypergraph[s : _Integer ? Positive | Automatic : Automatic, sig_, opts : OptionsPattern[]] := Block[{
    hg,
    simple = Replace[OptionValue["Simple"], Except[All | True | False] -> True],
    connected = Replace[OptionValue["Connected"], Except[All | True | False] -> True]
},
    Until[
        Switch[simple, All, True, True, SimpleHypergraphQ[hg], False, ! SimpleHypergraphQ[hg]] &&
            Switch[connected, All, True, True, ConnectedHypergraphQ[hg], False, ! ConnectedHypergraphQ[hg]],

        hg = Hypergraph[ResourceFunction["RandomHypergraph"][{s, sig}], opts]
    ];
    hg
]

