Package["WolframInstitute`Hypergraph`"]

PackageExport["EnumerateHypergraphs"]
PackageExport["EnumerateOrderedHypergraphs"]

PackageExport["RandomHypergraph"]



Options[EnumerateOrderedHypergraphs] := Join[{"Simple" -> True, "Connected" -> True, "Canonical" -> Automatic}, Options[Hypergraph]]

EnumerateOrderedHypergraphs[sig : {{_Integer, _Integer} ...}, opts : OptionsPattern[]] := EnumerateOrderedHypergraphs[Automatic, sig, opts]

EnumerateOrderedHypergraphs[
    {s : _Integer ? Positive | Automatic | All : Automatic},
    sig : {{_Integer, _Integer} ...},
    opts : OptionsPattern[]
] := With[{
    simple = Replace[OptionValue["Simple"], Except[All | True | False] -> True],
    connType = Replace[OptionValue["Connected"], {True -> Automatic, Except[False] -> None}],
    canonical = OptionValue["Canonical"]
},
    If[ MatchQ[canonical, False | None], Identity, DeleteDuplicatesBy[CanonicalHypergraph[#, Method -> canonical] &]] @
        If[connType === False, Select[Not @* ConnectedHypergraphQ], Identity] @
            Switch[simple, All, Identity, True, Select[SimpleHypergraphQ], False, Select[Not @* SimpleHypergraphQ]][
                Hypergraph[#["Input"], opts, "EdgeSymmetry" -> "Ordered", ImageSize -> Small] & /@ EnumerateHypergraphRules[
                    sig -> {},
                    If[MatchQ[s, Automatic | All], #, {s, #}] & @ Replace[connType, False -> None]
                ]
            ]
]

EnumerateOrderedHypergraphs[
    s : _Integer ? Positive | Automatic | All : Automatic,
    sig : {{_Integer, _Integer} ...},
    opts : OptionsPattern[]
] := Catenate[
    EnumerateOrderedHypergraphs[{#}, sig, opts] & /@ Range[Replace[s, Automatic | All :> maxConnectedAtoms[sig, Replace[OptionValue["Connected"], {True -> Automatic, _ -> None}]]]]
]


Options[EnumerateHypergraphs] := Options[EnumerateOrderedHypergraphs]

EnumerateHypergraphs[sig : {{_Integer, _Integer} ...}, opts : OptionsPattern[]] := EnumerateHypergraphs[Automatic, sig, opts]

EnumerateHypergraphs[s : _Integer ? Positive | Automatic | All | {_Integer ? Positive | Automatic | All} : Automatic, sig : {{_Integer, _Integer} ...}, opts : OptionsPattern[]] :=
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

