Package["WolframInstitute`Hypergraph`"]

PackageExport["EnumerateHypergraphs"]
PackageExport["EnumerateOrderedHypergraphs"]

PackageExport["RandomHypergraph"]
PackageExport["RandomAllHypergraph"]
PackageExport["RandomConnectedHypergraph"]



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



Options[RandomHypergraph] := Join[{"Simple" -> True, "Connected" -> All}, Options[Hypergraph]]

RandomHypergraph[s : _Integer ? Positive | Automatic : Automatic, sig_, opts : OptionsPattern[]] := Block[{
    hg,
    simple = Replace[OptionValue["Simple"], Except[All | True | False] -> True],
    connected = Replace[OptionValue["Connected"], Except[All | True | False] -> True]
},
    Until[
        Switch[simple, All, True, True, SimpleHypergraphQ[hg], False, ! SimpleHypergraphQ[hg]] &&
            Switch[connected, All, True, True, ConnectedHypergraphQ[hg], False, ! ConnectedHypergraphQ[hg]],

        hg = Hypergraph[ResourceFunction["RandomHypergraph"][{s, sig}]]
    ];
    Hypergraph[hg, FilterRules[{opts}, Options[Hypergraph]]]
]



RandomAllHypergraph[{v : _Integer ? Positive | Automatic, sig : {{_Integer ? Positive, _Integer ? Positive} ...}}] := With[{
	n = Replace[v, Automatic :> RandomInteger[{1, Total[Times @@@ sig]}]]
},
	Hypergraph[Range[n], Replace[sig, {m_, arity_} :> Splice @ RandomInteger[{1, n}, {m, arity}], 1]]
]

HyperConnectedComponents[edges_] := WeaklyConnectedComponents[UndirectedEdge @@@ Catenate[Partition[#, 2, 1] & /@ edges]]

RandomConnectEdges[edges_] := Block[{newEdges = edges, components},
	While[True,
		components = HyperConnectedComponents[newEdges];
		If[Length[components] == 1, Break[]];
		newEdges = Replace[newEdges, RandomChoice[Catenate[#1]] -> RandomChoice[Catenate[#2]], {2}] & @@ TakeDrop[components, 1]
	];
	newEdges
]

RandomConnectedHypergraph[{v : _Integer ? Positive | Automatic, sig : {{_Integer ? Positive, _Integer ? Positive} ...}}] := With[{
	n = Replace[v, Automatic :> RandomInteger[{1, Total[Times @@@ sig] - Total[sig[[All, 1]]] + 1}]]
},
	(*Hypergraph[RandomConnectEdges @ Replace[sig, {m_, arity_} :> Splice @ RandomInteger[{1, n}, {m, arity}], 1]]*)
	Fold[
		Block[{xs = RandomChoice[Union @@ #1, RandomInteger[{1, #2}]], ys, k},
			k = #2 - Length[xs];
			Append[#1, If[k == 0, xs, ys = Complement[Range[0, n - 1], xs]; RandomSample @ Join[xs, ys[[1 + ResourceFunction["TupleFromIndex"][RandomInteger[{1, Length[ys] ^ k}], k]]]]]]
		] &,
		{ResourceFunction["TupleFromIndex"][RandomInteger[{1, n ^ First[#]}], First[#]]},
		Rest[#]
	] & @ RandomSample[Replace[sig, {m_, arity_} :> Splice @ Table[arity, m], 1]]
]

