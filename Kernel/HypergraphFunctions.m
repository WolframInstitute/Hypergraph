Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphIncidence"]
PackageExport["CanonicalHypergraph"]
PackageExport["IsomorphicHypergraphQ"]
PackageExport["ToOrderedHypergraph"]
PackageExport["EdgeSymmetry"]
PackageExport["EdgeListTagged"]
PackageExport["SimpleHypergraph"]
PackageExport["SimpleHypergraphQ"]

PackageScope["CanonicalEdge"]



HoldPattern[Options[hg_Hypergraph ? HypergraphQ]] ^:= hg["Options"]

EdgeList[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeList"]

EdgeList[hg_Hypergraph ? HypergraphQ, patt_] ^:= Cases[hg["EdgeList"], patt]

EdgeListTagged[hg_Hypergraph ? HypergraphQ] := hg["EdgeListTagged"]

EdgeListTagged[hg_Hypergraph ? HypergraphQ, patt_] := Cases[hg["EdgeListTagged"], patt]

EdgeCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ EdgeList[hg]

EdgeTags[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeTags"]

EdgeSymmetry[hg_Hypergraph ? HypergraphQ] := hg["FullEdgeSymmetry"]

VertexList[hg_Hypergraph ? HypergraphQ] ^:= hg["VertexList"]

VertexList[hg_Hypergraph ? HypergraphQ, patt_] ^:= Cases[hg["VertexList"], patt]

VertexCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ VertexList[hg]


HypergraphIncidence[hg_ ? HypergraphQ] := Merge[(u |-> AssociationMap[u &, u]) /@ EdgeList[hg], Identity][[Key /@ VertexList[hg]]]


VertexDegree[hg_Hypergraph ? HypergraphQ] ^:= Values[Length /@ HypergraphIncidence[hg]]

VertexDegree[hg_Hypergraph ? HypergraphQ, vertex_] ^:= If[ListQ[vertex], Map[Length], Length] @ Lookup[HypergraphIncidence[hg], vertex]


mapEdgeOptions[f_, opt_] := Replace[Flatten[{opt}], {((edge_List -> tag_) -> v_) :> (f[edge] -> tag) -> v, (edge_List -> v_) :> f[edge] -> v}, {1}]

mapVertexOptions[f_, opt_] := Replace[Flatten[{opt}], (vertex_ -> v_) :> f[vertex] -> v, {1}]


CanonicalEdge[edge_List, symm : {___Cycles}] := First[Sort[Permute[edge, #] & /@ GroupElements[PermutationGroup[symm]]]]

CanonicalEdge[edge_List -> _, symm : {___Cycles}] := CanonicalEdge[edge, symm]


CanonicalHypergraph[hg_ ? HypergraphQ] := Block[{
	vs = VertexList[hg], edges = EdgeList[hg], tags = EdgeTags[hg],
	orderedEdges, counts, iso, emptyEdges, normalEdges, newEdges, ordering,
	freeVertices, newVertices
},
	orderedEdges = MapThread[
		{edge, symm} |-> (Permute[edge, #] & /@ GroupElements[PermutationGroup[symm]]),
		{edges, Values @ EdgeSymmetry[hg]}
	];
    counts = Length /@ orderedEdges;
    orderedEdges = Catenate @ orderedEdges;
	emptyEdges = Cases[orderedEdges, {}];
    normalEdges = DeleteCases[orderedEdges, {}];
	iso = ResourceFunction["FindCanonicalHypergraphIsomorphism"][normalEdges];
    newEdges = First @* Sort /@ TakeList[Map[Replace[iso], normalEdges, {2}], counts];
	ordering = Ordering[newEdges];
	newEdges = MapThread[If[#2 === None, #1, #1 -> #2] &, {newEdges[[ordering]], tags[[ordering]]}];
    freeVertices = DeleteElements[vs, Keys[iso]];
    newVertices = Max[iso] + Range[Length @ freeVertices];
    iso = <|iso, Thread[freeVertices -> newVertices]|>;
    Hypergraph[
        Values[iso],
        Join[emptyEdges, newEdges],
        With[{keys = Keys[Options[hg]]},
            KeySort @ Association @ Options[hg] //
                MapAt[
                    Sort @ mapEdgeOptions[Replace[#, iso, {1}] &, #] &,
                    {Key[#]} & /@ Intersection[{EdgeStyle, EdgeLabels, EdgeLabelStyle, "EdgeSymmetry"}, keys]
                ] //
                MapAt[
                    Sort @ mapVertexOptions[Replace[#, iso] &, #] &,
                    {Key[#]} & /@ Intersection[{VertexStyle, VertexLabels, VertexLabelStyle, VertexCoordinates}, keys]
                ] //
                Normal
        ]
    ]
]

CanonicalHypergraph[args___] := CanonicalHypergraph[Hypergraph[args]]


IsomorphicHypergraphQ[hg1_ ? HypergraphQ, hg2_ ? HypergraphQ] :=
    Through[{VertexList, EdgeList, EdgeSymmetry} @ CanonicalHypergraph[hg1]] === Through[{VertexList, EdgeList, EdgeSymmetry} @ CanonicalHypergraph[hg2]]


ToOrderedHypergraph[hg_ ? HypergraphQ] := Hypergraph[
	VertexList[hg],
	Catenate @ MapThread[
		{edge, tag, symm} |-> (If[tag === None, #, # -> tag] & @ Permute[edge, #] & /@ GroupElements[PermutationGroup[symm]]),
		{EdgeList[hg], EdgeTags[hg], Values @ EdgeSymmetry[hg]}
	],
	"EdgeSymmetry" -> "Ordered",
	Options[hg]
]


Hypergraph /: VertexReplace[hg_Hypergraph, rules_, opts : OptionsPattern[]] := Hypergraph[
    DeleteDuplicates @ Replace[VertexList[hg], rules, {1}],
    Hyperedges @@ (Replace[{(edge_ -> tag_) :> Replace[edge, rules, {1}] -> tag, edge_ :> Replace[edge, rules, {1}]}] /@ EdgeListTagged[hg]),
    opts,
    Replace[
        hg["Options"], {
            (opt : (VertexStyle | VertexLabels | VertexLabelStyle | VertexCoordinates) -> annotation_List) :>
                opt -> Replace[annotation, {(h : Rule | RuleDelayed)[v_, a_] :> h[Replace[v, rules], a]}, {1}],
            (opt : (EdgeStyle | EdgeLabels | EdgeLabelStyle | "EdgeSymmetry") -> annotation_List) :>
                opt -> Replace[annotation, {(h : Rule | RuleDelayed)[e_, a_] :> h[Replace[e, rules, {1}], a]}, {1}]
        },
        {1}
    ]
]

Hypergraph /: HoldPattern[VertexReplace[rules_][hg_Hypergraph]] := VertexReplace[hg, rules]


SimpleHypergraph[hg_ ? HypergraphQ, opts : OptionsPattern[]] := With[{symm = EdgeSymmetry[hg]},
    Hypergraph[
        VertexList[hg],
        Select[DuplicateFreeQ] @ DeleteDuplicates @ MapThread[CanonicalEdge, {Replace[EdgeListTagged[hg], (edge_ -> _) :> edge, {1}], Values[symm]}],
        opts,
        Replace[
            Options[hg],
            (opt : (EdgeStyle | EdgeLabels | EdgeLabelStyle | "EdgeSymmetry") -> annotation_List) :>
                    opt -> DeleteDuplicatesBy[First] @ Replace[
                        annotation,
                        ((edge_ -> _) | edge_ -> a_) /; EdgeQ[hg, Verbatim[edge]] :>
                            If[DuplicateFreeQ[edge], CanonicalEdge[edge, Lookup[symm, Key[edge]]] -> a, Nothing],
                        {1}
                    ],
            {1}
        ]
    ]
]

SimpleHypergraph[args___, opts : OptionsPattern[]] := SimpleHypergraph[Hypergraph[args], opts]

SimpleHypergraphQ[hg_ ? HypergraphQ] := EdgeCount[hg] === EdgeCount[SimpleHypergraph[hg]]


Hypergraph /: VertexIndex[hg_Hypergraph, v : Except[_List]] := First @ FirstPosition[VertexList[hg], v, {Missing[v]}, {1}, Heads -> False]

Hypergraph /: VertexIndex[hg_Hypergraph, vs_List] := VertexIndex[hg, #] & /@ vs


Hypergraph /: EdgeIndex[hg_Hypergraph, e : Except[_List]] := First @ FirstPosition[EdgeListTagged[hg], e, {Missing[e]}, {1}, Heads -> False]

Hypergraph /: EdgeIndex[hg_Hypergraph, es_List] := EdgeIndex[hg, #] & /@ es


Hypergraph /: VertexAdd[hg_Hypergraph, vertices_List, opts : OptionsPattern[]] :=
    Hypergraph[Join[VertexList[hg], vertices], EdgeListTagged[hg], opts, hg["Options"]]

Hypergraph /: VertexAdd[hg_Hypergraph, vertex_, opts : OptionsPattern[]] := VertexAdd[hg, {vertex}, opts]


Hypergraph /: EdgeAdd[hg_Hypergraph, edges : {(_List | _Rule) ...}, opts : OptionsPattern[]] :=
    Hypergraph[VertexList[hg], Join[EdgeListTagged[hg], edges], opts, hg["Options"]]

Hypergraph /: EdgeAdd[hg_Hypergraph, edge_, opts : OptionsPattern[]] := EdgeAdd[hg, {edge}, opts]


Hypergraph /: VertexQ[hg_Hypergraph, vertex_] := MemberQ[VertexList[hg], vertex]

Hypergraph /: EdgeQ[hg_Hypergraph, edge_List] := MemberQ[EdgeList[hg], edge]

Hypergraph /: EdgeQ[hg_Hypergraph, edge_Rule] := MemberQ[EdgeListTagged[hg], edge]

