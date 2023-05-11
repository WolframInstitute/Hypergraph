Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphIncidence"]
PackageExport["CanonicalHypergraph"]
PackageExport["IsomorphicHypergraphQ"]
PackageExport["EdgeSymmetry"]
PackageExport["EdgeListTagged"]

PackageScope["CanonicalEdge"]



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
    vs = VertexList[hg], edges = EdgeListTagged[hg], edgeSymmetry = EdgeSymmetry[hg],
    freeVertices, newVertices, emptyEdges, newEdges, iso, perm
},
    edges = CanonicalEdge[#, Lookup[edgeSymmetry, Key[#], {}]] & /@ edges;
    emptyEdges = Cases[edges, {}];
    edges = DeleteCases[edges, {}];
    {perm, iso} = ResourceFunction["FindCanonicalHypergraphIsomorphism"][edges, "IncludePermutation" -> True];
    newEdges = Map[Replace[iso], edges, {2}];
    newEdges = MapThread[CanonicalEdge[#1, Lookup[edgeSymmetry, Key[#2], {}]] &, {newEdges, edges}];
    newEdges = Permute[newEdges, perm];
    freeVertices = DeleteElements[vs, Keys[iso]];
    newVertices = Max[iso] + Range[Length @ freeVertices];
    iso = <|iso, Thread[freeVertices -> newVertices]|>;
    Hypergraph[
        Values[iso], Join[emptyEdges, newEdges],
        With[{keys = Keys[hg["Options"]]},
            KeySort @ Association @ hg["Options"] //
                MapAt[
                    Sort @ mapEdgeOptions[CanonicalEdge[Replace[#, iso, {1}], Lookup[edgeSymmetry, Key[#], {}]] &, #] &,
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


Hypergraph /: VertexReplace[hg_Hypergraph, rules_] := Hypergraph[
    DeleteDuplicates @ Replace[VertexList[hg], rules, {1}],
    Hyperedges @@ (Replace[{(edge_ -> tag_) :> Replace[edge, rules, {1}] -> tag, edge_ :> Replace[edge, rules, {1}]}] /@ EdgeListTagged[hg]),
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

