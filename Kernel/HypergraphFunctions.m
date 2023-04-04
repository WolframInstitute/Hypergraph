Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphIncidence"]
PackageExport["CanonicalHypergraph"]
PackageExport["IsomorphicHypergraphQ"]



EdgeList[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeList"]

EdgeList[hg_Hypergraph ? HypergraphQ, patt_] ^:= Cases[hg["EdgeList"], patt]

EdgeCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ EdgeList[hg]

EdgeTags[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeTags"]

VertexList[hg_Hypergraph ? HypergraphQ] ^:= hg["VertexList"]

VertexList[hg_Hypergraph ? HypergraphQ, patt_] ^:= Cases[hg["VertexList"], patt]

VertexCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ VertexList[hg]


HypergraphIncidence[hg_ ? HypergraphQ] := Merge[(u |-> AssociationMap[u &, u]) /@ EdgeList[hg], Identity][[Key /@ VertexList[hg]]]


VertexDegree[hg_Hypergraph ? HypergraphQ] ^:= Values[Length /@ HypergraphIncidence[hg]]

VertexDegree[hg_Hypergraph ? HypergraphQ, vertex_] ^:= If[ListQ[vertex], Map[Length], Length] @ Lookup[HypergraphIncidence[hg], vertex]


mapEdgeOptions[f_, opt_] := Replace[Flatten[{opt}], {((edge_List -> tag_) -> v_) :> (f[edge] -> tag) -> v, (edge_List -> v_) :> f[edge] -> v}, {1}]

mapVertexOptions[f_, opt_] := Replace[Flatten[{opt}], (vertex_ -> v_) :> f[vertex] -> v, {1}]


CanonicalHypergraph[hg_ ? HypergraphQ] := Block[{vs = hg["VertexList"], edges = hg["EdgeList"], newVertices, emptyEdges, newEdges, iso, perm},
    emptyEdges = Cases[edges, {}];
    edges = DeleteCases[edges, {}];
    {perm, iso} = ResourceFunction["FindCanonicalHypergraphIsomorphism"][edges, "IncludePermutation" -> True];
    newEdges = Permute[edges /. iso, perm];
    newVertices = Union[Values[iso], Max[iso] + Range[Length @ DeleteElements[vs, Keys[iso]]]];
    Hypergraph[
        newVertices, Join[emptyEdges, newEdges],
        With[{keys = Keys[hg["Options"]]},
            Association @ hg["Options"] //
                MapAt[
                    mapEdgeOptions[Replace[#, iso, {1}] &, #] &,
                    {Key[#]} & /@ Intersection[{EdgeStyle, EdgeLabels, EdgeLabelStyle, "Symmetry"}, keys]
                ] //
                MapAt[
                    mapVertexOptions[Replace[#, iso] &, #] &,
                    {Key[#]} & /@ Intersection[{VertexStyle, VertexLabels, VertexLabelStyle, VertexCoordinates}, keys]
                ] //
                Normal
        ]
    ]
]


IsomorphicHypergraphQ[hg1_ ? HypergraphQ, hg2_ ? HypergraphQ] :=
    Through[{VertexList, EdgeList} @ CanonicalHypergraph[hg1]] === Through[{VertexList, EdgeList} @ CanonicalHypergraph[hg2]]


VertexReplace[hg_Hypergraph, rules_] ^:= Hypergraph[
    Replace[VertexList[hg], rules, {1}],
    Hyperedges @@ Replace[EdgeList[hg], rules, {2}],
    hg["Options"]
]

