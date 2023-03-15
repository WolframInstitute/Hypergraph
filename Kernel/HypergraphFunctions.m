Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphIncidence"]
PackageExport["CanonicalHypergraph"]
PackageExport["IsomorphicHypergraphQ"]



EdgeList[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeList"]

EdgeList[hg_Hypergraph ? HypergraphQ, patt_] ^:= Cases[hg["EdgeList"], patt]

EdgeCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ EdgeList[hg]

VertexList[hg_Hypergraph ? HypergraphQ] ^:= hg["VertexList"]

VertexList[hg_Hypergraph ? HypergraphQ, patt_] ^:= Cases[hg["VertexList"], patt]

VertexCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ VertexList[hg]


HypergraphIncidence[hg_ ? HypergraphQ] := Merge[(u |-> AssociationMap[u &, u]) /@ EdgeList[hg], Identity][[Key /@ VertexList[hg]]]


VertexDegree[hg_Hypergraph ? HypergraphQ] ^:= Values[Length /@ HypergraphIncidence[hg]]

VertexDegree[hg_Hypergraph ? HypergraphQ, vertex_] ^:= If[ListQ[vertex], Map[Length], Length] @ Lookup[HypergraphIncidence[hg], vertex]


CanonicalHypergraph[hg_ ? HypergraphQ] := Block[{vs = hg["VertexList"], edges = hg["EdgeList"], newVertices, emptyEdges, newEdges, iso, perm},
    emptyEdges = Cases[edges, {}];
    edges = DeleteCases[edges, {}];
    {perm, iso} = ResourceFunction["FindCanonicalHypergraphIsomorphism"][edges, "IncludePermutation" -> True];
    newEdges = Permute[edges /. iso, perm];
    newVertices = Union[Values[iso], Max[iso] + Range[Length @ DeleteElements[vs, Keys[iso]]]];
    Hypergraph[newVertices, Join[emptyEdges, newEdges], hg["Options"]]
]


IsomorphicHypergraphQ[hg1_ ? HypergraphQ, hg2_ ? HypergraphQ] :=
    Through[{VertexList, EdgeList} @ CanonicalHypergraph[hg1]] === Through[{VertexList, EdgeList} @ CanonicalHypergraph[hg2]]


VertexReplace[hg_Hypergraph, rules_] ^:= Hypergraph[
    Replace[VertexList[hg], rules, {1}],
    Hyperedges @@ Replace[EdgeList[hg], rules, {2}],
    hg["Options"]
]

