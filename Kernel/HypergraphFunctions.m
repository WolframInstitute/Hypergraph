Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphIncidence"]
PackageExport["CanonicalHypergraph"]
PackageExport["IsomorphicHypergraphQ"]



EdgeList[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeList"]

EdgeCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ EdgeList[hg]

VertexList[hg_Hypergraph ? HypergraphQ] ^:= hg["VertexList"]

VertexCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ VertexList[hg]


HypergraphIncidence[hg_ ? HypergraphQ] := Merge[(u |-> AssociationMap[u &, u]) /@ EdgeList[hg], Identity][[Key /@ VertexList[hg]]]


VertexDegree[hg_Hypergraph ? HypergraphQ] ^:=
    KeyValueMap[{v, es} |-> Total @ Thread[Map[Count[#, v] &, es] (Length /@ es - 1)], HypergraphIncidence[hg]]


CanonicalHypergraph[hg_ ? HypergraphQ] := Block[{vs = hg["VertexList"], edges = hg["EdgeList"], newVertices, newEdges, iso, perm},
    {perm, iso} = ResourceFunction["FindCanonicalHypergraphIsomorphism"][edges, "IncludePermutation" -> True];
    newEdges = Permute[edges /. iso, perm];
    newVertices = Union[Values[iso], Max[iso] + Range[Length @ DeleteElements[vs, Keys[iso]]]];
    Hypergraph[newVertices, newEdges, hg["Options"]]
]


IsomorphicHypergraphQ[hg1_ ? HypergraphQ, hg2_ ? HypergraphQ] :=
    Through[CanonicalHypergraph[hg1]{"VertexList", "EdgeList"}] === Through[CanonicalHypergraph[hg2]{ "VertexList", "EdgeList"}]


VertexReplace[hg_Hypergraph, rules_] ^:= Hypergraph[
    Replace[VertexList[hg], rules, {1}],
    Hyperedges @@ Replace[EdgeList[hg], rules, {2}],
    hg["Options"]
]

