Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphIncidence"]



EdgeList[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeList"]

EdgeCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ EdgeList[hg]

VertexList[hg_Hypergraph ? HypergraphQ] ^:= hg["VertexList"]

VertexCount[hg_Hypergraph ? HypergraphQ] ^:= Length @ VertexList[hg]


HypergraphIncidence[hg_ ? HypergraphQ] := Merge[(u |-> AssociationMap[u &, u]) /@ EdgeList[hg], Identity][[Key /@ VertexList[hg]]]

VertexDegree[hg_Hypergraph ? HypergraphQ] ^:=
    KeyValueMap[{v, es} |-> Total @ Thread[Map[Count[#, v] &, es] (Length /@ es - 1)], HypergraphIncidence[hg]]

