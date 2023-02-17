Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphIncidence"]
PackageExport["HypergraphVertexDegree"]



HypergraphIncidence[hg_Hypergraph] :=
    Merge[(u |-> AssociationMap[u &, u]) /@ DeleteDuplicates /@ hg["EdgeList"], Apply[Union]]


HypergraphVertexDegree[hg_Hypergraph] := Length /@ HypergraphIncidence[hg]

