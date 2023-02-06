Package["WolframInstitute`Hypergraph`"]


PackageExport["HypergraphIncidenceMatrix"]
PackageExport["IncidenceHypergraph"]
PackageExport["HyperMatrix"]
PackageExport["HyperMatrixGraph"]



HypergraphIncidenceMatrix[hg : {{___}...}] := With[{vs = Union @@ hg},
	SparseArray @ Transpose[Total[2 ^ (# - 1)] & /@ Lookup[PositionIndex[#], vs, {}] & /@ hg]
]


IncidenceHypergraph[vs_List, mat_] :=
	With[{rules = Catenate @ MapThread[Thread @* Rule, {Position[Reverse[#], 1, {1}, Heads -> False] & /@ IntegerDigits[#, 2], vs}]},
		If[rules === {}, {}, Normal @ SparseArray[rules]]
	] & /@ Transpose[mat]

IncidenceHypergraph[mat_] := IncidenceHypergraph[Range[Length[mat]], mat]



HyperMatrix[hg : {{___}...}] := Block[{vs = Union @@ hg, n, index},
	n = Length[vs];
	index = First /@ PositionIndex[vs];
	KeyValueMap[
		If[#1 == 0, Length[#2], SparseArray[Normal @ Counts[Partition[Lookup[index, Catenate[#2]], #]], Table[n, #1]]] &,
		KeySort @ GroupBy[hg, Length]
	]
]


HyperMatrixGraph[vs_List, hm_List] := With[{n = Length[vs], dims = Dimensions /@ hm},
	(
		Flatten[KeyValueMap[If[IntegerQ[#2] && #2 > 0, Table[vs[[#1]], #2], Nothing] &, If[ArrayQ[#], Association @ ArrayRules[#], <|{} -> #|>]] & /@ hm, 2]
	) /; Equal @@ Prepend[Catenate[dims], n]
]

HyperMatrixGraph[hm_List] := With[{dims = Catenate[Dimensions /@ hm]},
	HyperMatrixGraph[Range[First[dims]], hm] /; Equal @@ dims
]

