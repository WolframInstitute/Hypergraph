Package["WolframInstitute`Hypergraph`"]


PackageExport["AdjacencyTensor"]
PackageExport["AdjacencyHypergraph"]
PackageExport["HypergraphIncidenceMatrix"]
PackageExport["IncidenceHypergraph"]

PackageExport["HyperMatrixQ"]
PackageExport["HyperMatrix"]
PackageExport["HyperMatrixGraph"]



AdjacencyTensor[hg : {___List}] := Block[{n = Length[hg], vs, index, d, arities, indices, r},
	If[n == 0, Return[{}]];
	vs = Union @@ hg;
	index = First /@ PositionIndex[Union @@ hg];
	d = Length[vs] + 1;
	arities = Length /@ hg;
	r = Max[arities];
	indices = PadRight[TakeList[Lookup[index, Catenate[hg]], arities], {n, r}, d];
	SparseArray[Normal @ Counts[indices], Table[d, r]]
]

AdjacencyTensor[hg_Hypergraph] := AdjacencyTensor[hg["EdgeList"]]


AdjacencyHypergraph[vs_List, t_ ? ArrayQ] := Block[{dims = Dimensions[t], st, d},
	d = First[dims, 0];
	If[d == 0, Return[{}]];
	(
		st = SparseArray[t];
		Hypergraph @ Catenate @ MapThread[Table[vs[[#1]], #2] & , {DeleteCases[t["ExplicitPositions"], d, {2}], t["ExplicitValues"]}]
	) /; Equal @@ dims && d == Length[vs] + 1
]

AdjacencyHypergraph[t_] := AdjacencyHypergraph[Range[Length[t] - 1], t]


HypergraphIncidenceMatrix[hg : {___List}] := With[{vs = Union @@ hg},
	If[	Length[vs] > 0,
		SparseArray @ Transpose[Total[2 ^ (# - 1)] & /@ Lookup[PositionIndex[#], vs, {}] & /@ hg],
		{{}}
	]
]

HypergraphIncidenceMatrix[hg_Hypergraph] := HypergraphIncidenceMatrix[hg["EdgeList"]]


IncidenceHypergraph[vs_List, mat_ ? MatrixQ] :=
	With[{rules = Catenate @ MapThread[Thread @* Rule, {Position[Reverse[#], 1, {1}, Heads -> False] & /@ IntegerDigits[#, 2], vs}]},
		If[rules === {}, {}, Normal @ SparseArray[rules]]
	] & /@ Transpose[mat] // Hypergraph

IncidenceHypergraph[mat_] := IncidenceHypergraph[Range[Length[mat]], mat]



(* HyperMatrix *)

HyperMatrixQ[_HyperMatrix ? System`Private`HoldValidQ] := True

HyperMatrixQ[___] := False


HyperMatrix[hg : {___List}] := Block[{vs = Union @@ hg, n, index},
	n = Length[vs];
	index = First /@ PositionIndex[vs];
	HyperMatrix @@ KeyValueMap[
		If[#1 == 0, Length[#2], SparseArray[Normal @ Counts[Partition[Lookup[index, Catenate[#2]], #]], Table[n, #1]]] &,
		KeySort @ GroupBy[hg, Length]
	]
]

HyperMatrix[hg_Hypergraph] := HyperMatrix[hg["EdgeList"]]

hm : HyperMatrix[arr___SparseArray] /; System`Private`HoldNotValidQ[hm] && Equal @@ Catenate[Dimensions /@ {arr}] := System`Private`HoldSetValid[hm]


(hm_HyperMatrix ? HyperMatrixQ)["Arrays"] := List @@ hm

(hm_HyperMatrix ? HyperMatrixQ)["Dimensions"] := Dimensions /@ hm["Arrays"]


Plus[hms___HyperMatrix ? HyperMatrixQ] ^:= Block[{arrays = Catenate @ Through[{hms}["Arrays"]], dim},
	dim = Max[Dimensions /@ arrays];
	HyperMatrix @@ Values[GroupBy[PadRight[#, ConstantArray[dim, ArrayDepth[#]]] & /@ arrays, Dimensions, Total]]
]


$HyperMatrixIcon = Deploy @ GraphicsRow[
	ArrayPlot3D[#, Boxed -> False] & /@ {
		Array[If[#1 == #2 == 3, 1, 0] &, {5, 5, 5}],
		Array[If[#1 == 3, 1, 0] &, {5, 5, 5}],
		Array[1 &, {5, 5, 5}]}, 0,
		ImageSize -> Tiny
	]

HyperMatrix /: MakeBoxes[hm_HyperMatrix ? HyperMatrixQ, form : TraditionalForm] := With[{
	boxes = RowBox[ToBoxes[Normal[#], TraditionalForm] & /@ hm["Arrays"]]
},
	InterpretationBox[boxes,hm]
]

HyperMatrix /: MakeBoxes[hm_HyperMatrix ? HyperMatrixQ, form_] := BoxForm`ArrangeSummaryBox[
	"HyperMatrix",
	hm,
	$HyperMatrixIcon,
	{{}},
	{{BoxForm`SummaryItem[{"Dimensions: ", Row[Dimensions /@ List @@ hm, ";"]}]}},
	form,
	"Interpretable" -> Automatic
]


HyperMatrixGraph[vs_List, hm_List] := With[{n = Length[vs], dims = Dimensions /@ hm},
	(
		Hypergraph @ Flatten[
			KeyValueMap[
				If[IntegerQ[#2] && #2 > 0, Table[vs[[#1]], #2], Nothing] &,
				If[ArrayQ[#], Association @ ArrayRules[#], <|{} -> #|>]
			] & /@ hm,
			2
		]
	) /; Equal @@ Prepend[Catenate[dims], n]
]

HyperMatrixGraph[hm_List] := With[{dims = Catenate[Dimensions /@ hm]},
	HyperMatrixGraph[Range[First[dims]], hm] /; Equal @@ dims
]

HyperMatrixGraph[hm_HyperMatrix ? HyperMatrixQ] := HyperMatrixGraph[List @@ hm]

