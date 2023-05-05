Package["WolframInstitute`Hypergraph`"]


PackageExport["AdjacencyTensor"]
PackageExport["AdjacencyHypergraph"]
PackageExport["HypergraphIncidenceMatrix"]
PackageExport["IncidenceHypergraph"]

PackageExport["HyperMatrixQ"]
PackageExport["HyperMatrix"]
PackageExport["HyperMatrixGraph"]

PackageExport["HypergraphTransitionMatrix"]



AdjacencyTensor[vs_List, hg : {___List}] := Block[{n = Length[hg], index, d, arities, indices, r},
	If[n == 0, Return[{}]];
	index = First /@ PositionIndex[Union @@ hg];
	d = Length[vs] + 1;
	arities = Length /@ hg;
	r = Max[arities];
	indices = PadRight[TakeList[Lookup[index, Catenate[hg]], arities], {n, r}, d];
	SparseArray[Normal @ Counts[indices], Table[d, r]]
]

AdjacencyTensor[hg : {___List}] := AdjacencyTensor[Union @@ hg, hg]

AdjacencyTensor[hg_Hypergraph] := AdjacencyTensor[VertexList[hg], EdgeList[hg]]


AdjacencyHypergraph[vs_List, t_ ? ArrayQ] := Block[{dims = Dimensions[t], st, d},
	d = First[dims, 0];
	If[d == 0, Return[{}]];
	(
		st = SparseArray[t];
		Hypergraph @ Catenate @ MapThread[Table[vs[[#1]], #2] & , {DeleteCases[t["ExplicitPositions"], d, {2}], t["ExplicitValues"]}]
	) /; Equal @@ dims && d == Length[vs] + 1
]

AdjacencyHypergraph[t_] := AdjacencyHypergraph[Range[Length[t] - 1], t]


HypergraphIncidenceMatrix[vs_List, hg : {___List}] :=
	If[	Length[vs] > 0,
		SparseArray @ Transpose[Total[2 ^ (# - 1)] & /@ Lookup[PositionIndex[#], vs, {}] & /@ hg],
		{{}}
	]

HypergraphIncidenceMatrix[hg : {___List}] := HypergraphIncidenceMatrix[Union @@ hg, hg]

HypergraphIncidenceMatrix[hg_Hypergraph] := HypergraphIncidenceMatrix[VertexList[hg], EdgeList[hg]]


IncidenceHypergraph[vs_List, mat_ ? MatrixQ] :=
	With[{rules = Catenate @ MapThread[Thread @* Rule, {Position[Reverse[#], 1, {1}, Heads -> False] & /@ IntegerDigits[#, 2], vs}]},
		If[rules === {}, {}, Normal @ SparseArray[rules]]
	] & /@ Transpose[mat] // Hypergraph

IncidenceHypergraph[mat_] := IncidenceHypergraph[Range[Length[mat]], mat]



(* HyperMatrix *)

HyperMatrixQ[_HyperMatrix ? System`Private`HoldValidQ] := True

HyperMatrixQ[___] := False


HyperMatrix[vs_List, edges : {(_List | _Rule) ...}, symm : {___Rule}] := Block[{n, index},
	n = Length[vs];
	index = First /@ PositionIndex[vs];
	HyperMatrix @@ KeyValueMap[
		#1 -> If[#1[[1]] == 0, Length[#2], SparseArray[Normal @ Counts[Partition[Lookup[index, Catenate[Replace[#2, (e_ -> tag_) :> e, {1}]]], #1[[1]]]], Table[n, #1[[1]]]]] &,
		KeySort @ GroupBy[edges, Through @* {Length, Lookup[symm, Key[#]] &}]
	]
]

HyperMatrix[hg : {___List}] := HyperMatrix[Union @@ hg, hg]

HyperMatrix[hg_Hypergraph] := HyperMatrix[VertexList[hg], hg["EdgeListTagged"], EdgeSymmetry[hg]]

hm : HyperMatrix[rules : ({_Integer, {___Cycles}} -> _Integer | _SparseArray) ...] /;
	System`Private`HoldNotValidQ[hm] && Equal @@ Catenate[Dimensions /@ {rules}[[All, 2]]] :=
	System`Private`HoldSetValid[hm]


(hm_HyperMatrix ? HyperMatrixQ)["Association"] := Association @@ hm

(hm_HyperMatrix ? HyperMatrixQ)["Arrays"] := (List @@ hm)[[All, 2]]

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
		Hypergraph[
			Range[n],
			Flatten[
				KeyValueMap[
					If[IntegerQ[#2] && #2 > 0, Table[vs[[#1]], #2], Nothing] &,
					If[ArrayQ[#], Association @ ArrayRules[#], <|{} -> #|>]
				] & /@ hm,
				2
			]
		]
	) /; Equal @@ Prepend[Catenate[dims], n]
]

HyperMatrixGraph[hm_List] := With[{dims = Catenate[Dimensions /@ hm]},
	HyperMatrixGraph[Range[First[dims]], hm] /; Equal @@ dims
]

HyperMatrixGraph[hm_HyperMatrix ? HyperMatrixQ] := HyperMatrixGraph[hm["Arrays"]]



HypergraphTransitionMatrix[hg_Hypergraph] := Block[{e, c, a, t},
	e = Sign @ HypergraphIncidenceMatrix[hg];
	c = Transpose[e] . e;
	a = e . Transpose[e];
	t = e . DiagonalMatrix[Diagonal[c]] . Transpose[e] - a;
	t / Total[t, {2}]
]

