Package["WolframInstitute`Hypergraph`"]


PackageExport["AdjacencyTensor"]
PackageExport["AdjacencyHypergraph"]
PackageExport["HypergraphIncidenceMatrix"]
PackageExport["IncidenceHypergraph"]

PackageExport["HypermatrixQ"]
PackageExport["Hypermatrix"]
PackageExport["HypermatrixGraph"]

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



(* Hypermatrix *)

HypermatrixQ[_Hypermatrix ? System`Private`HoldValidQ] := True

HypermatrixQ[___] := False


Hypermatrix[vs_List, edges : {(_List | _Rule) ...}, symm : {{___Cycles}...}] := Block[{n, index},
	n = Length[vs];
	index = First /@ PositionIndex[vs];
	Hypermatrix @@ KeyValueMap[
		#1 -> If[#1[[1]] == 0, Length[#2], SparseArray[Normal @ Counts[Partition[Lookup[index, Catenate[Replace[#2, (e_ -> _) :> e, {1}]]], #1[[1]]]], Table[n, #1[[1]]]]] &,
		KeySort @ GroupBy[Thread[{edges, symm}], {Length[Replace[#[[1]], (edge_ -> _) :> edge]], #[[2]]} &, Map[First]]
	]
]

Hypermatrix[hg : {___List}] := Hypermatrix[Union @@ hg, hg]

Hypermatrix[hg_Hypergraph] := Hypermatrix[VertexList[hg], EdgeListTagged[hg], EdgeSymmetry[hg]]

hm : Hypermatrix[rules : ({_Integer, {___Cycles}} -> _Integer | _SparseArray) ...] /;
	System`Private`HoldNotValidQ[hm] && Equal @@ Catenate[Dimensions /@ {rules}[[All, 2]]] :=
	System`Private`HoldSetValid[hm]


(hm_Hypermatrix ? HypermatrixQ)["Association"] := Association @@ hm

(hm_Hypermatrix ? HypermatrixQ)["Arrays"] := (List @@ hm)[[All, 2]]

(hm_Hypermatrix ? HypermatrixQ)["Dimensions"] := Dimensions /@ hm["Arrays"]


Plus[hms___Hypermatrix ? HypermatrixQ] ^:= Block[{arrays = Catenate @ Through[{hms}["Arrays"]], dim},
	dim = Max[Dimensions /@ arrays];
	Hypermatrix @@ Values[GroupBy[PadRight[#, ConstantArray[dim, ArrayDepth[#]]] & /@ arrays, Dimensions, Total]]
]


$HypermatrixIcon = Deploy @ GraphicsRow[
	ArrayPlot3D[#, Boxed -> False] & /@ {
		Array[If[#1 == #2 == 3, 1, 0] &, {5, 5, 5}],
		Array[If[#1 == 3, 1, 0] &, {5, 5, 5}],
		Array[1 &, {5, 5, 5}]}, 0,
		ImageSize -> Tiny
	]

Hypermatrix /: MakeBoxes[hm_Hypermatrix ? HypermatrixQ, form : TraditionalForm] := With[{
	boxes = RowBox[ToBoxes[Normal[#], TraditionalForm] & /@ hm["Arrays"]]
},
	InterpretationBox[boxes,hm]
]

Hypermatrix /: MakeBoxes[hm_Hypermatrix ? HypermatrixQ, form_] := BoxForm`ArrangeSummaryBox[
	"Hypermatrix",
	hm,
	$HypermatrixIcon,
	{{}},
	{{BoxForm`SummaryItem[{"Dimensions: ", Row[Dimensions /@ List @@ hm, ";"]}]}},
	form,
	"Interpretable" -> Automatic
]


HypermatrixGraph[vs_List, hm_List, keys_List] := Block[{n = Length[vs], dims = Dimensions /@ hm, edges},
	(
		edges = KeyValueMap[
			If[IntegerQ[#2] && #2 > 0, Table[vs[[#1]], #2], Nothing] &,
			If[ArrayQ[#], Association @ ArrayRules[#], <|{} -> #|>]
		] & /@ hm;
		Hypergraph[
			Range[n],
			Flatten[edges, 2],
			"EdgeSymmetry" -> Catenate @ MapThread[Thread[Rule[Catenate[#1], #2], List, {1}] &, {edges, keys[[All, 2]]}]
		]
	) /; Equal @@ Prepend[Catenate[dims], n]
]

HypermatrixGraph[hm_Association] := With[{dims = Catenate[Dimensions /@ hm]},
	HypermatrixGraph[Range[First[dims]], Values[hm], Keys[hm]] /; Equal @@ dims
]

HypermatrixGraph[hm_Hypermatrix ? HypermatrixQ] := HypermatrixGraph[hm["Association"]]



HypergraphTransitionMatrix[hg_Hypergraph] := Block[{e, c, a, t},
	e = Sign @ HypergraphIncidenceMatrix[hg];
	c = Transpose[e] . e;
	a = e . Transpose[e];
	t = e . DiagonalMatrix[Diagonal[c]] . Transpose[e] - a;
	t / Total[t, {2}]
]

