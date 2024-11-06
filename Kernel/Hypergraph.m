Package["WolframInstitute`Hypergraph`"]

PackageExport["HyperedgesQ"]
PackageExport["HypergraphQ"]

PackageExport["Hyperedges"]
PackageExport["CyclicEdge"]
PackageExport["Hypergraph"]
PackageExport["Hypergraph3D"]

PackageScope["$DefaultHypergraphAnnotations"]
PackageScope["$VertexAnnotations"]
PackageScope["$EdgeAnnotations"]
PackageScope["makeAnnotationRules"]



(* Annotations *)

$DefaultHypergraphAnnotations = <|
    VertexStyle -> {},
    VertexLabels -> {"Name", None},
    VertexLabelStyle -> {},
	VertexSize -> Automatic,
	VertexCoordinates -> Automatic,
	VertexShapeFunction -> Function[{PointSize[Replace[#3, {Automatic -> 3, {x_, _} :> x}]], Point[#1]}],

    EdgeStyle -> {Automatic, Inherited},
	"EdgeLineStyle" -> Automatic,
    EdgeLabels -> {"Name", None},
    EdgeLabelStyle -> {},
	"EdgeSize" -> Automatic,
    "EdgeSymmetry" -> "Unordered",
	"EdgeType" -> "Bag"
|>

$VertexAnnotations = {VertexStyle, VertexLabels, VertexLabelStyle, VertexSize, VertexCoordinates, VertexShapeFunction};
$EdgeAnnotations = {EdgeStyle, "EdgeLineStyle", EdgeLabels, EdgeLabelStyle, "EdgeSize", "EdgeSymmetry"};


getDefault[value_, def_ : None] := Replace[value, {_, default_} | default_ :> Replace[def, None -> default]]

makeAnnotationRules[opts_List, keys_ : All] := With[{
	customAnnotations = Flatten @ Values @ FilterRules[opts, {"VertexAnnotationRules", "EdgeAnnotationRules"}]
}, {
	annotations = Join[FilterRules[opts, Except["VertexAnnotationRules" | "EdgeAnnotationRules"]], customAnnotations]
},
	KeyValueMap[
		#1 -> With[{automatic = Replace[#2, {automatic_, _} | automatic_ :> automatic], default = getDefault[#2]},
			Replace[
				Flatten[ReplaceList[#1, FilterRules[annotations, #1]]],
				{
					Automatic -> All -> automatic,
					(k_ -> Except[Automatic | None, default]) :> k -> Inherited,
					s : Except[_Rule | _RuleDelayed] :> All -> s
				},
				{1}
			] // filterRulesByAll
		] &,
		Association[$DefaultHypergraphAnnotations, # -> Automatic & /@ DeleteDuplicates @ Keys[customAnnotations]] //
			If[keys === All, #, #[[ Key /@ Developer`ToList[keys] ]]] &
	] // If[MatchQ[keys, _List | All], Association, #[[1, 2]] &]
]


(* Hyperedges *)


HyperedgesQ[he_Hyperedges] := System`Private`HoldValidQ[he]

HyperedgesQ[___] := False

Hyperedges[0] := Hyperedges[]

Hyperedges[1] := Hyperedges[{}]


(he : Hyperedges[edges___]) /; ! HyperedgesQ[Unevaluated[he]] := Function[Null, System`Private`HoldSetValid[Hyperedges[##]], HoldAll] @@ Map[prepEdge, {edges}]

(he : HoldPattern[Hyperedges[edges___]])["EdgeListTagged"] /; HyperedgesQ[he] := {edges}

(he : HoldPattern[Hyperedges[edges___]])["EdgeList"] /; HyperedgesQ[he] := Replace[{edges}, (edge_ -> _) :> Developer`ToList[edge], {1}]

annotationHead = Labeled | Style | Annotation
stripAnnotations[expr_] := Replace[expr, annotationHead[x_, __] :> x, If[ListQ[expr], 1, {}]]

he_Hyperedges["VertexList"] /; HyperedgesQ[he] := DeleteDuplicates @ stripAnnotations @ Catenate @ he["EdgeList"]

he_Hyperedges["AnnotatedVertexList"] /; HyperedgesQ[he] := Replace[GatherBy[Catenate @ he["EdgeList"], stripAnnotations], {{___, x : annotationHead[__], ___} :> x, {x_, ___} :> x}, 1]

NonCommutativeMultiply[hs___Hyperedges ? HyperedgesQ] ^:= Hyperedges @@ Join @@@ Tuples[HyperEdges @@@ Through[{hs}["EdgeList"]]]

Plus[hs___Hyperedges ? HyperedgesQ] ^:= Join[hs]


(* hg : Hyperedges[edges___List] := With[{pos = Position[{edges}, {___, x_, x_, ___}, {1}, Heads -> False]},
	MapAt[SequenceReplace[{x_, x_} -> Nothing], Unevaluated[hg], pos] /; pos =!= {}
] *)


Inverse[hg_Hyperedges ? HyperedgesQ] ^:= Reverse /@ Reverse[hg]


Hyperedges /: MakeBoxes[hg : Hyperedges[args___] /; HyperedgesQ[Unevaluated[hg]], form_] := With[{
	boxes = Replace[Hold[args], {
		Hold[] -> 0,
		Hold[{}] -> "()",
		Hold[edge_List -> tag_] :> TooltipBox[RowBox[{"(", Splice @ Riffle[ToBoxes[#, form] & /@ edge, "\[VeryThinSpace]"], ")"}], ToBoxes[tag, form]],
		Hold[edge_List] :> RowBox[{"(", Splice @ Riffle[ToBoxes[#, form] & /@ edge, "\[VeryThinSpace]"], ")"}],
		Hold[edges__] :> RowBox[Riffle[ToBoxes[Hyperedges[#], form] & /@ {edges}, "\[CirclePlus]"]]
	}]
},
	InterpretationBox[boxes, hg]
]



(* Hypergraph *)


HypergraphQ[hg_Hypergraph] := System`Private`HoldValidQ[hg] ||
	MatchQ[Unevaluated[hg], Hypergraph[_List, _ ? HyperedgesQ, OptionsPattern[]]]

HypergraphQ[___] := False



(* Constructors *)

$EdgeHead = DirectedEdge | UndirectedEdge | CyclicEdge
$EdgePattern = _List | _Rule | $EdgeHead[__] | Labeled[_List | _Rule | $EdgeHead[__], __] | Style[_List | _Rule | $EdgeHead[__], __] | Annotation[_List | _Rule | $EdgeHead[__], ___]

EdgeType[edge_, type_] := Annotation[edge, "EdgeType" -> type]
EdgeSymmetry[edge_, type_] := Annotation[edge, "EdgeSymmetry" -> type]

$EdgeHeadSymmetryRules = {DirectedEdge -> "Ordered", UndirectedEdge -> "Unordered", CyclicEdge -> "Cyclic"}

EdgeAnnotate[spec_, data___] := Replace[spec, {
	Annotation[e_, xs___] :> Replace[EdgeAnnotate[e, data], Annotation[y_, ys___] :> Annotation[y, ys, xs]],
	Labeled[e_, label_, ___] :> EdgeAnnotate[e, EdgeLabels -> label],
	Style[e_, styles__] :> EdgeAnnotate[e, EdgeStyle -> Flatten[{styles}]],
	(head : $EdgeHead)[from_, to_, tag_] :> EdgeAnnotate[EdgeSymmetry[Join[Developer`ToList[from], Developer`ToList[to]] -> tag, Replace[head, $EdgeHeadSymmetryRules]], data],
	(head : $EdgeHead)[from_, to_] :> EdgeAnnotate[EdgeSymmetry[Join[Developer`ToList[from], Developer`ToList[to]], Replace[head, $EdgeHeadSymmetryRules]], data],
	(head : $EdgeHead)[from_] :> EdgeAnnotate[EdgeSymmetry[Developer`ToList[from], Replace[head, $EdgeHeadSymmetryRules]], data],
	Rule[from : Except[_List], to_] :> EdgeAnnotate[DirectedEdge[from, to], data],
	_ :> Annotation[spec, data]
}]

prepEdge = Replace[{edge : Rule[_List, _] | _List :> edge, rule : Rule[Except[_List], _] :> List @@ rule, edge : Except[_List] :> {edge}}]

EdgeSpecAnnotation[spec_] := Replace[
	EdgeAnnotate[spec],
	Annotation[e_, data___] :> prepEdge[e] -> Cases[Flatten[{data}], _Rule | _RuleDelayed]
]

extractEdgeAnnotations[edgeSpec : {$EdgePattern ...}] :=  With[{
	annotations = EdgeSpecAnnotation /@ edgeSpec
},
	{annotations[[All, 1]], (key |-> key -> Map[#[[1]] -> Lookup[#[[2]], key, getDefault[Lookup[$DefaultHypergraphAnnotations, key], Inherited]] &, annotations]) /@ DeleteDuplicates[Keys @ Catenate @ annotations[[All, 2]]]}
]

filterRulesByAll[rules_] := Take[rules, UpTo[LengthWhile[rules, #[[1]] =!= All &] + 1]]

Hypergraph[vs_List, edgeSpec : {$EdgePattern ...}, opts : OptionsPattern[]] := Block[{edges, annotations},
	{edges, annotations} = extractEdgeAnnotations[edgeSpec];
	Hypergraph[vs, Hyperedges @@ edges, annotations, opts]
]

Hypergraph[edgeSpec : {$EdgePattern ...}, opts : OptionsPattern[]] := Hypergraph[{}, edgeSpec, opts]

(* no vertices and empty options case *)
Hypergraph[edges : {__List}, {}] := Hypergraph[{}, edges]

Hypergraph[vs_List, opts : OptionsPattern[]] := Hypergraph[vs, {}, opts]

Hypergraph[g_ ? GraphQ, opts : OptionsPattern[]] := Hypergraph[VertexList[g], EdgeList[g], opts]

Hypergraph[] := Hypergraph[0]

hg : Hypergraph[edgeSpec_, opts : OptionsPattern[]] := Enclose @ With[{edges = ConfirmBy[Hyperedges[edgeSpec], HyperedgesQ]},
	Hypergraph[edges["VertexList"], edges, opts]
]

Hypergraph[he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] := Hypergraph[he["AnnotatedVertexList"], he, opts]


Hypergraph[vs_List, he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] :=
	Hypergraph[DeleteDuplicatesBy[Join[vs, he["AnnotatedVertexList"]], stripAnnotations], he, opts] /; ! ContainsAll[stripAnnotations[vs], he["VertexList"]]

hg : Hypergraph[vs_List, he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] /; System`Private`HoldNotValidQ[hg] := With[{
	vertices = DeleteDuplicates @ stripAnnotations[vs]
}, With[{
	annotations = Replace[vs, {
		Annotation[v_, data__] :> v -> Cases[Flatten[{data}], _Rule | _RuleDelayed],
		Labeled[v_, label_, ___] :> v -> {VertexLabels -> label},
		Style[v_, styles__] :> v -> {VertexStyle -> Flatten[{styles}]},
		v_ :> v -> {}
	}, 1],
	edges = he /. (Labeled | Style | Annotation)[v_, __] :> v
},
	System`Private`SetNoEntry @ System`Private`HoldSetValid[Hypergraph[vertices, edges, ##]] & @@
		Normal @ GroupBy[
			Flatten[{(key |-> key -> Map[#[[1]] -> Lookup[#[[2]], key, getDefault[Lookup[$DefaultHypergraphAnnotations, key], Inherited]] &, annotations]) /@ DeleteDuplicates[Keys @ Catenate @ annotations[[All, 2]]], opts}],
			First,
			If[
				KeyExistsQ[$DefaultHypergraphAnnotations, #[[1, 1]]],
				filterRulesByAll @ Replace[Flatten[#[[All, 2]]], s : Except[_Rule | _RuleDelayed] :> All -> s, {1}],
				#[[1, 2]]
			] &
		] 
	] /; ContainsAll[vertices, he["VertexList"]]
]



(* Properties *)

hg_Hypergraph[prop_String, args___] := HypergraphProp[hg, prop, args]


Options[Hypergraph] := Join[{
	PlotTheme -> "WolframModel",
	ColorFunction -> ColorData[97],
	"LayoutDimension" -> 2,
    "EdgeArrows" -> False,
    "EdgeType" -> "Cyclic",
	"EdgeMethod" -> "ConcavePolygon",
	"EdgeLineStyle" -> Automatic,
	"EdgeSize" -> Automatic,
	"EdgeSymmetry" -> Automatic
},
	FilterRules[Options[Graph], Except[PlotTheme]]
]

Options[Hypergraph3D] := Options[Hypergraph]

Hypergraph3D[args___, opts : OptionsPattern[]] := Hypergraph[args, Sequence @@ FilterRules[{"LayoutDimension" -> 3, opts}, Options[Hypergraph]]]

Hypergraph3D[hg_Hypergraph, opts : OptionsPattern[]] := Hypergraph3D[hg["VertexList"], hg["EdgeListTagged"], opts,
	"LayoutDimension" -> 3,
	(* VertexCoordinates -> Replace[OptionValue[Hypergraph, hg["Options"], VertexCoordinates], rules : {___Rule} :> Replace[rules, {(v_ -> c_) :> v -> Append[c, Automatic], c_ :> Append[c, Automatic]}, {1}]], *)
	FilterRules[hg["Options"], Except["LayoutDimension" | VertexCoordinates]]
]

Hypergraph[hg_Hypergraph, opts : OptionsPattern[]] := Hypergraph[VertexList[hg], EdgeListTagged[hg], opts, "LayoutDimension" -> 2, Options[hg]]

HypergraphProp[_, "Properties"] := {"Options", "AbsoluteOptions", "Verteces", "Edges", "VertexList", "EdgeList", "EdgeTags", "EdgeListTagged", "EdgeSymmetry", "FullEdgeSymmetry", "Arity"}

HypergraphProp[Hypergraph[_, _, opts___], "Options"] :=
	MapAt[Replace[rules : {(_Rule | _RuleDelayed) ...} :> filterRulesByAll[rules]], Flatten[{opts}], {All, 2}]

HypergraphProp[Hypergraph[vertices_, ___], "Vertices" | "VertexList"] := vertices

HypergraphProp[Hypergraph[_, edges_, ___], "Edges"] := edges


applyRules[expr_, rules_, length_Integer, default_] :=
	Map[
		FirstCase[#, Except[Inherited], default, {1}] &,
		Thread @ Values @ GroupBy[rules, First, PadRight[#, length, Replace[#, {} -> Inherited]] & @ReplaceList[expr, Replace[#, h_[All, rhs_] :> h[_, rhs], {1}], length] &]
	] // PadRight[#, length, {default}] &

HypergraphProp[hg_, "AbsoluteOptions", patt___] := Block[{
	vertices = VertexList[hg], edges = EdgeListTagged[hg],
	themeOpts, opts = Join[Options[hg, patt], Options[Hypergraph]],
	annotationRules, vertexAnnotations, edgeAnnotations,
	edgeCounts
},
	themeOpts = FilterRules[opts, PlotTheme];
	opts = Normal @ GroupBy[
		FixedPoint[Replace[#, (PlotTheme -> theme_) :> Splice @ Lookup[$HypergraphPlotThemes, theme, {}], {1}] &, opts],
		First,
		If[MemberQ[Join[$VertexAnnotations, $EdgeAnnotations], #[[1, 1]]], Flatten[{#[[All, 2]]}], #[[1, 2]]] &
	];
	annotationRules = makeAnnotationRules[opts];
	edgeCounts = Counts[edges];
	vertexAnnotations = Join[$VertexAnnotations, DeleteDuplicates @ Keys @ Lookup[opts, "VertexAnnotationRules", {}]];
	edgeAnnotations = Join[$EdgeAnnotations, DeleteDuplicates @ Keys @ Lookup[opts, "EdgeAnnotationRules", {}]];
	Join[
		KeyValueMap[
			{name, rules} |-> With[{
				default = getDefault[Lookup[$DefaultHypergraphAnnotations, name, None], Lookup[rules, _, None]]
			},
				name -> Map[v |-> (v -> First @ applyRules[v, rules, 1, default]), vertices]
			],
			AssociationThread[vertexAnnotations -> Lookup[annotationRules, vertexAnnotations]]
		],
		KeyValueMap[
			{name, rules} |-> With[{
				default = getDefault[Lookup[$DefaultHypergraphAnnotations, name, None], Lookup[rules, _, None]]
			},
				name -> Permute[#, FindPermutation[#[[All, 1]], edges]] & @ Catenate @ KeyValueMap[
					{e, count} |-> (e -> # & /@ If[	MatchQ[e, _Rule],
						PadRight[
							Join[
								DeleteCases[applyRules[e, rules, count, default], default],
								applyRules[e[[1]], rules, count, default]
							],
							count,
							{default}
						],
						applyRules[e, rules, count, default]
					]),
					edgeCounts
				]
			],
			AssociationThread[edgeAnnotations -> Lookup[annotationRules, edgeAnnotations]]
		],
		DeleteDuplicatesBy[First] @ FilterRules[opts, Except[Join[vertexAnnotations, edgeAnnotations]]],
		themeOpts
	]
]

HypergraphProp[hg_, "EdgeSymmetry"] := Values @ Lookup[AbsoluteOptions[hg], "EdgeSymmetry"]

HypergraphProp[hg_, "EdgeListTagged"] := hg["Edges"]["EdgeListTagged"]

HypergraphProp[hg_, "EdgeTags"] := Replace[hg["EdgeListTagged"], {(_ -> tag_) :> tag, _ -> None}, {1}]

HypergraphProp[hg_, "EdgeList"] := hg["Edges"]["EdgeList"]

HypergraphProp[hg_, "VertexList"] := hg["Edges"]["VertexList"]

HypergraphProp[hg_, "Arity"] := Length /@ hg["EdgeList"]


findMinGenSet[groupGens_, order : _Integer | Automatic : Automatic] :=
	Sort @ First[FixedPoint[findSmallerGenSet[Replace[order, Automatic :> GroupOrder[PermutationGroup[groupGens]]]], {groupGens, Length[groupGens]}]]

findSmallerGenSet[order_][{genSet_, len_}] := First[
	Replace[
		ResourceFunction["SelectSubsets"][genSet, {Max[len - 1, 0]}, Function[subset, GroupOrder[PermutationGroup[subset]] === order], 1],
		subset_ :> {subset, Max[len - 1, 0]},
		{1}
	],
	{genSet, len}
]

HypergraphProp[hg_, "FullEdgeSymmetry"] := With[{
	cycles = Replace[
		hg["EdgeSymmetry"],
		{
			"Directed" | "Ordered" :> ({} &),
			"Cyclic" :> ({Cycles[{Range[Length[#]]}]} &),
			cycles : {___Cycles} :> (cycles &),
			_ :> (Cycles[{#}] & /@ Subsets[Range[Length[#]], {2}] &)
		},
		{1}
	]
},
	MapThread[findMinGenSet[#1[#2]] &, {cycles, EdgeList[hg]}]
]



(* Values *)

Plus[hs___Hypergraph] ^:= HypergraphUnion[hs]

Times[hs___Hypergraph] ^:= HypergraphHadamardProduct[hs]

NonCommutativeMultiply[hs___Hypergraph] ^:= HypergraphHadamardProduct[hs]

