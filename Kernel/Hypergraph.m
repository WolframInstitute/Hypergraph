Package["WolframInstitute`Hypergraph`"]

PackageExport["HyperedgesQ"]
PackageExport["HypergraphQ"]

PackageExport["Hyperedges"]
PackageExport["Hypergraph"]
PackageExport["Hypergraph3D"]

PackageScope["$DefaultHypergraphAnnotations"]
PackageScope["$VertexAnnotations"]
PackageScope["$EdgeAnnotations"]
PackageScope["makeAnnotationRules"]
PackageScope["applyIndexedRules"]



(* Annotations *)

$DefaultHypergraphAnnotations = <|
    VertexStyle -> {},
    VertexLabels -> {"Name", None},
    VertexLabelStyle -> {},
	VertexSize -> Automatic,
	VertexCoordinates -> Automatic,
	VertexShapeFunction -> Function[{AbsolutePointSize[Replace[#3, Automatic -> 3]], Point[#1]}],

    EdgeStyle -> Automatic,
	"EdgeLineStyle" -> Automatic,
    EdgeLabels -> {"Name", None},
    EdgeLabelStyle -> {},
	"EdgeSize" -> Automatic,
    "EdgeSymmetry" -> "Unordered"
|>

$VertexAnnotations = {VertexStyle, VertexLabels, VertexLabelStyle, VertexSize, VertexCoordinates, VertexShapeFunction};
$EdgeAnnotations = {EdgeStyle, "EdgeLineStyle", EdgeLabels, EdgeLabelStyle, "EdgeSize", "EdgeSymmetry"};


makeAnnotationRules[opts_List, keys_ : All] := If[MatchQ[keys, _List | All], Association, #[[1, 2]] &] @ KeyValueMap[
    #1 -> Block[{automatic, default},
        If[ MatchQ[#2, {_, _}],
            {automatic, default} = #2,
            automatic = default = #2
        ];
        DeleteDuplicatesBy[Replace[{(Verbatim[_] -> _) :> _, _ :> Unique[]}]] @
            Append[Replace[Flatten[ReplaceList[#1, FilterRules[opts, #1]]], {Automatic -> _ -> automatic, s : Except[_Rule | _RuleDelayed] :> _ -> s}, {1}], _ -> default]
    ] &,
    If[keys === All, $DefaultHypergraphAnnotations, $DefaultHypergraphAnnotations[[ Key /@ Developer`ToList[keys] ]]]
]

applyIndexedRules[expr_, rules_, index_Integer, default_ : None] := Enclose[
    GroupBy[rules, First, If[Length[#] >= index, Return[#[[index]], CompoundExpression], If[Length[#] > 0, Return[Last[#], CompoundExpression]]] & @ ReplaceList[expr, #, index] &];
    default
]

applyRules[expr_, rules_, length_Integer] := Take[Catenate @ Values @ GroupBy[rules, First, PadRight[#, length, #] & @ ReplaceList[expr, #, length] &], UpTo[length]]


(* Hyperedges *)


HyperedgesQ[Hyperedges[(_List | _Rule)...]] := True

HyperedgesQ[___] := False


Hyperedges[0] := Hyperedges[]

Hyperedges[1] := Hyperedges[{}]


(he : Hyperedges[edges___])["EdgeListTagged"] /; HyperedgesQ[Unevaluated[he]] := {edges}

(he : Hyperedges[edges___])["EdgeList"] /; HyperedgesQ[Unevaluated[he]] := Replace[{edges}, (edge_ -> _) :> Developer`ToList[edge], {1}]

(he : Hyperedges[edges___])["VertexList"] /; HyperedgesQ[Unevaluated[he]] := DeleteDuplicates @ Catenate @ he["EdgeList"]

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

$EdgePattern = _List | _Rule | Labeled[_List | _Rule, _] | Style[_List | _Rule, _] | Annotation[_List | _Rule, _]

extractEdgeAnnotations[edgeSpec : {$EdgePattern ...}] :=  With[{
	labels = Cases[edgeSpec, l_Labeled :> Rule @@ l],
	styles = Cases[edgeSpec, s_Style :> Rule @@ s],
	annotations = Cases[edgeSpec, Annotation[e_, data_] :> Map[#[[1]] -> ReplacePart[#, 1 -> e] &, Cases[Flatten[{data}], _Rule | _RuleDelayed]]],
	edges = Replace[edgeSpec, (Labeled | Style | Annotation)[e_, _] :> e, {1}]
},
	{edges, {If[styles === {}, Nothing, EdgeStyle -> styles], If[labels === {}, Nothing, EdgeLabels -> labels], annotations}}
]

Hypergraph[vs_List, edgeSpec : {$EdgePattern ...}, opts : OptionsPattern[]] := Block[{edges, annotations},
	{edges, annotations} = extractEdgeAnnotations[edgeSpec];
	Hypergraph[vs, Hyperedges @@ edges, annotations, opts]
]

Hypergraph[edgeSpec : {$EdgePattern ...}, opts : OptionsPattern[]] := Hypergraph[{}, edgeSpec, opts]

Hypergraph[vs_List, opts : OptionsPattern[]] := Hypergraph[vs, {}, opts]

Hypergraph[] := Hypergraph[0]

hg : Hypergraph[edgeSpec_, opts : OptionsPattern[]] := Enclose @ With[{edges = ConfirmBy[Hyperedges[edgeSpec], HyperedgesQ]},
	Hypergraph[edges["VertexList"], edges, opts]
]

Hypergraph[he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] := Hypergraph[he["VertexList"], he, opts]


hg : Hypergraph[vs_List, he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] /;
	! ContainsAll[vs, he["VertexList"]] := Hypergraph[Join[vs, DeleteElements[he["VertexList"], vs]], he, opts]

hg : Hypergraph[vs_List, he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] /;
	ContainsAll[vs, he["VertexList"]] && System`Private`HoldNotValidQ[hg] := System`Private`SetNoEntry @ With[{
		labels = Cases[vs, l_Labeled :> Rule @@ l],
		styles = Cases[vs, s_Style :> Rule @@ s],
		annotations = Cases[vs, Annotation[v_, data__] :> Map[#[[1]] -> ReplacePart[#, 1 -> v] &, Cases[Flatten[{data}], _Rule | _RuleDelayed]]],
		vertices = Replace[vs, (Labeled | Style | Annotation)[v_, _] :> v, {1}],
		edges = he /. (Labeled | Style | Annotation)[v_, _] :> v
	},
		System`Private`HoldSetValid[Hypergraph[vertices, edges, ##]] & @@
			Normal @ GroupBy[
				Flatten[{If[styles === {}, Nothing, VertexStyle -> styles], If[labels === {}, Nothing, VertexLabels -> labels], annotations, opts}],
				First,
				If[
					KeyExistsQ[$DefaultHypergraphAnnotations, #[[1, 1]]],
					Take[#, UpTo[LengthWhile[#, #[[1]] =!= _ &] + 1]] & @ Replace[Flatten[#[[All, 2]]], s : Except[_Rule | _RuleDelayed] :> _ -> s, {1}],
					#[[1, 2]]
				] &
			]
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

HypergraphProp[Hypergraph[_, _, opts___], "Options"] :=
	MapAt[Replace[rules : {(_Rule | _RuleDelayed) ...} :> DeleteDuplicatesBy[rules, Replace[{(Verbatim[_] -> _) :> _, _ :> Unique[]}]]], Flatten[{opts}], {All, 2}]

HypergraphProp[Hypergraph[vertices_, ___], "VertexList"] := vertices

HypergraphProp[Hypergraph[_, edges_, ___], "Edges"] := edges

HypergraphProp[hg_, "AbsoluteOptions", patt___] := Block[{vertices = VertexList[hg], edges = EdgeListTagged[hg], opts = Join[Options[hg, patt], Options[Hypergraph]], annotationRules, edgeCounts},
	annotationRules = makeAnnotationRules[FixedPoint[Replace[#, (PlotTheme -> theme_) :> Splice @ Lookup[$HypergraphPlotThemes, theme, {}], {1}] &, opts]];
	edgeCounts = Counts[edges];
	Join[
		KeyValueMap[
			{name, rules} |-> name -> Map[v |-> (v -> First @ applyRules[v, rules, 1]), vertices],
			AssociationThread[$VertexAnnotations -> Lookup[annotationRules, $VertexAnnotations]]
		],
		KeyValueMap[
			{name, rules} |-> name -> Permute[#, FindPermutation[#[[All, 1]], edges]] & @ Catenate @ KeyValueMap[{e, count} |-> (e -> # & /@ applyRules[e, rules, count]), edgeCounts],
			AssociationThread[$EdgeAnnotations -> Lookup[annotationRules, $EdgeAnnotations]]
		],
		Normal[KeyDrop[opts, Join[$VertexAnnotations, $EdgeAnnotations]]]
	]
]

HypergraphProp[hg_, "EdgeSymmetry"] := Values @ Lookup[AbsoluteOptions[hg], "EdgeSymmetry"]

HypergraphProp[hg_, "EdgeListTagged"] := hg["Edges"]["EdgeListTagged"]

HypergraphProp[hg_, "EdgeTags"] := Replace[hg["EdgeListTagged"], {(_ -> tag_) :> tag, _ -> None}, {1}]

HypergraphProp[hg_, "EdgeList"] := hg["Edges"]["EdgeList"]

HypergraphProp[hg_, "VertexList"] := hg["Edges"]["VertexList"]


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

NonCommutativeMultiply[hs___Hypergraph] ^:= HypergraphUnion[hs]

Plus[hs___Hypergraph] ^:= HypergraphUnion[hs]

