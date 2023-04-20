Package["WolframInstitute`Hypergraph`"]

PackageExport["HyperedgesQ"]
PackageExport["HypergraphQ"]

PackageExport["Hyperedges"]
PackageExport["Hypergraph"]
PackageExport["Hypergraph3D"]



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

Hypergraph[vs_List, edgeSpec : {(_List | _Rule) ...}, opts : OptionsPattern[]] := Hypergraph[vs, Hyperedges @@ edgeSpec, opts]

Hypergraph[edgeSpec : {(_List | _Rule) ...}, opts : OptionsPattern[]] := Hypergraph[Hyperedges @@ edgeSpec, opts]

Hypergraph[] := Hypergraph[0]

hg : Hypergraph[edgeSpec_, opts : OptionsPattern[]] := Enclose @ With[{edges = ConfirmBy[Hyperedges[edgeSpec], HyperedgesQ]},
	Hypergraph[edges["VertexList"], edges, opts]
]

Hypergraph[he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] := Hypergraph[he["VertexList"], he, opts]


hg : Hypergraph[vs_List, he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] /;
	! ContainsAll[vs, he["VertexList"]] := Hypergraph[Join[vs, DeleteElements[he["VertexList"], vs]], he, opts]

hg : Hypergraph[vs_List, he_Hyperedges ? HyperedgesQ, opts : OptionsPattern[]] /;
	ContainsAll[vs, he["VertexList"]] && System`Private`HoldNotValidQ[hg] := System`Private`SetNoEntry[
		System`Private`HoldSetValid[Hypergraph[vs, he, ##]] & @@ Sort @ DeleteDuplicatesBy[Flatten[{opts}], First]
	]



(* Properties *)

hg_Hypergraph[prop_String, args___] := HypergraphProp[hg, prop, args]


Options[Hypergraph] := Join[{
	 ColorFunction -> ColorData[97],
	"LayoutDimension" -> 2,
    "EdgeArrows" -> False,
    "EdgeType" -> "Cyclic",
	"EdgeSymmetry" -> Automatic
},
	Options[Graph]
]

Options[Hypergraph3D] := Options[Hypergraph]

Hypergraph3D[args___, opts : OptionsPattern[]] := Hypergraph[args, Sequence @@ FilterRules[{"LayoutDimension" -> 3, opts}, Options[Hypergraph]]]

Hypergraph3D[hg_Hypergraph, opts : OptionsPattern[]] := Hypergraph3D[hg["VertexList"], hg["EdgeList"], opts,
	"LayoutDimension" -> 3,
	VertexCoordinates -> Replace[OptionValue[Hypergraph, hg["Options"], VertexCoordinates], rules : {___Rule} :> Replace[rules, {(v_ -> c_) :> v -> Append[c, Automatic], c_ :> Append[c, Automatic]}, {1}]],
	FilterRules[hg["Options"], Except["LayoutDimension" | VertexCoordinates]]
]

Hypergraph[hg_Hypergraph, opts : OptionsPattern[]] := Hypergraph[hg["VertexList"], hg["EdgeList"], opts, "LayoutDimension" -> 2, hg["Options"]]

HypergraphProp[Hypergraph[_, _, opts___], "Options"] := Flatten[{opts}]

HypergraphProp[Hypergraph[vertices_, ___], "VertexList"] := vertices

HypergraphProp[Hypergraph[_, edges_, ___], "Edges"] := edges

HypergraphProp[hg_, "EdgeSymmetry"] := Flatten[{Replace[Lookup[hg["Options"], "EdgeSymmetry", Nothing], Automatic -> Nothing], _ -> "Unordered"}]

HypergraphProp[hg_, "EdgeListTagged"] := hg["Edges"]["EdgeListTagged"]

HypergraphProp[hg_, "EdgeTags"] := Replace[hg["EdgeListTagged"], {(_ -> tag_) :> tag, _ -> None}, {1}]

HypergraphProp[hg_, "EdgeList"] := hg["Edges"]["EdgeList"]

HypergraphProp[hg_, "VertexList"] := hg["Edges"]["VertexList"]

HypergraphProp[hg_, "FullEdgeSymmetry"] := With[{symmFunc = Map[
		Replace[#[[1]], All -> _] -> Replace[#[[2]], {
			"Directed" | "Ordered" :> ({Cycles[{}]} &),
			"Cyclic" :> ({Cycles[{}], Cycles[{Range[Length[#]]}]} &),
			cycles : {___Cycles} :> (cycles &),
			_ :> (Prepend[Cycles[{#}] & /@ Subsets[Range[Length[#]], {2}], Cycles[{}]] &)
		}] &,
		Replace[Flatten[{hg["EdgeSymmetry"]}], {Automatic -> _ -> "Unordered", s : Except[_Rule] :> _ -> s}, {1}]
	]
},
	Catenate @ KeyValueMap[{edge, multiplicity} |->
		edge -> #[edge] & /@ PadRight[#, multiplicity, #] & @ ReplaceList[edge, symmFunc],
		Counts[hg["EdgeList"]]
	]
]



(* Values *)

NonCommutativeMultiply[hs___Hypergraph] ^:= Hypergraph[
	Through[Unevaluated @ NonCommutativeMultiply[hs]["Edges"]],
	Merge[Through[{hs}["Symmetry"]], Identity],
	Normal @ Merge[Through[{hs}["Options"]], First]
]

Plus[hs___Hypergraph] ^:= Hypergraph[
	Through[Unevaluated @ Plus[hs]["Edges"]],
	Through[Unevaluated @ Join[hs]["Symmetry"]],
	Normal @ Merge[Through[{hs}["Options"]], First]
]

