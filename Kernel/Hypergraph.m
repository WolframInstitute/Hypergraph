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

(he : Hyperedges[edges___])["EdgeList"] /; HyperedgesQ[Unevaluated[he]] := Replace[{edges}, (edge_ -> _) :> edge, {1}]

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
	MatchQ[Unevaluated[hg], Hypergraph[_List, _ ? HyperedgesQ, _Association, OptionsPattern[]]]

HypergraphQ[___] := False



(* Constructors *)

Hypergraph[vs_List, edgeSpec : {___List}, symm_Association : <||>, opts : OptionsPattern[]] := Hypergraph[vs, Hyperedges @@ edgeSpec, symm, opts]

Hypergraph[edgeSpec : {___List}, symm_Association : <||>, opts : OptionsPattern[]] := Hypergraph[Hyperedges @@ edgeSpec, symm, opts]

Hypergraph[] := Hypergraph[0]

hg : Hypergraph[edgeSpec_, symm_Association : <||>, opts : OptionsPattern[]] := Enclose @ With[{edges = ConfirmBy[Hyperedges[edgeSpec], HyperedgesQ]},
	Hypergraph[edges["VertexList"], edges, symm, opts]
]

Hypergraph[he_Hyperedges ? HyperedgesQ, symm_Association : <||>, opts : OptionsPattern[]] := Hypergraph[he["VertexList"], he, symm, opts]


hg : Hypergraph[vs_List, he_Hyperedges ? HyperedgesQ, symm_Association : <||>, opts : OptionsPattern[]] /;
	! ContainsAll[vs, he["VertexList"]] := Hypergraph[Join[vs, DeleteElements[he["VertexList"], vs]], he, symm, opts]

hg : Hypergraph[vs_List, he_Hyperedges ? HyperedgesQ, symm : _ ? AssociationQ : <||>, opts : OptionsPattern[]] /;
	ContainsAll[vs, he["VertexList"]] && System`Private`HoldNotValidQ[hg] := System`Private`SetNoEntry[
		System`Private`HoldSetValid[Hypergraph[vs, he, symm, ##]] & @@ DeleteDuplicatesBy[Flatten[{opts}], First]
	]



(* Properties *)

hg_Hypergraph[prop_String, args___] := HypergraphProp[hg, prop, args]


Options[Hypergraph] := Join[{
	 ColorFunction -> ColorData[97],
	"LayoutDimension" -> 2,
    "EdgeArrows" -> False,
    "EdgeType" -> "Cyclic"
},
	Options[Graph]
]

Options[Hypergraph3D] := Options[Hypergraph]

Hypergraph3D[args___, opts : OptionsPattern[]] := Hypergraph[args, FilterRules[{"LayoutDimension" -> 3, opts}, Options[Hypergraph]]]

Hypergraph3D[hg_Hypergraph, opts : OptionsPattern[]] := Hypergraph3D[hg["VertexList"], hg["EdgeList"], opts,
	"LayoutDimension" -> 3,
	VertexCoordinates -> Replace[OptionValue[Hypergraph, hg["Options"], VertexCoordinates], rules : {___Rule} :> Replace[rules, {(v_ -> c_) :> v -> Append[c, 0], c_ :> Append[c, 0]}, {1}]],
	FilterRules[hg["Options"], Except["LayoutDimension" | VertexCoordinates]]
]

Hypergraph[hg_Hypergraph, opts : OptionsPattern[]] := Hypergraph[hg["VertexList"], hg["EdgeList"], opts, "LayoutDimension" -> 2, hg["Options"]]

HypergraphProp[Hypergraph[_, _, _, opts___], "Options"] := Flatten[{opts}]

HypergraphProp[Hypergraph[vertices_, __], "VertexList"] := vertices

HypergraphProp[Hypergraph[_, edges_, __], "Edges"] := edges

HypergraphProp[Hypergraph[_, _, symm_, ___], "Symmetry"] := <|symm, _ -> "Unordered"|>

HypergraphProp[hg_, "EdgeListTagged"] := hg["Edges"]["EdgeListTagged"]

HypergraphProp[hg_, "EdgeTags"] := Replace[hg["EdgeListTagged"], {(_ -> tag_) :> tag, _ -> None}, {1}]

HypergraphProp[hg_, "EdgeList"] := hg["Edges"]["EdgeList"]

HypergraphProp[hg_, "VertexList"] := hg["Edges"]["VertexList"]

HypergraphProp[hg_, "EdgeSymmetry"] := With[{symmFunc = KeyValueMap[{k, v} |->
		Replace[k, All -> _] -> Replace[v, {
			"Directed" :> (Cycles[{{}}] &),
			"Cyclic" :> (Cycles[{Append[Range[Length[#]], 1]}] &),
			_ :> (Cycles[{#}] & /@ Subsets[Range[Length[#]], {2}] &)
		}],
		hg["Symmetry"]
	]
},
	AssociationMap[
		Replace[#, symmFunc][#] &,
		hg["EdgeList"]
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

