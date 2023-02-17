Package["WolframInstitute`Hypergraph`"]

PackageExport["HyperedgesQ"]
PackageExport["HypergraphQ"]

PackageExport["Hyperedges"]
PackageExport["Hypergraph"]



(* Hyperedges *)


HyperedgesQ[Hyperedges[(_List | _Rule)...]] := True

HyperedgesQ[___] := False


Hyperedges[0] := Hyperedges[]

Hyperedges[1] := Hyperedges[{}]


(he : Hyperedges[edges___])["EdgeList"] /; HyperedgesQ[Unevaluated[he]] := Replace[{edges}, (edge_ -> _) :> edge, {1}]

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


HypergraphQ[hg : Hypergraph[_Hyperedges ? HyperedgesQ, _Association ? AssociationQ]] := System`Private`HoldValidQ[hg]

HypergraphQ[___] := False



Hypergraph[edgeSpec : {___List}, symm : _ ? AssociationQ : <||>] := Hypergraph[Hyperedges @@ edgeSpec, symm]

hg : Hypergraph[edgeSpec_] := Enclose @ Hypergraph[ConfirmBy[Hyperedges[edgeSpec], HyperedgesQ]]

hg : Hypergraph[he_Hyperedges ? HyperedgesQ, symm : _ ? AssociationQ : <||>] /; System`Private`HoldNotValidQ[hg] :=
	System`Private`HoldSetValid[Hypergraph[he, symm]]



hg_Hypergraph[prop_String, args___] := HypergraphProp[hg, prop, args]


HypergraphProp[Hypergraph[edges_, _], "Edges"] := edges

HypergraphProp[Hypergraph[_, symm_], "Symmetry"] := <|symm, _ -> "Unordered"|>

HypergraphProp[hg_, "EdgeList"] := List @@ hg["Edges"]

HypergraphProp[hg_, "VertexList"] := Union @@ hg["EdgeList"]

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


NonCommutativeMultiply[hs___Hypergraph] ^:= Hypergraph[
	Through[Unevaluated @ NonCommutativeMultiply[hs]["Edges"]],
	Merge[Through[Unevaluated @ {hs}["Symmetry"]], Identity]
]

Plus[hs___Hypergraph] ^:= Hypergraph[
	Through[Unevaluated @ Plus[hs]["Edges"]],
	Through[Unevaluated @ Join[hs]["Symmetry"]]
]



