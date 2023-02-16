Package["WolframInstitute`Hypergraph`"]

PackageExport["Hyperedges"]
PackageExport["Hypergraph"]



(* Hyperedges *)


Hyperedges[0] := Hyperedges[]

Hyperedges[1] := Hyperedges[{}]


NonCommutativeMultiply[hs___Hyperedges] ^:= Hyperedges @@ Join @@@ Tuples[{hs}]

Plus[hs___Hyperedges] ^:= Join[hs]


(* hg : Hyperedges[edges___List] := With[{pos = Position[{edges}, {___, x_, x_, ___}, {1}, Heads -> False]},
	MapAt[SequenceReplace[{x_, x_} -> Nothing], Unevaluated[hg], pos] /; pos =!= {}
] *)


Inverse[hg_Hyperedges] ^:= Reverse /@ Reverse[hg]


Hyperedges /: MakeBoxes[hg : Hyperedges[args___], form_] := With[{
	boxes = Replace[Hold[args], {
		Hold[] -> 0,
		Hold[{}] -> "()",
		Hold[edge_List] :> RowBox[{"(", Splice @ Riffle[edge, "\[VeryThinSpace]"], ")"}],
		Hold[edges__List] :> RowBox[Riffle[ToBoxes[Hyperedges[#], form] & /@ {edges}, "\[CirclePlus]"]]
	}]
},
	InterpretationBox[boxes, hg]
]



(* Hypergraph *)

Hypergraph[edgeSpec_] := Hypergraph[If[MatchQ[edgeSpec, {___List}], Hyperedges @@ edgeSpec, Hyperedges[edgeSpec]], Association[]]


hg_Hypergraph[prop_String, args___] := HypergraphProp[hg, prop, args]


HypergraphProp[Hypergraph[edges_, _], "Edges"] := edges

HypergraphProp[Hypergraph[_, symm_], "Symmetry"] := symm

HypergraphProp[hg_, "EdgeList"] := List @@ hg["Edges"]

HypergraphProp[hg_, "VertexList"] := Union @@ hg["EdgeList"]


NonCommutativeMultiply[hs___Hypergraph] ^:= Hypergraph[
	Through[Unevaluated @ NonCommutativeMultiply[hs]["Edges"]],
	Merge[Through[Unevaluated @ {hs}["Symmetry"]], Identity]
]

Plus[hs___Hypergraph] ^:= Hypergraph[
	Through[Unevaluated @ Plus[hs]["Edges"]],
	Through[Unevaluated @ Join[hs]["Symmetry"]]
]



