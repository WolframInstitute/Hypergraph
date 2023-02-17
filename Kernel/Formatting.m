
Package["WolframInstitute`Hypergraph`"]



Hypergraph /: MakeBoxes[hg_Hypergraph /; HypergraphQ[Unevaluated[hg]], form : StandardForm] := With[{
	boxes = ToBoxes[
        SimpleHypergraphPlot[hg, BaseStyle -> {GraphicsHighlightColor -> Red}],
        form
    ]
},
	hypergraphBox[boxes, hg]
]

Hypergraph /: MakeBoxes[hg_Hypergraph /; HypergraphQ[Unevaluated[hg]], form : TraditionalForm] := With[{
	edgeBoxes = ToBoxes[Tooltip[hg["Edges"], hg["Symmetry"]], form]
},
	InterpretationBox[edgeBoxes, hg]
]


(* mimicking Graph and Tree behaviour *)

SetAttributes[hypergraphBox, HoldAllComplete];
hypergraphBox[GraphicsBox[box_, opts___], hg_] := GraphicsBox[
	NamespaceBox["Hypergraph", DynamicModuleBox[{Typeset`hg = HoldComplete[hg]}, box]],
	opts
]

hypergraphBox[_, hg_] := ToBoxes[hg, TraditionalForm]


PossibleHypergraphBoxQ[HoldPattern[GraphicsBox[NamespaceBox["Hypergraph", _, ___], ___]]] := True

PossibleHypergraphBoxQ[___] := False


FromGraphicsBox[HoldPattern[GraphicsBox[NamespaceBox["Hypergraph", DynamicModuleBox[vars_, ___], ___], ___]], _] := Module[vars, Typeset`hg]

Unprotect[GraphicsBox]
With[{lhs = HoldPattern[MakeExpression[g_GraphicsBox ? PossibleHypergraphBoxQ, fmt_ ]]},
	If[	!KeyExistsQ[FormatValues[GraphicsBox], lhs],
		PrependTo[
			FormatValues[GraphicsBox],
			lhs :> FromGraphicsBox[g, fmt]
		]
	]
]
Protect[GraphicsBox]

