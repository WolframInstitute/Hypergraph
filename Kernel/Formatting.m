
Package["WolframInstitute`Hypergraph`"]

PackageScope["$HypergraphRuleArrow"]
PackageScope["$HypergraphRulePlotOptions"]



(* Hypergraph *)

Hypergraph /: MakeBoxes[hg_Hypergraph /; HypergraphQ[Unevaluated[hg]], form : StandardForm] := With[{
	boxes = Block[{BoxForm`$UseTextFormattingWhenConvertingInput = False},
        ToBoxes[
            SimpleHypergraphPlot[#, BaseStyle -> {GraphicsHighlightColor -> Red}],
            form
        ]
    ]
},
	hypergraphBox[boxes, #]
] & @ hg

Hypergraph /: MakeBoxes[hg_Hypergraph /; HypergraphQ[Unevaluated[hg]], form : TraditionalForm] := With[{
	edgeBoxes = ToBoxes[#["Edges"], form],
    tooltip = ToBoxes[#["EdgeSymmetry"]]
},
	InterpretationBox[edgeBoxes, #, Tooltip -> tooltip]
] & @ hg


(* mimicking Graph and Tree behaviour *)

SetAttributes[hypergraphBox, HoldAllComplete];
hypergraphBox[(head : GraphicsBox | Graphics3DBox)[box_, opts___], hg_] := head[
	NamespaceBox["Hypergraph", DynamicModuleBox[{Typeset`hg = HoldComplete[hg]}, box]],
	opts
]

hypergraphBox[_, hg_] := ToBoxes[hg, TraditionalForm]


PossibleHypergraphBoxQ[HoldPattern[(GraphicsBox | Graphics3DBox)[NamespaceBox["Hypergraph", _, ___], ___]]] := True

PossibleHypergraphBoxQ[___] := False


FromGraphicsBox[HoldPattern[(GraphicsBox | Graphics3DBox)[NamespaceBox["Hypergraph", DynamicModuleBox[vars_, ___], ___], ___]], _] := Module[vars, Typeset`hg]

Unprotect[GraphicsBox, Graphics3DBox]
Scan[head |->
    With[{lhs = HoldPattern[MakeExpression[g_head ? PossibleHypergraphBoxQ, fmt_]]},
        If[	!KeyExistsQ[FormatValues[head], lhs],
            PrependTo[
                FormatValues[head],
                lhs :> FromGraphicsBox[g, fmt]
            ]
        ]
    ],
    {GraphicsBox, Graphics3DBox}
]
Protect[GraphicsBox, Graphics3DBox]


(* HypergraphRule *)

$HypergraphRuleArrow = FilledCurve[
    {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
    {{{-1., 0.1848}, {0.2991, 0.1848}, {-0.1531, 0.6363}, {0.109, 0.8982}, {1., 0.0034},
    {0.109, -0.8982}, {-0.1531, -0.6363}, {0.2991, -0.1848}, {-1., -0.1848}, {-1., 0.1848}}}
]

$HypergraphRulePlotOptions = {
    VertexLabels -> Automatic,
    Frame -> True,
    FrameTicks -> None,
    PlotRangePadding -> .2,
    ImagePadding -> 3,
    AspectRatio -> 1,
    ImageSize -> Tiny
};

HypergraphRule /: MakeBoxes[hr : HoldPattern[HypergraphRule[input_, output_]] /; HypergraphRuleQ[Unevaluated[hr]], form : StandardForm] := With[{
    boxes = ToBoxes[
        GraphicsRow[{
            SimpleHypergraphPlot[input, $HypergraphRulePlotOptions],
            Graphics[{GrayLevel[0.65], $HypergraphRuleArrow}, ImageSize -> Scaled[0.01]],
            SimpleHypergraphPlot[output, $HypergraphRulePlotOptions]
        },
            PlotRangePadding -> 1
        ],
        form
    ]
},
    InterpretationBox[boxes, hr]
]

HypergraphRule /: MakeBoxes[hr : HoldPattern[HypergraphRule[input_, output_]] /; HypergraphRuleQ[Unevaluated[hr]], form : TraditionalForm] := With[{
    boxes = RowBox[{MakeBoxes[input, form], "->", MakeBoxes[output, form]}]
},
    InterpretationBox[boxes, hr]
]

