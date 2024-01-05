
Package["WolframInstitute`Hypergraph`"]

PackageScope["$HypergraphRuleArrow"]
PackageScope["$HypergraphRulePlotOptions"]



(* Hypergraph *)

Hypergraph /: MakeBoxes[hg_Hypergraph /; HypergraphQ[Unevaluated[hg]], form : StandardForm] := With[{
    boxId = SymbolName[Unique["Hypergraph"]]
},
    {
	boxes = Block[{BoxForm`$UseTextFormattingWhenConvertingInput = False},
        ToBoxes[
            Insert[
                SimpleHypergraphPlot[#, BaseStyle -> {
                        GraphicsHighlightColor -> Red,
                        ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[
                                MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], boxId], ToBoxes[HypergraphDraw[hg]]]]
                            ], MenuEvaluator -> Automatic]}
                        }
                    }
                ],
                BoxID -> boxId,
                -1
            ],
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
    Frame -> True,
    FrameTicks -> None,
    PlotRangePadding -> Scaled[0.1],
    ImagePadding -> 3,
    AspectRatio -> Automatic,
    ImageSize -> Tiny
};

HypergraphRule /: MakeBoxes[hr_HypergraphRule /; HypergraphRuleQ[Unevaluated[hr]], form : StandardForm] := With[{boxId = SymbolName[Unique["HypergraphRule"]]}, With[{
    boxes = Block[{BoxForm`$UseTextFormattingWhenConvertingInput = False}, ToBoxes[
        Insert[BoxID -> boxId, -1] @ GraphicsRow[{
            SimpleHypergraphPlot[#["Input"], $HypergraphRulePlotOptions],
            Graphics[{GrayLevel[0.65], $HypergraphRuleArrow}, ImageSize -> 24],
            SimpleHypergraphPlot[#["Output"], $HypergraphRulePlotOptions]
        },
            PlotRangePadding -> 1,
            BaseStyle -> {
                GraphicsHighlightColor -> Blue,
                ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[
                    MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], boxId], ToBoxes[HypergraphRuleDraw[hr]]]]],
                    MenuEvaluator -> Automatic]}}
            },
            ImageSize -> Medium
        ],
        form
    ]]
},
    hypergraphRuleBox[boxes, #]
]] & @ hr

HypergraphRule /: MakeBoxes[hr_HypergraphRule /; HypergraphRuleQ[Unevaluated[hr]], form : TraditionalForm] := With[{
    boxes = RowBox[{ToBoxes[#["Input"], form], "->", ToBoxes[#["Output"], form]}]
},
    InterpretationBox[boxes, #]
] & @ hr



SetAttributes[hypergraphRuleBox, HoldAllComplete];
hypergraphRuleBox[(head : (GraphicsBox | Graphics3DBox))[box_, opts___], hr_] := head[
	NamespaceBox["HypergraphRule", DynamicModuleBox[{Typeset`hr = HoldComplete[hr]}, box]],
	opts
]

hypergraphRuleBox[_, hr_] := ToBoxes[hr, TraditionalForm]


PossibleHypergraphRuleBoxQ[HoldPattern[(GraphicsBox | Graphics3DBox)[NamespaceBox["HypergraphRule", _, ___], ___]]] := True

PossibleHypergraphRuleBoxQ[___] := False


FromGraphicsBox[HoldPattern[(GraphicsBox | Graphics3DBox)[NamespaceBox["HypergraphRule", DynamicModuleBox[vars_, ___], ___], ___]], _] := Module[vars, Typeset`hr]

Unprotect[GraphicsBox, Graphics3DBox]
Scan[head |->
    With[{lhs = HoldPattern[MakeExpression[g_head ? PossibleHypergraphRuleBoxQ, fmt_]]},
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

