
Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphLargeQ"]
PackageExport["SetHypergraphSummaryThresholds"]

PackageScope["$HypergraphRuleArrow"]
PackageScope["$HypergraphRulePlotOptions"]
PackageScope["$HypergraphIcon"]
PackageScope["$HypergraphSummaryThresholds"]

(* Summary box configuration *)
$HypergraphIcon := HypergraphIcon =
    SimpleHypergraphPlot[{{1, 2, 3}, {3, 4, 5, 6}, {5, 2}, {3, 4}, {1, 2}, {6, 1}, {2}}, Background -> None]

(* Thresholds for displaying summary box instead of full plot *)
$HypergraphSummaryThresholds = <|
    "MaxVertices" -> 32,
    "MaxEdges" -> 128,
    "MaxTotalElements" -> 196
|>;

(* Utility function to determine if a hypergraph should use summary display *)
HypergraphLargeQ[hg_Hypergraph ? HypergraphQ] := With[{
    vertexCount = VertexCount[hg],
    edgeCount = EdgeCount[hg]
},
    vertexCount > $HypergraphSummaryThresholds["MaxVertices"] ||
    edgeCount > $HypergraphSummaryThresholds["MaxEdges"] ||
    (vertexCount + edgeCount) > $HypergraphSummaryThresholds["MaxTotalElements"]
]

HypergraphLargeQ[___] := False

(* Function to configure summary thresholds *)
SetHypergraphSummaryThresholds[rules___Rule] := (
    AssociateTo[$HypergraphSummaryThresholds, rules]
)



(* Hypergraph *)

Hypergraph /: MakeBoxes[hg_Hypergraph /; HypergraphQ[Unevaluated[hg]], form : StandardForm] := 
    If[
        HypergraphLargeQ[hg],
        
        (* Large hypergraph: show summary box *)
  
        BoxForm`ArrangeSummaryBox[
            "Hypergraph",
            hg,
            $HypergraphIcon,
            {
                {BoxForm`SummaryItem[{"Vertices: ", VertexCount[hg]}]},
                {BoxForm`SummaryItem[{"Edges: ", EdgeCount[hg]}]}
            },
            {
                {BoxForm`SummaryItem[{"Max arity: ", Max[hg["Arity"]]}]},
                {BoxForm`SummaryItem[{"Edge symmetries: ", CountDistinct[hg["EdgeSymmetry"]]}]}
            },
            form,
            "Interpretable" -> Automatic
        ],
        
        (* Small hypergraph: show graphical plot *)
        With[{
            boxId = SymbolName[Unique["Hypergraph"]]
        },
            {
            boxes = Block[{BoxForm`$UseTextFormattingWhenConvertingInput = False},
                ToBoxes[
                    Show[
                        SimpleHypergraphPlot[#, BaseStyle -> {
                                GraphicsHighlightColor -> Red,
                                ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[
                                        MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], boxId], ToBoxes[HypergraphDraw[hg]]]]
                                    ], MenuEvaluator -> Automatic]}
                                }
                            }
                        ],
                        BoxID -> boxId
                    ],
                    form
                ]
            ]
        },
            hypergraphBox[boxes, #]
        ] & @ hg
    ]

Hypergraph /: MakeBoxes[hg_Hypergraph /; HypergraphQ[Unevaluated[hg]], form : TraditionalForm] := With[{
	edgeBoxes = ToBoxes[#["Edges"], form],
    tooltip = ToBoxes[Options[#, "EdgeSymmetry"]]
},
	InterpretationBox[edgeBoxes, #, Tooltip -> tooltip]
] & @ hg


(* Hyperedge *)

Hyperedge /: MakeBoxes[he : Hyperedge[edgeTag_, sym_ : "Unordered"], form_ ] := With[{
    edge = Replace[edgeTag, (edge_ -> _) :> edge],
    tag = Replace[edgeTag, {(_ -> tag_) :> tag, _ -> None}]
}, {
    boxes = TooltipBox[RowBox[Join[{#1}, Riffle[ToBoxes[#, form] & /@ edge, ","], {#2}] & @@
        Replace[sym, {"Ordered" | {} -> {"{", "}"}, "Cyclic" -> {"[", "]"}, _ -> {"(", ")"}}]], ToBoxes[If[tag === None, sym, sym -> tag], form]]
},
    InterpretationBox[boxes, he]
]


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
    ImagePadding -> 3
}

HypergraphRule /: MakeBoxes[hr_HypergraphRule /; HypergraphRuleQ[Unevaluated[hr]], form : StandardForm] := With[{boxId = SymbolName[Unique["HypergraphRule"]]}, With[{
    boxes = Block[{BoxForm`$UseTextFormattingWhenConvertingInput = False}, ToBoxes[
        Show[GraphicsRow[{
            SimpleHypergraphPlot[#["Input"], $HypergraphRulePlotOptions],
            Graphics[{GrayLevel[0.65], $HypergraphRuleArrow}, ImageSize -> 24],
            SimpleHypergraphPlot[#["Output"], $HypergraphRulePlotOptions]
        },
            PlotRangePadding -> 1,
            BaseStyle -> {
                GraphicsHighlightColor -> Blue,
                GraphicsBoxOptions -> {ImageSize -> Medium},
                ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[
                    MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], boxId], ToBoxes[HypergraphRuleDraw[hr]]]]],
                    MenuEvaluator -> Automatic]}}
            }
        ], BoxID -> boxId],
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

