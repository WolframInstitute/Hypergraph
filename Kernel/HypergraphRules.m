Package["WolframInstitute`Hypergraph`"]


PackageExport["HypergraphRuleQ"]
PackageExport["HypergraphRule"]
PackageExport["ToPatternEdges"]
PackageExport["ToPatternRules"]
PackageExport["PatternRuleToMultiReplaceRule"]
PackageExport["HighlightRule"]



ToPatternEdges[edges : {___List}] := Block[{vs = Union @@ edges, varSymbols},
    varSymbols = Association @ MapIndexed[#1 -> Symbol["\[FormalX]" <> ToString[#2[[1]]]] &, vs];
    Replace[edges, Pattern[#, _] & /@ varSymbols, {2}]
]

ToPatternEdges[hg_ ? HypergraphQ] := ToPatternEdges[EdgeList[hg]]


ToPatternRules[lhs : {___List}, rhs : {___List}] := Block[{vs = Union @@ lhs, ws = Union @@ rhs, varSymbols, newVars},
    varSymbols = Association @ MapIndexed[#1 -> Symbol["\[FormalX]" <> ToString[#2[[1]]]] &, Union[vs, ws]];
    newVars = Replace[Complement[ws, vs], varSymbols, {2}];
    Replace[lhs, Pattern[#, _] & /@ varSymbols, {2}] :> Module[##] & [Replace[newVars, varSymbols, {1}], Replace[rhs, varSymbols, {2}]]
]

ToPatternRules[HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ]] := ToPatternRules[EdgeList[input], EdgeList[output]]

ToPatternRules[rules : {___HypergraphRule}] := ToPatternRules /@ rules


PatternRuleToMultiReplaceRule[rule : _[Verbatim[Condition][lhs_, _] | lhs_, _Module]] := With[{
    nothings = Unevaluated @@ Array[Nothing &, Length[lhs] - 1, Automatic, Hold @* List]
},
	ResourceFunction["SpliceAt"][
		MapAt[List, {2}] @ MapAt[Splice, {2, 2}] @ rule,
		nothings,
		{2, 2}
	]
]

PatternRuleToMultiReplaceRule[rule : _[lhs_List | Verbatim[HoldPattern][lhs_List], Verbatim[Module][vars_, rhs_List]]] := Block[{
	l = Length[Unevaluated[lhs]], r = Length[Unevaluated[rhs]],
	newRule
},
	newRule = If[r > 1, MapAt[Splice, rule, {2, 2}], Delete[rule, {2, 2, 0}]];
	If[r < l, newRule = Nest[Insert[Nothing, {2, 2}], MapAt[List, newRule, {2}], l - r]];
	ReplaceAt[newRule /. Verbatim[Module][{}, body_] :> body, Splice[body_] :> body, {2}]
]


(rule : HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ])[hg_ ? HypergraphQ] := Block[{
    vertices = VertexList[hg], edges= EdgeList[hg],
    inputVertices = VertexList[input], ouputVertices = VertexList[output],
    inputEdges = EdgeList[input], outputEdges = EdgeList[output],
    matches,
    lhsVertices, inputFreeVertices, newVertices, deleteVertices, newVertexMap
},
    matches = First /@ Keys @ ResourceFunction["MultiReplace"][
        edges,
        ToPatternEdges @ input,
        {1},
        "Mode" -> "OrderlessSubsets"
    ];
    lhsVertices = Union @@ inputEdges;
    inputFreeVertices = Complement[inputVertices, lhsVertices];
    newVertices = Complement[ouputVertices, inputVertices];
    deleteVertices = Complement[inputVertices, ouputVertices];
    newVertexMap = Block[{$ModuleNumber = 1}, # -> Unique["\[FormalX]"] & /@ newVertices];
    Catenate @ Map[pos |-> Block[{matchEdges = Extract[edges, pos], matchVertices, matchVertexMap},
        matchVertices = Union @@ matchEdges;
        matchVertexMap = DeleteDuplicatesBy[Thread[Catenate @ inputEdges -> Catenate @ matchEdges], First];
        Block[{
            origVertexMap = Join[
                matchVertexMap,
                Thread[inputFreeVertices -> #],
                newVertexMap
            ],
            deleteOrigVertices, newEdges
        },
            deleteOrigVertices = Replace[deleteVertices, origVertexMap, {1}];
            newEdges = Replace[outputEdges, origVertexMap, {2}];
            <| "Hypergraph" -> Hypergraph[
                    Union[DeleteElements[vertices, deleteOrigVertices], Replace[ouputVertices, origVertexMap, {1}]],
                    Insert[
                        Replace[Delete[edges, pos], Alternatives @@ deleteOrigVertices -> Nothing, {2}],
                        Splice @ newEdges,
                        Min[pos]
                    ]
                ],
                "MatchVertices" -> Join[#, matchVertices],
                "MatchEdges" -> matchEdges,
                "NewVertices" -> Values[newVertexMap],
                "NewEdges" -> newEdges
            |>
        ] & /@ Catenate[Permutations /@ Subsets[Complement[vertices, matchVertices], {Length[inputFreeVertices]}]]
    ],
        matches
    ]
]


$HypergraphRulePlotOptions = {
    VertexLabels -> Automatic,
    Frame -> True,
    FrameTicks -> None,
    PlotRangePadding -> .5,
    ImagePadding -> 3,
    AspectRatio -> 1,
    ImageSize -> Tiny
};

Options[HighlightRule] := Options[SimpleHypergraphPlot]

HighlightRule[rule_HypergraphRule, hg_ ? HypergraphQ, opts : OptionsPattern[]] := Block[{
    vs = VertexList[hg], edges = EdgeList[hg],
    matches, embedding
},
    matches = rule[hg];

    edgeStyles = Thread[edges ->
        Replace[
            edges,
            Join[
                Replace[Flatten[{OptionValue[SimpleHypergraphPlot, hg["Options"], EdgeStyle]}], {Automatic -> Nothing, s : Except[_Rule] :> _ -> s}, {1}],
                MapIndexed[#1 -> Directive[OptionValue[SimpleHypergraphPlot, hg["Options"], ColorFunction][#2[[1]]], EdgeForm[Transparent]] &, edges]
            ],
            {1}
        ]
    ];
    embedding = Thread[vs -> HypergraphEmbedding[hg]];
    Map[
        GraphicsRow[{
            SimpleHypergraphPlot[
                hg,
                opts,
                EdgeStyle -> Map[
                    # -> Directive[Thick, Dashed, Red, EdgeForm[Directive[Dashed, Red, Thick]]] &,
                    #MatchEdges
                ],
                VertexStyle -> Map[# -> Directive[PointSize[0.02], Red] &, #MatchVertices],
                ImageSize -> Medium,
                $HypergraphRulePlotOptions
            ],
            Graphics[{Arrowheads[0.3], Arrow[{{0, 0}, {.25, 0}}]}, ImageSize -> Medium],
            SimpleHypergraphPlot[
                #Hypergraph,
                opts,
                EdgeStyle -> Join[
                    Map[
                        # -> Directive[Thick, Dashed, Red, EdgeForm[Directive[Dashed, Red, Thick]]] &,
                        (* output edges always getting spliced at the first position *)
                        #NewEdges
                    ],
                    edgeStyles
                ],
                VertexStyle -> Map[# -> Directive[PointSize[0.02], Red] &, #NewVertices],
                VertexCoordinates -> embedding,
                ImageSize -> Medium,
                hg["Options"],
                $HypergraphRulePlotOptions
            ]
        }, PlotRangePadding -> 1] &,
        matches
    ]
]



HypergraphRuleQ[hr_HypergraphRule] := System`Private`HoldValidQ[hr]

HypergraphRuleQ[___] := $Failed

Options[HypergraphRule] := Options[Hypergraph]

HypergraphRule[input_, _]["Input"] := input

HypergraphRule[_, output_]["Output"] := output

(hr : HypergraphRule[input_, output_, opts : OptionsPattern[]]) /; ! HypergraphRuleQ[Unevaluated[hr]] :=
    Enclose[
        System`Private`HoldSetValid @ HypergraphRule[##] & [
            ConfirmBy[Hypergraph[input, opts], HypergraphQ],
            ConfirmBy[Hypergraph[output, opts], HypergraphQ]
        ]
    ]

HypergraphRule /: MakeBoxes[hr : HypergraphRule[input_, output_] ? HypergraphRuleQ, form_] := With[{
    boxes = ToBoxes[
        GraphicsRow[{
            SimpleHypergraphPlot[input, $HypergraphRulePlotOptions],
            Graphics[{Arrowheads[.5], Arrow[{{0, 0}, {.5, 0}}]}, ImageSize -> Tiny],
            SimpleHypergraphPlot[output, $HypergraphRulePlotOptions]
        },
            PlotRangePadding -> 1
        ],
        form
    ]
},
    InterpretationBox[boxes, hr]
]

