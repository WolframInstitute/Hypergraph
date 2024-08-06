Package["WolframInstitute`Hypergraph`"]


PackageExport["HypergraphRuleQ"]
PackageExport["HypergraphRule"]
PackageExport["ToLabeledEdges"]
PackageExport["ToLabeledPatternEdges"]
PackageExport["ToPatternRules"]
PackageExport["HighlightRule"]
PackageScope["PatternRuleToMultiReplaceRule"]



makeVertexLabelPattern[vertex_, label_, makePattern_ : False] := Replace[label, {
    None :> If[TrueQ[makePattern], _, vertex],
    Automatic | Placed[Automatic, _] :> If[TrueQ[makePattern], Pattern[#, _] & @ Symbol["\[FormalL]" <> StringDelete[ToString[vertex, InputForm], Except[WordCharacter]]], vertex],
    "Name" | Placed["Name", _] :> vertex,
    Placed[placedLabel_, _] :> placedLabel,
    _ :> label
}]

ToLabeledEdges[vertexLabels_Association, edges : {___List}, makePattern_ : False, vertexCondition_ : True] := Block[{labels, patterns, varSymbols, labeledEdges},
    {patterns, labels} = Reap[
        varSymbols = Association @ KeyValueMap[#1 ->
            If[ TrueQ[makePattern],
                Labeled[
                    Sow[Pattern[#, _] & @ Symbol["\[FormalV]" <> StringDelete[ToString[#1], Except[WordCharacter]]], "VertexPattern"],
                    Sow[Replace[#2, s_Symbol :> Pattern @@ {s, _}], "LabelPattern"]
                ],
                Labeled[#1, #2]
            ] &, vertexLabels],
        {"VertexPattern", "LabelPattern"}
    ][[2]];
    Scan[Sow[#, "LabelPattern"] &, First[labels, {}]];
    Scan[Sow[#, "VertexPattern"] &, First[patterns, {}]];
    labeledEdges = Replace[edges, varSymbols, {2}];
    If[ TrueQ[makePattern],
        If[ TrueQ[vertexCondition],
            Function[Null, Condition[labeledEdges, UnsameQ[##]], HoldAll] @@
                DeleteDuplicates[Hold @@ First[labels, {}] /. Verbatim[Pattern][label_, _] :> label],
            Condition[labeledEdges, True]
        ],
        labeledEdges
    ]
]

ToLabeledEdges[hg_ ? HypergraphQ, makePattern_ : False, vertexCondition_ : True, edgeCondition_ : False] := Block[{
    vs = VertexList[hg],
    edges = EdgeList[hg],
    taggedEdges = EdgeListTagged[hg],
    opts = AbsoluteOptions[hg],
    vertexLabels,
    edgeSymmetry = EdgeSymmetry[hg],
    edgeTags = EdgeTags[hg],
    edgeLabels
},
    vertexLabels = Association @ Map[Apply[#1 -> makeVertexLabelPattern[#1, #2, makePattern] &], Lookup[opts, VertexLabels]];
    edgeLabels = Values[Lookup[opts, EdgeLabels]];
    labeledEdges = MapAt[
        MapIndexed[
            With[{edge = #1, symm = edgeSymmetry[[#2[[1]]]], tag = edgeTags[[#2[[1]]]],
                label = Replace[edgeLabels[[#2[[1]]]], Placed[l_, _] :> l]
            },
                Labeled[edge,
                    {
                        symm,
                        ReplaceAll[
                            If[makePattern, Replace[label, {None -> _, s_Symbol :> Pattern @@ {s, _}}], label],
                            {"EdgeTag" -> tag, "EdgeSymmetry" -> symm, Automatic | "Name" -> edge}
                        ]
                    }
                ]
            ] &
        ],
        ToLabeledEdges[vertexLabels, edges, makePattern, vertexCondition],
        If[makePattern, {1}, {{}}]
    ];
    If[ TrueQ[makePattern] && TrueQ[edgeCondition],
        Function[Null, ReplaceAt[labeledEdges, x_ :> x && UnsameQ[##], {2}], HoldAll] @@
            DeleteDuplicates[Hold @@ labeledEdges[[1, All, 2, 2]] /. Verbatim[Pattern][label_, _] :> label],
        labeledEdges
    ]
]

ToLabeledPatternEdges[hg_ ? HypergraphQ, vertexCondition_ : True, edgeCondition_ : False] := Block[{
    edges = EdgeList[hg],
    simpleEdgeSymmetry
},
    simpleEdgeSymmetry = Replace[edges, Replace[Flatten[{EdgeSymmetry[hg]}], {Automatic -> _ -> "Unordered", s : Except[_Rule] :> _ -> s}, {1}], {1}];
    Which[
        AllTrue[simpleEdgeSymmetry, MatchQ["Unordered" | "Undirected"]],
        MapAt[{OrderlessPatternSequence @@ #} &, #, {1, All, 1}],
        AllTrue[simpleEdgeSymmetry, MatchQ["Ordered" | "Directed"]],
        #,
        True,
        With[{edgeSymmetry = EdgeSymmetry[hg]},
            MapAt[
                SubsetMap[
                    MapIndexed[With[{edge = #1, symm = Flatten[edgeSymmetry[[#2]]]}, Replace[
                        Alternatives @@ (Permute[edge, #] & /@ GroupElements[PermutationGroup[symm]]),
                        Verbatim[Alternatives][x_] :> x
                    ]] &],
                    {All, 1}
                ],
                #,
                {1}
            ]
        ]
    ] & @ ToLabeledEdges[hg, True, vertexCondition, edgeCondition]
]


ToPatternRules[lhs : {___List}, rhs : {___List}] := Block[{vs = Union @@ lhs, ws = Union @@ rhs, varSymbols, newVars},
    varSymbols = Association @ MapIndexed[#1 -> Symbol["\[FormalV]" <> ToString[#2[[1]]]] &, Union[vs, ws]];
    newVars = Replace[Complement[ws, vs], varSymbols, {2}];
    Replace[lhs, Pattern[#, _] & /@ varSymbols, {2}] :> Module[##] & [Replace[newVars, varSymbols, {1}], Replace[rhs, varSymbols, {2}]]
]

ToPatternRules[HoldPattern[HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ]]] := ToPatternRules[EdgeList[input], EdgeList[output]]

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


Options[HypergraphRuleApply] = Join[
    {
        "BindingsMethod" -> Automatic, "SymmetryMethod" -> Automatic, "CanonicalizeMethod" -> Automatic, "MatchesMethod" -> Automatic,
        "DistinctVertexLabels" -> True, "DistinctEdgeLabels" -> True
    },
    Options[ResourceFunction["MultiReplace"]]
]

HypergraphRuleApply[input_, output_, hg_, opts : OptionsPattern[]] := Block[{
    vertices = VertexList[hg], edges = EdgeListTagged[hg],
    inputVertices = VertexList[input], outputVertices = VertexList[output],
    inputEdges = EdgeList[input], outputEdges = EdgeListTagged[output],
    bindingsMethod = Replace[OptionValue["BindingsMethod"], {First -> (Take[#, UpTo[1]] &), _ -> Identity}],
    symmetryMethod = Replace[OptionValue["SymmetryMethod"], {Automatic -> (_ &), _ -> Identity}],
    matchesMethod = Replace[OptionValue["MatchesMethod"], {Automatic -> Identity}],
    canonicalizeMethod = Replace[OptionValue["CanonicalizeMethod"], {
        Automatic -> Identity,
        Full -> DeleteDuplicatesBy[Sort /@ #[[{"MatchVertices", "MatchEdgePositions", "NewVertices", "NewEdges", "DeletedVertices"}]] &]
    }],
    vertexConditionQ = TrueQ[OptionValue["DistinctVertexLabels"]],
    edgeConditionQ = TrueQ[OptionValue["DistinctEdgeLabels"]],
    patterns, labelPatterns,
    annotationRules, outputAnnotationRules,
    vertexAnnotations, outputVertexAnnotations,
    edgeAnnotations, outputEdgeAnnotations,
    outputSymmetry,
    embedding,
    matches,
    lhsVertices, inputFreeVertices, newVertices, deleteVertices, newVertexMap
},
    outputSymmetry = Thread[outputEdges -> EdgeSymmetry[output]];
    {patterns, labelPatterns} = First[#, {}] & /@ Reap[
        matches = Thread @ Keys @ matchesMethod @ ResourceFunction["MultiReplace"][
            ToLabeledEdges[hg],
            MapAt[symmetryMethod, ToLabeledPatternEdges[input, vertexConditionQ, edgeConditionQ], {1, All, 2, 1}],
            {1},
            FilterRules[{opts}, Options[ResourceFunction["MultiReplace"]]],
            "PatternSubstitutions" -> True,
            "Mode" -> "OrderlessSubsets"
        ],
        {"VertexPattern", "LabelPattern"}
    ][[2]];
    lhsVertices = Union @@ inputEdges;
    inputFreeVertices = Complement[inputVertices, lhsVertices];
    labelPatterns = Extract[labelPatterns, Lookup[PositionIndex[inputVertices], inputFreeVertices]];
    newVertices = Complement[outputVertices, inputVertices];
    deleteVertices = Complement[inputVertices, outputVertices];
    newVertexMap = Block[{$ModuleNumber = 1}, Verbatim[#] -> Unique["\[FormalV]"] & /@ newVertices];

    annotationRules = AbsoluteOptions[hg];
    outputAnnotationRules = AbsoluteOptions[output];

    {vertexAnnotations, outputVertexAnnotations} = Map[
        rules |-> KeyTake[rules, {VertexStyle, VertexLabelStyle, VertexLabels}],
        {annotationRules, outputAnnotationRules}
    ];
    {edgeAnnotations, outputEdgeAnnotations} = Map[
        rules |-> KeyTake[rules, {EdgeStyle, EdgeLabelStyle, "EdgeSymmetry", EdgeLabels}],
        {annotationRules, outputAnnotationRules}
    ];

    embedding = Thread[vertices -> HypergraphEmbedding[hg]];

    canonicalizeMethod @ Catenate @ MapThread[{pos, bindings} |-> Block[{
        matchEdges = Replace[Extract[edges, pos], (edge_ -> _) :> edge, {1}],
        matchVertices, matchVertexMap
    },
        matchVertices = Union @@ matchEdges;
        Catenate @ Map[initBinding |-> (
            Map[matchFreeVertices |-> Block[{
                binding = Association[
                    initBinding /. Labeled[expr_, _] :> expr,
                    Thread[Pattern[Evaluate @ Symbol["\[FormalV]" <> ToString[#]], _] & /@ (Length[initBinding] + Range[Length[inputFreeVertices]]) -> matchFreeVertices]
                ],
                origVertexMap,
                deleteOrigVertices, newEdges,
                bindingRules
            },
                matchVertexMap = Thread[inputVertices -> (patterns /. binding)];
                origVertexMap = Join[
                    matchVertexMap,
                    newVertexMap
                ];
                binding = Enclose @ Association[
                    binding,
                    KeyValueMap[{patt, label} |->
                        With[{varPatts = Union[Cases[patt, Verbatim[Pattern][sym_, _], All]]},
                            AssociationThread[
                                varPatts,
                                Confirm @ With[{vars = Extract[varPatts, {All, 1}, Hold]},
                                    Replace[label, Function[Null, {patt :> #, _ -> $Failed}, HoldAll] @@ vars]
                                ]
                            ]
                        ],
                        AssociationThread[
                            labelPatterns,
                            With[{labelRules = Lookup[vertexAnnotations, VertexLabels, {}]},
                                Replace[Replace[#, labelRules], Automatic | "Name" -> #] & /@ Replace[inputFreeVertices, origVertexMap, {1}]
                            ]
                        ]
                    ]
                ];
                If[FailureQ[binding], Return[Nothing, Block]];
                bindingRules = Normal @ KeyMap[Replace[Verbatim[Pattern][sym_Symbol, _] :> HoldPattern[sym]]] @ binding;
                deleteOrigVertices = Replace[deleteVertices, origVertexMap, {1}];
                newEdges = Replace[outputEdges, {
                    e : (edge_ -> tag_) :> CanonicalEdge[Replace[edge, origVertexMap, {1}], Lookup[outputSymmetry, Key[e], {}]] -> (tag /. bindingRules),
                    edge_ :> CanonicalEdge[Replace[edge, origVertexMap, {1}], Lookup[outputSymmetry, Key[edge], {}]]
                },
                    {1}
                ];
                <| "Hypergraph" -> Hypergraph[
                        Union[DeleteElements[vertices, deleteOrigVertices], Replace[outputVertices, origVertexMap, {1}]],
                        Insert[
                            Replace[Delete[edges, pos], {(edge_ -> tag_) :> DeleteElements[edge, deleteOrigVertices] -> tag, edge_ :> DeleteElements[edge, deleteOrigVertices]}, {1}],
                            Splice @ newEdges,
                            Min[pos, Length[edges] + 1]
                        ],
                        FilterRules[hg["Options"],
                            Except[VertexStyle | VertexLabels | VertexLabelStyle | VertexCoordinates | EdgeStyle | EdgeLabels | EdgeLabelStyle | "EdgeSymmetry"]
                        ],
                        Normal @ Map[DeleteDuplicates] @ MapAt[Function[Null, Unevaluated[#] /. bindingRules, HoldAll], Key[VertexLabels]] @ Merge[{vertexAnnotations, outputVertexAnnotations},
                           Apply[Join[
                                Replace[#2, (h : (Rule | RuleDelayed))[vertex_, annotation_] :> h[Replace[vertex, origVertexMap], annotation], {1}],
                                DeleteCases[#1, (Rule | RuleDelayed)[Alternatives @@ deleteOrigVertices, _]]
                            ] &]
                        ],
                        Normal @ Map[DeleteDuplicates] @ MapAt[Function[Null, Unevaluated[#] /. bindingRules, HoldAll], Key[EdgeLabels]] @ Merge[{edgeAnnotations, outputEdgeAnnotations},
                           Apply[Join[
                                Replace[#2, {
                                    (h : (Rule | RuleDelayed))[e : (edge_List -> tag_), annotation_] :>
                                        h[CanonicalEdge[Replace[edge, origVertexMap, {1}], Lookup[outputSymmetry, Key[e], {}]] -> (tag /. bindingRules), annotation],
                                    (h : (Rule | RuleDelayed))[edge_List, annotation_] :>
                                        h[CanonicalEdge[Replace[edge, origVertexMap, {1}], Lookup[outputSymmetry, Key[edge], {}]], annotation]
                                },
                                {1}
                                ],
                                DeleteCases[#1, Alternatives @@ matcheEdges | (Alternatives @@ matcheEdges -> _) -> _]
                            ] &]
                        ],
                        VertexCoordinates -> embedding,
                        PlotLabel -> (Lookup[output["Options"], PlotLabel, None] /. bindingRules)
                    ],
                    "MatchVertices" -> Join[matchFreeVertices, matchVertices],
                    "MatchEdges" -> matchEdges,
                    "MatchEdgePositions" -> pos,
                    "NewVertices" -> Values[newVertexMap],
                    "NewEdges" -> newEdges,
                    "DeletedVertices" -> deleteOrigVertices,
                    "RuleVertexMap" -> origVertexMap,
                    "Bindings" -> binding
                |>
            ],
                Catenate[Permutations /@ Subsets[Complement[vertices, matchVertices], {Length[inputFreeVertices]}]]
            ]
        ),
            bindingsMethod @ bindings
        ]
    ],
        matches
    ]
]

(rule : HoldPattern[HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ]])[
    hg_ ? HypergraphQ, opts : OptionsPattern[HypergraphRuleApply]
] /; HypergraphRuleQ[rule] = HypergraphRuleApply[input, output, hg, opts]



Options[HighlightRule] := Join[{
    "HighlightLeftVertexStyle" -> Directive[PointSize[0.02], Red],
    "HighlightRightVertexStyle" -> Directive[PointSize[0.02], Red],
    "HighlightLeftEdgeStyle" -> Directive[Thick, Red, EdgeForm[Directive[Red, Thick]]],
    "HighlightRightEdgeStyle" -> Directive[Thick, Red, EdgeForm[Directive[Red, Thick]]]
}, Options[HypergraphRuleApply], Options[SimpleHypergraphPlot]]

HighlightRule[rule_ ? HypergraphRuleQ, hg_ ? HypergraphQ, opts : OptionsPattern[]] :=
    HighlightRule[rule[hg, FilterRules[{opts}, Options[HypergraphRuleApply]]], hg, FilterRules[{opts}, Except[Options[HypergraphRuleApply], Options[HighlightRule]]]]

HighlightRule[matches : {___Association}, hg_ ? HypergraphQ, opts : OptionsPattern[]] := Block[{
    edges = EdgeListTagged[hg], plotOpts = FilterRules[{opts}, Options[SimpleHypergraphPlot]], hgOpts = AbsoluteOptions[hg]
},
    Map[
        GraphicsRow[{
            SimpleHypergraphPlot[
                hg,
                EdgeStyle -> MapAt[#[[1]] -> OptionValue["HighlightLeftEdgeStyle"] &, Lookup[hgOpts, EdgeStyle], #MatchEdgePositions],
                VertexStyle -> Map[# -> OptionValue["HighlightLeftVertexStyle"] &, #MatchVertices],
                plotOpts,
                PlotRange -> Automatic,
                $HypergraphRulePlotOptions
            ],
            Graphics[{GrayLevel[0.65], $HypergraphRuleArrow}, ImageSize -> 24],
            SimpleHypergraphPlot[
                #Hypergraph,
                EdgeStyle -> Map[
                    # -> OptionValue["HighlightRightEdgeStyle"] &,
                    (* output edges always getting spliced at the first position *)
                    #NewEdges
                ],
                VertexStyle -> Map[# -> OptionValue["HighlightRightVertexStyle"] &, #NewVertices],
                VertexCoordinates -> DeleteCases[
                    Thread[VertexList[hg] -> HypergraphEmbedding[hg]],
                    HoldPattern[Evaluate[Alternatives @@ #NewVertices -> _]]
                ],
                plotOpts,
                PlotRange -> Automatic,
                $HypergraphRulePlotOptions
            ]
        }, PlotRangePadding -> 1] &,
        matches
    ]
]



HypergraphRuleQ[hr_HypergraphRule] := System`Private`HoldValidQ[hr] ||
    MatchQ[Unevaluated[hr], HoldPattern[HypergraphRule[input_, output_]] /; HypergraphQ[Unevaluated[input]] && HypergraphQ[Unevaluated[output]]]

HypergraphRuleQ[___] := False

Options[HypergraphRule] := Options[Hypergraph]

HoldPattern[HypergraphRule[input_, _] ? HypergraphRuleQ]["Input"] := input

HoldPattern[HypergraphRule[_, output_] ? HypergraphRuleQ]["Output"] := output

(hr : HoldPattern[HypergraphRule[input_, output_, opts : OptionsPattern[]]]) /; ! HypergraphRuleQ[Unevaluated[hr]] :=
    Enclose[
        System`Private`HoldSetValid @ HypergraphRule[##] & [
            ConfirmBy[Hypergraph[input, opts], HypergraphQ],
            ConfirmBy[Hypergraph[output, opts], HypergraphQ]
        ]
    ]

