Package["WolframInstitute`Hypergraph`"]


PackageExport["HypergraphRuleQ"]
PackageExport["HypergraphRule"]
PackageExport["ToLabeledEdges"]
PackageExport["ToLabeledPatternEdges"]
PackageExport["ToPatternRules"]
PackageExport["HighlightRule"]
PackageScope["PatternRuleToMultiReplaceRule"]



makeVertexLabelPattern[vertex_, label_, makePattern_ : False] := Replace[label, {
    None :> If[TrueQ[makePattern], _, None],
    Automatic | Placed[Automatic, _] :> If[TrueQ[makePattern], Pattern[#, _] & @ Symbol["\[FormalL]" <> StringDelete[ToString[vertex, InputForm], Except[WordCharacter]]], vertex],
    "Name" | Placed["Name", _] :> vertex,
    Placed[placedLabel_, _] :> placedLabel,
    _ :> label
}]

ToLabeledEdges[vertexLabels_Association, edges : {___List}, makePattern_ : False, vertexCondition_ : True] := Block[{labels, patterns, varSymbols, labeledEdges},
    {patterns, labels} = Reap[
        varSymbols = KeyValueMap[If[FreeQ[#1, _Pattern], #1, Verbatim[#1]] ->
            If[ TrueQ[makePattern],
                If[ MatchQ[#2, _BlankSequence | _BlankNullSequence | Verbatim[Pattern][_, _BlankSequence | _BlankNullSequence]],
                    Sow[#2, "LabelPattern"]; Sow[Pattern[Evaluate[Symbol["\[FormalV]" <> ToString[Hash[#1]]]], #2], "VertexPattern"],
                    Labeled[
                        Sow[If[MatchQ[#1, _Pattern], #, Pattern[#, _] & @ Symbol["\[FormalV]" <> ToString[Hash[#1]]]], "VertexPattern"],
                        Sow[Replace[#2, s_Symbol :> Pattern @@ {s, _}], "LabelPattern"]
                    ]
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
    vertexLabels = Association @ MapThread[#1 -> makeVertexLabelPattern[#1, #2, makePattern] &, {vs, Lookup[opts, VertexLabels][[All, 2]]}];
    edgeLabels = Values[Lookup[opts, EdgeLabels]];
    labeledEdges = MapAt[
        MapIndexed[
            With[{
                edge = #1, symm = edgeSymmetry[[#2[[1]]]], tag = edgeTags[[#2[[1]]]],
                label = Replace[edgeLabels[[#2[[1]]]], Placed[(l_ -> tags_) | l_, _] | (l_ -> tags_) :> {l, tags}]
            },
                Labeled[Thread[Labeled[edge, PadRight[Last[label, {}], Length[edge], If[makePattern, _, Automatic]]]],
                    {
                        symm,
                        ReplaceAll[
                            If[makePattern, Replace[label[[1]], {None -> _, s_Symbol :> Pattern @@ {s, _}}], label[[1]]],
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
                        Alternatives @@ (Permute[edge, #] & /@ symm),
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

ToPatternRules[HoldPattern[HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ, ___]]] := ToPatternRules[EdgeList[input], EdgeList[output]]

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
    inputVertices = VertexList[input], outputVertices = VertexList[output], newOutputVertices,
    inputEdges = EdgeList[input], outputEdges = EdgeListTagged[output], newOutputEdges,
    bindingsMethod = Replace[OptionValue["BindingsMethod"], {First -> (Take[#, UpTo[1]] &), _ -> Identity}],
    symmetryMethod = Replace[OptionValue["SymmetryMethod"], {Automatic -> Identity, _ -> (_ &)}],
    matchesMethod = Replace[OptionValue["MatchesMethod"], {Automatic -> Identity}],
    canonicalizeMethod = Replace[OptionValue["CanonicalizeMethod"], {
        Automatic -> Identity,
        Full -> DeleteDuplicatesBy[Sort /@ #[[{"MatchVertices", "MatchEdgePositions", "NewVertices", "NewEdges", "DeletedVertices"}]] &]
    }],
    vertexConditionQ = TrueQ[OptionValue["DistinctVertexLabels"]],
    edgeConditionQ = TrueQ[OptionValue["DistinctEdgeLabels"]],
    patterns, labelPatterns, freeVertexPositions,
    annotationRules, outputAnnotationRules, labelVertexRules,
    vertexAnnotations, outputVertexAnnotations,
    edgeAnnotations, outputEdgeAnnotations,
    outputSymmetry,
    embedding,
    matches,
    inputFreeVertices, newVertices, deleteVertices, newVertexMap, holdOutputVertices
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
    inputFreeVertices = Complement[inputVertices, Catenate[inputEdges]];
    freeVertexPositions = Lookup[PositionIndex[inputVertices], inputFreeVertices];

    annotationRules = AbsoluteOptions[hg];
    outputAnnotationRules = AbsoluteOptions[output];

    labelVertexRules = DeleteCases[Verbatim[_] -> _] @ Thread[(labelPatterns /. Verbatim[Pattern][sym_Symbol, _] :> HoldPattern[sym]) -> inputVertices];
    newOutputVertices = MapThread[
        Replace[#2, Append[labelVertexRules, _ -> #1]] &,
        {
            outputVertices,
            Lookup[outputAnnotationRules, VertexLabels][[All, 2]]
        }
    ];
    newOutputEdges = With[{rules = DeleteCases[Thread[outputVertices -> newOutputVertices], x_ -> x_]},
        Replace[outputEdges, {(edge_List -> tag_) :> (Replace[edge, rules, 1] -> tag), edge_List :> Replace[edge, rules, 1]}, 1]
    ];

    {vertexAnnotations, outputVertexAnnotations} = Map[
        Apply[{vs, rules} |-> Map[Thread[vs -> #[[All, 2]]] &, KeyTake[rules, Join[$VertexAnnotations, Lookup[annotationRules, "VertexAnnotations", {}]]]]],
        {{vertices, annotationRules}, {newOutputVertices, outputAnnotationRules}}
    ];
    {edgeAnnotations, outputEdgeAnnotations} = Map[
        Apply[{edges, rules} |-> Map[Thread[edges -> #[[All, 2]]] &, KeyTake[rules, Join[$EdgeAnnotations, Lookup[annotationRules, "EdgeAnnotations", {}]]]]],
        {{edges, annotationRules}, {newOutputEdges, outputAnnotationRules}}
    ];
    
    newVertices = Complement[newOutputVertices, inputVertices];
    deleteVertices = Complement[inputVertices, newOutputVertices];
    newVertexMap = Block[{$ModuleNumber = 1}, # -> Unique["\[FormalV]"] & /@ newVertices];

    embedding = Thread[vertices -> HypergraphEmbedding[hg]];

    canonicalizeMethod @ Catenate @ MapThread[{pos, bindings} |-> Block[{
        matchEdges = Replace[Extract[edges, pos], (edge_ -> _) :> edge, {1}],
        matchVertices, matchVertexMap
    },
        matchVertices = Union @@ matchEdges;
        Catenate @ Map[initBinding |-> (
            Map[matchFreeVertices |-> Block[{
                binding = Association[
                    Replace[initBinding, {Labeled[expr_, _] :> expr, seq_Sequence :> RuleCondition[Sequence @@ Replace[{seq}, Labeled[expr_, _] :> expr, 1]]}, 1],
                    Thread[Extract[patterns, freeVertexPositions] -> matchFreeVertices]
                ],
                origVertexMap,
                deleteOrigVertices, newEdges,
                bindingRules
            },
                matchVertexMap = Association @ MapThread[#1 :> Evaluate[#2 /. binding] &, {inputVertices, patterns}];
                
                binding = Enclose @ Association[
                    KeyDrop[binding, patterns],
                    KeyValueMap[{patt, label} |->
                        With[{varPatts = Union[Cases[patt, Verbatim[Pattern][_Symbol, _], All]]},
                            AssociationThread[
                                varPatts,
                                Confirm @ With[{vars = Extract[varPatts, {All, 1}, Hold]},
                                    Replace[label, Function[Null, {patt :> #, _ -> $Failed}, HoldAll] @@ vars]
                                ]
                            ]
                        ],
                        AssociationThread[
                            Extract[labelPatterns, freeVertexPositions],
                            With[{labelRules = Lookup[vertexAnnotations, VertexLabels, {}]},
                                Replace[Replace[#, labelRules], Automatic | "Name" -> #] & /@ Replace[inputFreeVertices, matchVertexMap, 1]
                            ]
                        ]
                    ]
                ];
                If[FailureQ[binding], Return[Nothing, Block]];
                bindingRules = Normal @ KeyMap[Replace[Verbatim[Pattern][sym_Symbol, _] :> HoldPattern[sym]]] @ binding;
                
                origVertexMap = Association[
                    matchVertexMap,
                    newVertexMap
                ];

                deleteOrigVertices = Replace[deleteVertices, origVertexMap, 1];
                (* Hold to handle Sequence multi arity matches *)
                origVertexMap = Hold /@ origVertexMap;
                edgeArities = First[#, {}] & @ Reap[newEdges = Replace[newOutputEdges,
                    {
                        e : (edge_ -> tag_) :> ((Sow[Length /@ #]; ReleaseHold[#]) & @ CanonicalEdge[Replace[edge, origVertexMap, 1], Lookup[outputSymmetry, Key[e], {}]]) -> (tag /. bindingRules),
                        edge_ :> ((Sow[Length /@ #]; ReleaseHold[#]) & @ CanonicalEdge[Replace[edge, origVertexMap, 1], Lookup[outputSymmetry, Key[edge], {}]])
                    },
                    1
                ]][[2]];
                holdOutputVertices = Replace[newOutputVertices, origVertexMap, 1];
                <| "Hypergraph" -> Hypergraph[
                        Union[DeleteElements[vertices, deleteOrigVertices], ReleaseHold @ holdOutputVertices],
                        Insert[
                            Replace[Delete[edges, pos], {(edge_ -> tag_) :> DeleteElements[edge, deleteOrigVertices] -> tag, edge_ :> DeleteElements[edge, deleteOrigVertices]}, 1],
                            Splice @ newEdges,
                            Min[pos, Length[edges] + 1]
                        ],
                        Normal @ Map[DeleteDuplicates] @ MapAt[Function[Null, Unevaluated[#] /. bindingRules, HoldAll], {Key[VertexLabels], All, 2}] @ Merge[{vertexAnnotations, outputVertexAnnotations},
                           Apply[Join[
                                Catenate[Thread[#, List, 1] & /@ MapAt[List @* ReleaseHold, {All, 1}] @ Thread[holdOutputVertices -> Lookup[#2, newOutputVertices]]],
                                DeleteCases[#1, (Rule | RuleDelayed)[Alternatives @@ deleteOrigVertices, _]]
                            ] &]
                        ],
                        Normal @ Map[DeleteDuplicates] @ MapAt[Function[Null, Unevaluated[#] /. bindingRules, HoldAll], {Key[EdgeLabels], All, 2}] @ Merge[{edgeAnnotations, outputEdgeAnnotations},
                           Apply[Join[
                                Replace[#2,
                                    {
                                        (h : (Rule | RuleDelayed))[e : (edge_List -> tag_), annotation_] :>
                                            h[ReleaseHold @ CanonicalEdge[Replace[edge, origVertexMap, 1], Lookup[outputSymmetry, Key[e], {}]] -> (tag /. bindingRules), annotation],
                                        (h : (Rule | RuleDelayed))[edge_List, annotation_] :>
                                            h[ReleaseHold @ CanonicalEdge[Replace[edge, origVertexMap, 1], Lookup[outputSymmetry, Key[edge], {}]], annotation]
                                    },
                                    1
                                ],
                                Replace[
                                    DeleteCases[#1, Alternatives @@ matchEdges | (Alternatives @@ matchEdges -> _) -> _],
                                    {
                                        (h_[edge_ -> tag_]) :> h[DeleteElements[edge, deleteOrigVertices] -> tag],
                                        h_[edge_] :> h[DeleteElements[edge, deleteOrigVertices]]
                                    },
                                    1
                                ]
                            ] &]
                        ],
                        VertexCoordinates -> embedding,
                        PlotLabel -> (Lookup[outputAnnotationRules, PlotLabel, None] /. bindingRules),
                        FilterRules[annotationRules, Except[Join[vertexAnnotations, edgeAnnotations]]]
                    ],
                    "MatchVertices" -> Join[matchFreeVertices, matchVertices],
                    "MatchEdges" -> matchEdges,
                    "MatchEdgePositions" -> pos,
                    "NewVertices" -> Values[newVertexMap],
                    "NewEdges" -> newEdges,
                    "DeletedVertices" -> deleteOrigVertices,
                    "RuleVertexMap" -> Replace[origVertexMap, Hold[p_] :> p, 1],
                    "Bindings" -> bindingRules,
                    "EdgeArities" -> edgeArities
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

(rule : HoldPattern[HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ, ruleOpts___]])[
    hg_ ? HypergraphQ, opts : OptionsPattern[HypergraphRuleApply]
] /; HypergraphRuleQ[rule] = HypergraphRuleApply[input, output, hg, opts, ruleOpts]



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
                Hypergraph[
                    hg,
                    EdgeStyle -> MapAt[#[[1]] -> OptionValue["HighlightLeftEdgeStyle"] &, Lookup[hgOpts, EdgeStyle], #MatchEdgePositions],
                    VertexStyle -> Map[# -> OptionValue["HighlightLeftVertexStyle"] &, #MatchVertices]
                ],
                plotOpts,
                PlotRange -> Automatic,
                PlotRangePadding -> Scaled[.25],
                $HypergraphRulePlotOptions
            ],
            Graphics[{GrayLevel[0.65], $HypergraphRuleArrow}],
            SimpleHypergraphPlot[
                Hypergraph[
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
                    ]
                ],
                plotOpts,
                PlotRange -> Automatic,
                PlotRangePadding -> Scaled[.25],
                $HypergraphRulePlotOptions
            ]
        }, PlotRangePadding -> 1] &,
        matches
    ]
]



HypergraphRuleQ[hr_HypergraphRule] := System`Private`HoldValidQ[hr] ||
    MatchQ[Unevaluated[hr], HoldPattern[HypergraphRule[input_, output_, ___]] /; HypergraphQ[Unevaluated[input]] && HypergraphQ[Unevaluated[output]]]

HypergraphRuleQ[___] := False

Options[HypergraphRule] := Join[Options[HypergraphRuleApply], Options[Hypergraph]]

HoldPattern[HypergraphRule[input_, _, ___] ? HypergraphRuleQ]["Input"] := input

HoldPattern[HypergraphRule[_, output_, ___] ? HypergraphRuleQ]["Output"] := output

HypergraphRule[hr_HypergraphRule ? HypergraphRuleQ, opts : OptionsPattern[]] := With[{hOpts = FilterRules[{opts}, Options[Hypergraph]]},
    HypergraphRule[Hypergraph[hr["Input"], hOpts], Hypergraph[hr["Output"], hOpts], FilterRules[{opts}, Except[Options[Hypergraph]]]]
]

(hr : HoldPattern[HypergraphRule[input_, output_, opts : OptionsPattern[]]]) /; ! HypergraphRuleQ[Unevaluated[hr]] :=
    Enclose[
        System`Private`HoldSetValid @ HypergraphRule[##, FilterRules[{opts}, Options[HypergraphRuleApply]]] & [
            ConfirmBy[Hypergraph[input, FilterRules[{opts}, Options[Hypergraph]]], HypergraphQ],
            ConfirmBy[Hypergraph[output, FilterRules[{opts}, Options[Hypergraph]]], HypergraphQ]
        ]
    ]

