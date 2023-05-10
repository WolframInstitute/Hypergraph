Package["WolframInstitute`Hypergraph`"]


PackageExport["HypergraphRuleQ"]
PackageExport["HypergraphRule"]
PackageExport["ToLabeledEdges"]
PackageExport["ToLabeledPatternEdges"]
PackageExport["ToPatternRules"]
PackageExport["PatternRuleToMultiReplaceRule"]
PackageExport["HighlightRule"]



makeVertexLabelPattern[vertex_, label_, makePattern_ : False] := Replace[label, {
    None :> If[TrueQ[makePattern], _, vertex],
    Automatic | "Name" :> If[TrueQ[makePattern], Pattern[#, _] & @ Symbol["\[FormalL]" <> StringDelete[ToString[vertex, InputForm], Except[WordCharacter]]], vertex],
    Placed[Automatic | "Name", _] :> vertex,
    Placed[placedLabel_, _] :> placedLabel,
    _ :> label
}]

ToLabeledEdges[vertexLabels_Association, edges : {___List}, makePattern_ : False] := Block[{labels, patterns, varSymbols, labeledEdges},
    {labels, patterns} = Reap[
        varSymbols = KeyValueMap[#1 ->
            If[ TrueQ[makePattern],
                Labeled[
                    Sow[Pattern[#, _] & @ Symbol["\[FormalX]" <> StringDelete[ToString[#1], Except[WordCharacter]]], "VertexPattern"],
                    Sow[#2, "Label"]
                ],
                Labeled[#1, #2]
            ] &, vertexLabels],
        {"Label", "VertexPattern"}
    ][[2]];
    Scan[Sow[#, "Label"] &, First[labels, {}]];
    Scan[Sow[#, "VertexPattern"] &, First[patterns, {}]];
    labeledEdges = Replace[edges, varSymbols, {2}];
    If[ TrueQ[makePattern],
        Condition[labeledEdges, UnsameQ[##]] & @@
            DeleteDuplicates @ Cases[First[labels, {}], Verbatim[Pattern][label_, _] :> label, All],
        labeledEdges
    ]
]

ToLabeledEdges[hg_ ? HypergraphQ, makePattern_ : False] := Block[{
    vs = VertexList[hg],
    vertexLabelRules = makeAnnotationRules[hg["Options"], VertexLabels],
    vertexLabels,
    edgeSymmetry = Values[EdgeSymmetry[hg]],
    edgeTags = EdgeTags[hg]
},
    vertexLabels = AssociationMap[makeVertexLabelPattern[#, Replace[#, vertexLabelRules], makePattern] &, vs];
    MapAt[
        MapIndexed[With[{edge = #1, symm = edgeSymmetry[[#2[[1]]]], tag = edgeTags[[#2[[1]]]]},
            Labeled[edge, {symm, If[makePattern, Replace[tag, None -> _], tag]}]] &
        ],
        ToLabeledEdges[vertexLabels, hg["EdgeList"], makePattern],
        If[makePattern, {1}, {{}}]
    ]
]

ToLabeledPatternEdges[hg_ ? HypergraphQ] := Block[{
    edges = EdgeList[hg],
    simpleEdgeSymmetry
},
    simpleEdgeSymmetry = Replace[edges, Replace[Flatten[{hg["EdgeSymmetry"]}], {Automatic -> _ -> "Unordered", s : Except[_Rule] :> _ -> s}, {1}], {1}];
    Which[
        AllTrue[simpleEdgeSymmetry, MatchQ["Unordered" | "Undirected"]],
        MapAt[{OrderlessPatternSequence @@ #} &, #, {1, All, 1}],
        AllTrue[simpleEdgeSymmetry, MatchQ["Ordered" | "Directed"]],
        #,
        True,
        With[{edgeSymmetry = Values[EdgeSymmetry[hg]]},
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
    ] & @ ToLabeledEdges[hg, True]
]


ToPatternRules[lhs : {___List}, rhs : {___List}] := Block[{vs = Union @@ lhs, ws = Union @@ rhs, varSymbols, newVars},
    varSymbols = Association @ MapIndexed[#1 -> Symbol["\[FormalX]" <> ToString[#2[[1]]]] &, Union[vs, ws]];
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


Options[HypergraphRuleApply] = {"Bindings" -> Automatic, "Symmetry" -> Automatic}

HypergraphRuleApply[input_, output_, hg_, opts : OptionsPattern[]] := Block[{
    vertices = VertexList[hg], edges = hg["EdgeListTagged"],
    inputVertices = VertexList[input], outputVertices = VertexList[output],
    inputEdges = EdgeList[input], outputEdges = output["EdgeListTagged"],
    bindingsMethod = Replace[OptionValue["Bindings"], {First -> (Take[#, UpTo[1]] &), _ -> Identity}],
    symmetryMethod = Replace[OptionValue["Symmetry"], {Automatic -> (_ &), _ -> Identity}],
    annotationRules, outputAnnotationRules,
    vertexAnnotations, outputVertexAnnotations,
    edgeAnnotations, outputEdgeAnnotations,
    embedding,
    matches,
    lhsVertices, inputFreeVertices, newVertices, deleteVertices, newVertexMap
},
    patterns = First[
        Reap[
            matches = Thread @ DeleteDuplicatesBy[Sort @* First] @ Keys @ ResourceFunction["MultiReplace"][
                ToLabeledEdges[hg],
                MapAt[symmetryMethod, ToLabeledPatternEdges[input], {1, All, 2, 1}],
                {1},
                "PatternSubstitutions" -> True,
                "Mode" -> "OrderlessSubsets"
            ],
            "VertexPattern"
        ][[2]],
        {}
    ];
    lhsVertices = Union @@ inputEdges;
    inputFreeVertices = Complement[inputVertices, lhsVertices];
    newVertices = Complement[outputVertices, inputVertices];
    deleteVertices = Complement[inputVertices, outputVertices];
    newVertexMap = Block[{$ModuleNumber = 1}, # -> Unique["\[FormalX]"] & /@ newVertices];

    annotationRules = makeAnnotationRules[hg["Options"]];
    outputAnnotationRules = makeAnnotationRules[output["Options"]];

    {vertexAnnotations, outputVertexAnnotations} = MapThread[{vs, rules} |->
        Append[
            Map[Thread[vs -> Replace[vs, #, {1}]] &, rules[[Key /@ {VertexStyle, VertexLabelStyle}]]],
            VertexLabels -> Function[Null, Thread[vs :> #], HoldAll] @@ Replace[Hold[vs], rules[VertexLabels], {2}]
        ],
        {{vertices, outputVertices}, {annotationRules, outputAnnotationRules}}
    ];
    {edgeAnnotations, outputEdgeAnnotations} = MapThread[{es, rules} |->
        Append[
            Map[Thread[es -> Replace[es, #, {1}]] &, rules[[Key /@ {EdgeStyle, EdgeLabelStyle, "EdgeSymmetry"}]]],
            EdgeLabels -> Function[Null, Thread[es :> #], HoldAll] @@ Replace[Hold[es], rules[EdgeLabels], {2}]
        ],
        {{edges, outputEdges}, {annotationRules, outputAnnotationRules}}
    ];

    {vertexStyles, outputVertexStyles} = MapThread[{h, vs} |->
        Thread[vs ->
            Replace[
                vs,
                Append[
                    Replace[Flatten[{OptionValue[SimpleHypergraphPlot, h["Options"], VertexStyle]}], {Automatic -> Nothing, s : Except[_Rule | _RuleDelayed] :> _ -> s}, {1}],
                    _ -> Black
                ],
                {1}
            ]
        ],
        {{hg, output}, {vertices, outputVertices}}
    ];

    {edgeAnnotations, outputEdgeAnnotations} = MapThread[{h, es, annotations} |->
        MapAt[
            Join[
                #,
                MapIndexed[#1 -> Directive[OptionValue[SimpleHypergraphPlot, h["Options"], ColorFunction][#2[[1]]], EdgeForm[Transparent]] &, es]
            ] &,
            annotations,
            Key[EdgeStyle]
        ],
        {{hg, output}, {edges, outputEdges}, {edgeAnnotations, outputEdgeAnnotations}}
    ];
    outputEdgeAnnotations = MapAt["Ordered" &, outputEdgeAnnotations, {Key["EdgeSymmetry"], All, 2}];

    embedding = Thread[vertices -> HypergraphEmbedding[hg]];

    Catenate @ MapThread[{pos, bindings} |-> Block[{
        matchEdges = Replace[Extract[edges, pos], (edge_ -> _) :> edge, {1}],
        matchVertices, matchVertexMap
    },
        Catenate @ Map[binding |-> (
            matchVertices = Union @@ matchEdges;
            matchVertexMap = Thread[inputVertices -> (patterns /. binding)];
            Block[{
                origVertexMap = Join[
                    matchVertexMap,
                    Thread[inputFreeVertices -> #],
                    newVertexMap
                ],
                deleteOrigVertices, newEdges,
                bindingReplace = Normal @ KeyMap[Replace[Verbatim[Pattern][sym_Symbol, _] :> HoldPattern[sym]]] @ binding
            },
                deleteOrigVertices = Replace[deleteVertices, origVertexMap, {1}];
                newEdges = Replace[outputEdges, {
                        (edge_ -> tag_) :> Replace[edge, origVertexMap, {1}] ->
                            (tag /. bindingReplace),
                        edge_ :> Replace[edge, origVertexMap, {1}]
                    },
                    {1}
                ];
                <| "Hypergraph" -> Hypergraph[
                        Union[DeleteElements[vertices, deleteOrigVertices], Replace[outputVertices, origVertexMap, {1}]],
                        Insert[
                            Replace[Delete[edges, pos], {(edge_ -> tag_) :> DeleteElements[edge, deleteOrigVertices] -> tag, edge_ :> DeleteElements[edge, deleteOrigVertices]}, {1}],
                            Splice @ newEdges,
                            Min[pos]
                        ],
                        Normal @ MapAt[Function[Null, Unevaluated[#] /. bindingReplace, HoldAll], Key[VertexLabels]] @ Merge[{vertexAnnotations, outputVertexAnnotations},
                           Apply[Join[
                                Replace[#2, (h : (Rule | RuleDelayed))[vertex_, annotation_] :> h[Replace[vertex, origVertexMap], annotation], {1}],
                                DeleteCases[#1, (Rule | RuleDelayed)[Alternatives @@ deleteOrigVertices, _]]
                            ] &]
                        ],
                        Normal @ MapAt[Function[Null, Unevaluated[#] /. bindingReplace, HoldAll], Key[EdgeLabels]] @ Merge[{edgeAnnotations, outputEdgeAnnotations},
                           Apply[Join[
                                Replace[#2, {
                                    (h : (Rule | RuleDelayed))[edge_List -> tag_, annotation_] :> h[Replace[edge, origVertexMap, {1}] -> (tag /. bindingReplace), annotation],
                                    (h : (Rule | RuleDelayed))[edge_List, annotation_] :> h[Replace[edge, origVertexMap, {1}], annotation]
                                },
                                {1}
                                ],
                                DeleteCases[#1, Alternatives @@ matcheEdges | (Alternatives @@ matcheEdges -> _) -> _]
                            ] &]
                        ],
                        VertexCoordinates -> embedding,
                        PlotLabel -> (Lookup[output["Options"], PlotLabel, None] /. bindingReplace),
                        FilterRules[hg["Options"],
                            Except[VertexStyle | VertexLabels | VertexLabelStyle | VertexCoordinates | EdgeStyle | EdgeLabels | EdgeLabelStyle | "EdgeSymmetry"]
                        ]
                    ],
                    "MatchVertices" -> Join[#, matchVertices],
                    "MatchEdges" -> matchEdges,
                    "MatchEdgePositions" -> pos,
                    "NewVertices" -> Values[newVertexMap],
                    "NewEdges" -> newEdges,
                    "DeletedVertices" -> deleteOrigVertices,
                    "RuleVertexMap" -> origVertexMap
                |>
            ] & /@ Catenate[Permutations /@ Subsets[Complement[vertices, matchVertices], {Length[inputFreeVertices]}]]
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



Options[HighlightRule] := Join[Options[HypergraphRuleApply], Options[SimpleHypergraphPlot]]

HighlightRule[rule_ ? HypergraphRuleQ, hg_ ? HypergraphQ, opts : OptionsPattern[]] := Block[{
    vs = VertexList[hg], edges = EdgeList[hg],
    matches
},
    matches = rule[hg, FilterRules[{opts}, Options[HypergraphRuleApply]]];

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
                AspectRatio -> Automatic,
                $HypergraphRulePlotOptions
            ],
            Graphics[{GrayLevel[0.65], $HypergraphRuleArrow}, ImageSize -> Scaled[0.01]],
            SimpleHypergraphPlot[
                #Hypergraph,
                opts,
                EdgeStyle -> Map[
                    # -> Directive[Thick, Dashed, Red, EdgeForm[Directive[Dashed, Red, Thick]]] &,
                    (* output edges always getting spliced at the first position *)
                    #NewEdges
                ],
                VertexStyle -> Map[# -> Directive[PointSize[0.02], Red] &, #NewVertices],
                AspectRatio -> Automatic,
                $HypergraphRulePlotOptions
            ]
        }, PlotRangePadding -> 1] &,
        matches
    ]
]



HypergraphRuleQ[hr_HypergraphRule] := System`Private`HoldValidQ[hr] ||
    MatchQ[Unevaluated[hr], HoldPattern[HypergraphRule[input_, output_]] /; HypergraphQ[Unevaluated[input]] && HypergraphQ[Unevaluated[output]]]

HypergraphRuleQ[___] := $Failed

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

