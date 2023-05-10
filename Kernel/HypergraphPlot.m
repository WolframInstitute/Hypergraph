Package["WolframInstitute`Hypergraph`"]

PackageExport["SimpleHypergraphPlot"]
PackageExport["SimpleHypergraphPlot3D"]
PackageExport["HypergraphEmbedding"]

PackageScope["makeAnnotationRules"]



makeVertexLabel[vertex_, label_, style_, pos_] := Replace[label /. "Name" -> vertex, {
    None -> Nothing,
    Automatic :> Text[Style[vertex, style], pos, {1, 1}],
    Placed[placedLabel_, offset_] :> Text[Style[placedLabel, style], pos, offset],
    l_ :> Text[Style[l, style], pos, {1, 1}]
}]


makeAnnotationRules[opts_List] := Association @ KeyValueMap[
    #1 -> Block[{automatic, default},
        If[ MatchQ[#2, {_, _}],
            {automatic, default} = #2,
            automatic = default = #2
        ];
        Append[Replace[Flatten[ReplaceList[#1, opts]], {Automatic -> _ -> automatic, s : Except[_Rule | _RuleDelayed] :> _ -> s}, {1}], _ -> default]
    ] &,
    $DefaultHypergraphAnnotations
]

Options[SimpleHypergraphPlot] := Join[Options[Hypergraph], Options[Graphics], Options[Graphics3D]];

SimpleHypergraphPlot[h : {___List}, args___] := SimpleHypergraphPlot[Hypergraph[h], args]

SimpleHypergraphPlot[h_Hypergraph, plotOpts : OptionsPattern[]] := Enclose @ Block[{
    graph,
    vertexEmbedding, edgeEmbedding,
    vs = h["VertexList"], es = h["EdgeList"], edgeTags = h["EdgeTags"],
    nullEdges, longEdges, ws,
    colorFunction, edgeArrowsQ, edgeType,
    vertexStyle, vertexLabels, vertexLabelStyle,
    edgeStyle, edgeLabels, edgeLabelStyle, edgeSymmetries,
    vertexCoordinates,
    bounds, corner, size, dim,
    opts = FilterRules[{plotOpts, h["Options"]}, Options[SimpleHypergraphPlot]],
    edgeIndex,
    makeEdge
},
    edgeIndex = PositionIndex[es];
    colorFunction = OptionValue[SimpleHypergraphPlot, opts, ColorFunction];
    {vertexStyle, vertexLabels, vertexLabelStyle, edgeStyle, edgeLabels, edgeLabelStyle, edgeSymmetries} = Values @ makeAnnotationRules[opts];
    edgeSymmetries = Replace[es, edgeSymmetries, {1}];
    edgeArrowsQ = TrueQ[OptionValue[SimpleHypergraphPlot, opts, "EdgeArrows"]];
    edgeType = OptionValue[SimpleHypergraphPlot, opts, "EdgeType"];
    dim = ConfirmMatch[OptionValue[SimpleHypergraphPlot, opts, "LayoutDimension"], 2 | 3];

    nullEdges = \[FormalN] /@ Range[Count[es, {}]];
    longEdges = Cases[es, {_, _, __}];
    ws = Join[vs, nullEdges, \[FormalE] /@ Range[Length[longEdges]]];

    vertexCoordinates = Replace[OptionValue[SimpleHypergraphPlot, opts, VertexCoordinates], Except[{___Rule}] -> {}];
    vertexCoordinates = Join[vertexCoordinates, Thread[Complement[ws, vertexCoordinates[[All, 1]]] -> Automatic]];
    vertexCoordinates = MapAt[Replace[coords_List :> PadRight[coords, dim]], vertexCoordinates, {All, 2}];
    vertexCoordinates = Replace[vertexCoordinates, {(_ -> Automatic)...} -> Automatic];

	graph = Switch[dim, 2, Graph, 3, Graph3D][
        ws,
        Join[
            Annotation[DirectedEdge[##], EdgeWeight -> 1] & @@@ Cases[es, {_, _}],
            Catenate[
                MapIndexed[{edge, i} |->
                    With[{clickEdges = Catenate[{#, If[SameQ @@ #, Nothing, Reverse[#]]} & /@ #] & @ Subsets[edge, {2}], weight = Length[edge]},
                        Join[
                            Annotation[DirectedEdge[##, edge], EdgeWeight -> 1] & @@@ clickEdges,
                            If[ DuplicateFreeQ @ edge,
                                Annotation[DirectedEdge[#, \[FormalE] @@ i], EdgeWeight -> weight] & /@ edge,
                                {}
                            ]
                        ]
                    ],
                    longEdges
                ]
            ]
        ],
        VertexShapeFunction -> ((Sow[#2 -> #1, "v"]; Point[#1]) &),
        EdgeShapeFunction -> ((Sow[#2 -> #1, "e"]; GraphComputation`GraphElementData["Line"][#1, None]) &),
        VertexCoordinates -> vertexCoordinates,
        FilterRules[
            FilterRules[{opts}, Except[
                VertexStyle | EdgeStyle |
                VertexLabels | EdgeLabels | VertexLabelStyle | EdgeLabelStyle |
                VertexCoordinates
            ]],
            Options[Graph]
        ],
        GraphLayout -> {"SpringEmbedding", "EdgeWeighted" -> True}
    ];
    {vertexEmbedding, edgeEmbedding} = First[#, {}] & /@ Reap[GraphPlot[graph], {"v", "e"}][[2]];
	edgeEmbedding = Chop /@ Join[Merge[edgeEmbedding, Identity], Association[vertexEmbedding][[Key /@ nullEdges]]];
    vertexEmbedding = Chop /@ Association[vertexEmbedding][[Key /@ vs]];
    bounds = CoordinateBounds[Values[vertexEmbedding]];
    corner = bounds[[All, 1]];
    size = Max[1, #2 - #1 & @@@ bounds];

    makeEdge[edge_, tag_, symm_, i_, j_, initPrimitive_] := Block[{
        primitive,
        pos = Replace[RegionCentroid[BoundingRegion @ If[RegionQ[initPrimitive], Identity, DiscretizeGraphics] @ initPrimitive], {} -> corner],
        edgeTagged, style, label, labelStyle, labelPrimitive
    },
        edgeTagged = If[tag === None, edge, edge -> tag];
        style = With[{defStyle = Directive[colorFunction[i], EdgeForm[Transparent]]},
            If[Length[#] > 1, ResourceFunction["LookupPart"][#, j, defStyle], Replace[Last[#, defStyle], Automatic -> defStyle]] & @ ReplaceList[edgeTagged, edgeStyle]
        ];
        label = If[Length[#] > 1, ResourceFunction["LookupPart"][#, j, None], Last[#, None]] & @ ReplaceList[edgeTagged, edgeLabels];
        labelStyle = If[Length[#] > 1, ResourceFunction["LookupPart"][#, j, Last[#, {}]], Last[#, {}]] & @ ReplaceList[edgeTagged, edgeLabelStyle];
        labelPrimitive = Replace[label /. {"Name" -> edge, "EdgeTag" -> tag, "EdgeSymmetry" -> symm}, {
            None -> Nothing,
            Automatic :> Text[edge, pos],
            Placed[placedLabel_, offset_] :> Text[placedLabel, pos, offset],
            label_ :> Text[label, pos]
        }];
        primitive = initPrimitive /. _EmptyRegion -> {};
        {
            If[MatchQ[pos, {_, _, _}], EdgeForm[], Nothing],
            style,
            If[ MatchQ[label, Placed[_, Tooltip]],
                Tooltip[primitive, Replace[labelPrimitive, Text[expr_, ___] :> Style[expr, labelStyle]]],
                {primitive, Replace[labelPrimitive, Text[expr_, args___] :> Text[Style[expr, labelStyle], args]]}
            ]
        }
    ];

	Switch[dim, 2, Graphics, 3, Graphics3D][{
		Opacity[.5],
		Arrowheads[{{Medium, .5}}],
		AbsoluteThickness[Medium],
		MapIndexed[Block[{
            edge = #1[[1]],
            emb = Replace[#1[[1]], vertexEmbedding, {1}],
            mult = #1[[2]], i = #2[[1]],
            position, primitive
        },
            {
                Switch[Length[emb],
                    0, Table[
                        Sow[position = edgeIndex[edge][[j]], "Position"];
                        Sow[primitive = Switch[dim, 2, Circle, 3, Sphere][Lookup[edgeEmbedding, \[FormalN][j]], size 0.03], "Primitive"];
                        makeEdge[edge, edgeTags[[ position ]], edgeSymmetries[[ position ]], i, j, primitive],
                        {j, mult}
                    ],
                    1, Block[{r = size 0.03, dr = size 0.01},
                        Table[
                            Sow[position = edgeIndex[edge][[j]], "Position"];
                            Sow[primitive = Switch[dim, 2, Disk[First[emb], r += dr], 3, Sphere[First[emb], r += dr], _, Nothing], "Primitive"];
                            makeEdge[edge, edgeTags[[ position ]], edgeSymmetries[[ position ]], i, j, primitive],
                            {j, mult}
                        ]
                    ],
                    2, MapIndexed[
                        (
                            Sow[position = edgeIndex[edge][[#2[[1]]]], "Position"];
                            Sow[primitive = #1[[1]], "Primitive"];
                            With[{symm = edgeSymmetries[[ position ]]},
                                makeEdge[edge, edgeTags[[ position ]], symm, i, #2[[1]],
                                    If[edgeArrowsQ || MatchQ[symm, "Ordered" | "Directed" | {}], Arrow, Identity] @ primitive
                                ]
                            ]
                        ) &,
                        GraphComputation`GraphElementData["Line"][#, None] /. BezierCurve -> BSplineCurve & /@ Lookup[edgeEmbedding, DirectedEdge @@ edge]
                    ],
                    _, Block[{counts = <||>, points},
                        Table[
                            Sow[position = edgeIndex[edge][[j]], "Position"];
                            points = With[{c = Lookup[counts, #, 0] + 1},
                                AppendTo[counts, # -> c];
                                Lookup[edgeEmbedding, #][[c]]
                            ] & /@ (DirectedEdge[##, edge] & @@@ Partition[#1[[1]], 2, 1, If[edgeType === "Cyclic", 1, None]]);
                            With[{curves = Catenate[GraphComputation`GraphElementData["Line"][#, None] /. BezierCurve -> BSplineCurve & /@ points]}, {
                                Switch[dim,
                                    2, Sow[primitive = FilledCurve[curves], "Primitive"],
                                    3, Block[{pts, region},
                                        pts = MeshCoordinates @ DiscretizeGraphics @ curves;
                                        region = DiscretizeRegion[#, MaxCellMeasure -> {"Area" -> Area[#] / (Length[pts] + 1)}] & @
                                            Polygon[Prepend[First[pts]] /@ Partition[pts[[2 ;; -2]], 2, 1]];
                                        Sow[primitive = areaGradientDescent[region, .1, 20], "Primitive"]
                                    ]
                                ];
                                With[{symm = edgeSymmetries[[ position ]]},
                                    makeEdge[edge, edgeTags[[ position ]], symm, i, j,
                                        If[ edgeArrowsQ || MatchQ[symm, "Ordered" | "Directed" | {}],
                                            With[{lengths = RegionMeasure @* DiscretizeGraphics /@ curves},
                                                {   primitive,
                                                    Arrowheads[{Medium, #} & /@ ((Prepend[Accumulate[Most[lengths]], 0] + lengths / 2) / Total[lengths])],
                                                    Switch[dim, 2, Arrow @ JoinedCurve[curves], 3, Arrow /@ curves]
                                                }
                                            ],
                                            primitive
                                        ]
                                    ]
                                ]
                            }],
                            {j, mult}
                        ]
                    ]
                ]
            }] &,
            Tally[es]
        ],
        Opacity[1],
		KeyValueMap[{Replace[#1, vertexStyle], Point[#2]} &, vertexEmbedding],
        KeyValueMap[With[{label = Replace[#1, vertexLabels], style = Replace[#1, vertexLabelStyle]},
            makeVertexLabel[#1, label, style, #2]
        ] &,
            vertexEmbedding
        ]
	},
        FilterRules[{opts}, Options[Switch[dim, 2, Graphics, 3, Graphics3D]]],
        ImageSize -> Medium,
		Boxed -> False
	]
]


SimpleHypergraphPlot3D[h_, opts___] := SimpleHypergraphPlot[h, "LayoutDimension" -> 3, opts]


HypergraphEmbedding[hg_Hypergraph ? HypergraphQ] := Cases[SimpleHypergraphPlot[hg], Point[p_, ___] :> p, Infinity]

