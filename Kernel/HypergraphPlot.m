Package["WolframInstitute`Hypergraph`"]

PackageExport["SimpleHypergraphPlot"]
PackageExport["SimpleHypergraphPlot3D"]
PackageExport["HypergraphEmbedding"]



makeEdge[edge_, tag_, edgeLabels_, edgeLabelStyle_, primitive_, defaultPos_] := Block[{
    pos = Replace[RegionCentroid[If[RegionQ[primitive], Identity, DiscretizeGraphics] @ primitive], {} -> defaultPos],
    edgeTagged, label, style
},
    edgeTagged = If[tag === None, edge, edge -> tag];
    label = Replace[edgeTagged, edgeLabels];
    style = Replace[edgeTagged, edgeLabelStyle];
    {
        Replace[edgeTagged, Append[edgeStyle, _ -> Directive[colorFunction[i], EdgeForm[Transparent]]]],
        primitive /. _EmptyRegion -> Nothing,
        Replace[label, {
            None -> Nothing,
            Automatic | "Name" :> {style, Text[edge, pos]},
            "EdgeTag" :> {style, Text[tag, pos]},
            Placed[Automatic | "Name", offset_] :> {style, Text[edge, pos, offset]},
            Placed["EdgeTag", offset_] :> {style, Text[tag, pos, offset]},
            Placed[placedLabel_, offset_] :> {style, Text[placedLabel, pos, offset]},
            _ :> {style, Text[label, pos]}
        }]
    }
]

makeVertexLabel[vertex_, label_, style_, pos_] := Replace[label, {
    None -> Nothing,
    Automatic | "Name" :> {style, Text[vertex, pos, {1, 1}]},
    Placed[Automatic | "Name", offset_] :> {style, Text[vertex, pos, offset]},
    Placed[placedLabel_, offset_] :> {style, Text[placedLabel, pos, offset]},
    _ :> {style, Text[label, pos, {1, 1}]}
}]


Options[SimpleHypergraphPlot] := Join[Options[Hypergraph], Options[Graphics], Options[Graphics3D]];

SimpleHypergraphPlot[h : {___List}, args___] := SimpleHypergraphPlot[Hypergraph[h], args]

SimpleHypergraphPlot[h_Hypergraph, plotOpts : OptionsPattern[]] := Enclose @ Block[{
    graph,
    vertexEmbedding, edgeEmbedding,
    vs = h["VertexList"], es = h["EdgeList"], edgeTags = h["EdgeTags"],
    longEdges, ws,
    colorFunction, edgeArrowsQ, edgeType,
    vertexStyle,vertexLabels, vertexLabelStyle,
    edgeStyle, edgeLabels, edgeLabelStyle,
    vertexCoordinates,
    bounds, corner, size, dim,
    opts = FilterRules[{plotOpts, h["Options"]}, Options[SimpleHypergraphPlot]],
    edgeIndex
},
    edgeIndex = PositionIndex[es];
    colorFunction = OptionValue[SimpleHypergraphPlot, opts, ColorFunction];
    vertexLabels = Append[Replace[Flatten[ReplaceList[VertexLabels, opts]], {Automatic -> _ -> "Name", s : Except[_Rule] :> _ -> s}, {1}], _ -> None];
    vertexStyle = Append[Replace[Flatten[ReplaceList[VertexStyle, opts]], {Automatic -> _ -> Black, s : Except[_Rule] :> _ -> s}, {1}], _ -> Black];
    vertexLabelStyle = Append[Replace[Flatten[ReplaceList[VertexLabelStyle, opts]], {Automatic -> _ -> Black, s : Except[_Rule] :> _ -> s}, {1}], _ -> Black];
    edgeLabels = Append[Replace[Flatten[ReplaceList[EdgeLabels, opts]], {Automatic -> _ -> "Name", s : Except[_Rule] :> _ -> s}, {1}], _ -> None];
    edgeLabelStyle = Append[Replace[Flatten[ReplaceList[EdgeLabelStyle, opts]], {Automatic -> _ -> Black, s : Except[_Rule] :> _ -> s}, {1}], _ -> Black];
    edgeStyle = Replace[Flatten[ReplaceList[EdgeStyle, opts]], {Automatic -> Nothing, s : Except[_Rule] :> _ -> s}, {1}];
    edgeArrowsQ = TrueQ[OptionValue[SimpleHypergraphPlot, opts, "EdgeArrows"]];
    edgeType = OptionValue[SimpleHypergraphPlot, opts, "EdgeType"];
    dim = ConfirmMatch[OptionValue[SimpleHypergraphPlot, opts, "LayoutDimension"], 2 | 3];

    longEdges = Cases[es, {_, _, __}];
    ws = Join[vs, \[FormalE] /@ Range[Length[longEdges]]];

    vertexCoordinates = OptionValue[SimpleHypergraphPlot, opts, VertexCoordinates];
    If[ MatchQ[vertexCoordinates, {___Rule}],
        vertexCoordinates = Join[vertexCoordinates, Thread[Complement[ws, vertexCoordinates[[All, 1]]]  -> Automatic]]
    ];

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
	vertexEmbedding = Chop /@ Association[vertexEmbedding][[Key /@ vs]];
    edgeEmbedding = Chop /@ Merge[edgeEmbedding, Identity];
    bounds = CoordinateBounds[Values[vertexEmbedding]];
    corner = bounds[[All, 1]];
    size = Max[1, #2 - #1 & @@@ bounds];
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
                        Sow[primitive = EmptyRegion[2], "Primitive"];
                        makeEdge[edge, edgeTags[[ position ]], edgeLabels, edgeLabelStyle, primitive, corner],
                        {j, mult}
                    ],
                    1, Block[{r = size 0.03, dr = size 0.01},
                        Table[
                            Sow[position = edgeIndex[edge][[j]], "Position"];
                            Sow[primitive = Switch[dim, 2, Disk[First[emb], r += dr], 3, Sphere[First[emb], r += dr], _, Nothing], "Primitive"];
                            makeEdge[edge, edgeTags[[ position ]], edgeLabels, edgeLabelStyle, primitive, corner],
                            {j, mult}
                        ]
                    ],
                    2, MapIndexed[
                        (
                            Sow[position = edgeIndex[edge][[#2[[1]]]], "Position"];
                            Sow[primitive = #1[[1]], "Primitive"];
                            makeEdge[edge, edgeTags[[ position ]], edgeLabels, edgeLabelStyle, If[edgeArrowsQ, Arrow, Identity] @ primitive, corner]
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
                                If[ edgeArrowsQ,
                                    With[{lengths = RegionMeasure @* DiscretizeGraphics /@ curves},
                                        {   Arrowheads[{Medium, #} & /@ ((Prepend[Accumulate[Most[lengths]], 0] + lengths / 2) / Total[lengths])],
                                            Switch[dim, 2, Arrow @ JoinedCurve[curves], 3, Arrow /@ curves]
                                        }
                                    ],
                                    Nothing
                                ],
                                makeEdge[edge, edgeTags[[ position ]], edgeLabels, edgeLabelStyle, primitive, corner]
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


HypergraphEmbedding[hg_Hypergraph ? HypergraphQ] := Cases[SimpleHypergraphPlot[hg], Point[p_] :> p, Infinity]

