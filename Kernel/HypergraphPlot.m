Package["WolframInstitute`Hypergraph`"]

PackageExport["SimpleHypergraphPlot"]
PackageExport["SimpleHypergraphPlot3D"]



Options[SimpleHypergraphPlot] = Join[{
    ColorFunction -> ColorData[97],
    "EdgeArrows" -> False,
    "EdgeType" -> "Cyclic",
    VertexLabels -> None
}, Options[Hypergraph], Options[Graphics], Options[Graphics3D]];

SimpleHypergraphPlot[h : {___List}, args___] := SimpleHypergraphPlot[Hypergraph[h], args]

SimpleHypergraphPlot[h_Hypergraph, plotOpts : OptionsPattern[]] := Enclose @ Block[{
    graph,
    vertexEmbedding, edgeEmbedding,
    vs = h["VertexList"], es = h["EdgeList"],
    longEdges, ws,
    colorFunction, vertexLabels, edgeArrowsQ, edgeType,
    vertexStyle, vertexLabelStyle, edgeStyle,
    size, dim,
    opts = FilterRules[{h["Options"], plotOpts}, Options[SimpleHypergraphPlot]]
},
    colorFunction = OptionValue[SimpleHypergraphPlot, opts, ColorFunction];
    vertexLabels = OptionValue[SimpleHypergraphPlot, opts, VertexLabels];
    vertexStyle = Replace[OptionValue[SimpleHypergraphPlot, opts, VertexStyle], {Automatic -> _ -> Black, s : Except[_Rule] :> _ -> s}];
    vertexLabelStyle = Replace[OptionValue[SimpleHypergraphPlot, opts, VertexLabelStyle], {Automatic -> _ -> Black, s : Except[_Rule] :> _ -> s}];
    edgeStyle = Replace[OptionValue[SimpleHypergraphPlot, opts, EdgeStyle], {Automatic -> {}, s : Except[_Rule] :> _ -> s}];
    edgeArrowsQ = TrueQ[OptionValue[SimpleHypergraphPlot, opts, "EdgeArrows"]];
    edgeType = OptionValue[SimpleHypergraphPlot, opts, "EdgeType"];
    dim = ConfirmMatch[OptionValue[SimpleHypergraphPlot, opts, "LayoutDimension"], 2 | 3];

    longEdges = Cases[es, {_, _, __}];
    ws = Join[vs, \[FormalE] /@ Range[Length[longEdges]]];
	graph = Switch[dim, 2, Graph, 3, Graph3D][
        ws,
        Join[
            Annotation[DirectedEdge[##], EdgeWeight -> 1] & @@@ Cases[es, {_, _}],
            Catenate[
                MapIndexed[{edge, i} |->
                    With[{clickEdges = Join[#, Reverse /@ #] & @ Subsets[edge, {2}], weight = Length[edge]},
                        Join[
                            Annotation[DirectedEdge[##, edge], EdgeWeight -> 1] & @@@ clickEdges,
                            Annotation[DirectedEdge[#, \[FormalE] @@ i], EdgeWeight -> weight] & /@ edge
                        ]
                    ],
                    longEdges
                ]
            ]
        ],
        VertexShapeFunction -> ((Sow[#2 -> #1, "v"]; Point[#1]) &),
        EdgeShapeFunction -> ((Sow[#2 -> #1, "e"]; GraphComputation`GraphElementData["Line"][#1, None]) &),
        FilterRules[FilterRules[{opts}, Except[EdgeStyle -> _]], Options[Graph]],
        GraphLayout -> {"SpringEmbedding", "EdgeWeighted" -> True}
    ];
    {vertexEmbedding, edgeEmbedding} = First[#, {}] & /@ Reap[GraphPlot[graph], {"v", "e"}][[2]];
	vertexEmbedding = Chop /@ Association[vertexEmbedding][[Key /@ vs]];
    edgeEmbedding = Chop /@ Merge[edgeEmbedding, Identity];
    size = Max[1, #2 - #1 & @@@ CoordinateBounds[Values[vertexEmbedding]]];
	Switch[dim, 2, Graphics, 3, Graphics3D][{
		Opacity[.5],
		Arrowheads[{{Medium, .5}}],
		AbsoluteThickness[Medium],
		MapIndexed[With[{edge = #1[[1]], emb = Replace[#1[[1]], vertexEmbedding, {1}], mult = #1[[2]], i = #2[[1]]}, {
                Replace[edge, Append[Flatten[{edgeStyle}], _ -> Directive[colorFunction[i], EdgeForm[colorFunction[i]]]]],
                Switch[Length[emb],
                    0, Nothing,
                    1, Block[{r = size 0.03, dr = size 0.01}, Table[Switch[dim, 2, Disk[First[emb], r += dr], 3, Sphere[First[emb], r += dr], _, Nothing], mult]],
                    2, If[edgeArrowsQ, Map[Arrow], Identity] @ GraphComputation`GraphElementData["Line"][#, None] & /@ Lookup[edgeEmbedding, DirectedEdge @@ #1[[1]]],
                    _, {
                        Table[
                            With[{curves = Catenate[GraphComputation`GraphElementData["Line"][#, None] & /@ #[[All, j]]]}, {
                                Switch[dim,
                                    2, FilledCurve[curves],
                                    3, Block[{pts, region},
                                        pts = MeshCoordinates @ DiscretizeGraphics @ curves;
                                        region = DiscretizeRegion[#, MaxCellMeasure -> {"Area" -> Area[#] / (Length[pts] + 1)}] & @
                                            Polygon[Prepend[First[pts]] /@ Partition[pts[[2 ;; -2]], 2, 1]];
                                        {EdgeForm[], areaGradientDescent[region, .1, 20]}
                                    ]
                                    (* 3, Polygon @ Catenate @ #[[All, j]] *)
                                ],
                                If[ edgeArrowsQ,
                                    With[{lengths = RegionMeasure @* DiscretizeGraphics /@ curves},
                                        {   Arrowheads[{Medium, #} & /@ ((Prepend[Accumulate[Most[lengths]], 0] + lengths / 2) / Total[lengths])],
                                            Arrow @ JoinedCurve[curves]
                                        }
                                    ],
                                    Nothing
                                ]
                            }] & @ Lookup[edgeEmbedding, DirectedEdge[##, edge] & @@@
                                Partition[#1[[1]], 2, 1, If[edgeType === "Cyclic", 1, None]]
                            ],
                            {j, mult}
                        ]
                    }
                ]
            }] &,
            Tally[es]
        ],
        Opacity[1],
		KeyValueMap[{Replace[#1, vertexStyle], Point[#2]} &, vertexEmbedding],
        Switch[vertexLabels,
            Automatic, KeyValueMap[{Replace[#1, vertexLabelStyle], Text[##, {2, 2}]} &, vertexEmbedding],
            Placed[Automatic, _Offset], KeyValueMap[{Replace[#1, vertexLabelStyle], Text[##, vertexLabels[[2, 1]]]} &, vertexEmbedding],
            _, Nothing
        ]
	},
        FilterRules[{opts}, Options[Switch[dim, 2, Graphics, 3, Graphics3D]]],
        ImageSize -> Medium,
		Boxed -> False
	]
]


SimpleHypergraphPlot3D[h_, opts___] := SimpleHypergraphPlot[h, "LayoutDimension" -> 3, opts]

