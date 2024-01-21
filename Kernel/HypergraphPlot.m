Package["WolframInstitute`Hypergraph`"]

PackageExport["SimpleHypergraphPlot"]
PackageExport["SimpleHypergraphPlot3D"]
PackageExport["HypergraphEmbedding"]

PackageScope["makeVertexLabel"]
PackageScope["$HypergraphPlotThemes"]



$HypergraphPlotThemes = <|
    Automatic -> {
        EdgeStyle -> Automatic,
        "EdgeLineStyle" -> Automatic,
        VertexStyle -> Automatic,
        VertexSize -> Automatic,
        VertexShapeFunction -> Automatic
    },
    "WolframModel" -> {
        EdgeStyle -> {
            {_, _} | {} -> Directive[Arrowheads[{{0.03, .5}}], Opacity[.7], Hue[0.63, 0.7, 0.5]],
            _ -> Directive[Opacity[0.1], Hue[0.63, 0.66, 0.81]]
        },
        "EdgeLineStyle" -> Directive[Opacity[.7], Hue[0.63, 0.7, 0.5]],
        VertexStyle -> Directive[Hue[0.63, 0.26, 0.89], EdgeForm[Directive[Hue[0.63, 0.7, 0.33], Opacity[0.95]]]],
        VertexSize -> 0.01,
        VertexShapeFunction -> Function[If[Length[#1] == 3, Sphere[#1, #3], Disk[#1, Offset[200 #3]]]]
    },
    "Dark" -> {
        VertexStyle -> White,
        EdgeStyle -> White,
        "EdgeLineStyle" -> White,
        Background -> Black,
        VertexLabelStyle -> White
    },
    "LinkedHypergraph" -> {
        VertexShapeFunction -> _HoldForm -> Function[Inset[
            Framed[Style[#2, FontSize -> 16, FontColor -> Black],
            Background -> White, FrameMargins -> {4 {1, 1}, {1, 1}}, FrameStyle ->RGBColor[0.34, 0.39, 0.55, .5], RoundingRadius -> {5, 10}],
            #1, #3
        ]],
        "EdgeLineStyle" -> Directive[Arrowheads[{{0.03, .5}}], Opacity[.7], Hue[0.63, 0.7, 0.5]],
        VertexLabelStyle -> _ -> FontSize -> 8,
        VertexLabels -> {_HoldForm -> None, _ -> Automatic},
        PlotTheme -> "WolframModel"
    }
|>

makeVertexLabel[vertex_, label_, defaultStyle_, pos_, labelOffset_ : {0, .01}] := With[{style = Replace[defaultStyle, Automatic | None -> Black]},
    Replace[label /. "Name" -> vertex, {
        None -> Nothing,
        Automatic :> Text[Style[vertex, style], pos + labelOffset],
        Placed[placedLabel_, offset_] :> If[offset === Tooltip, Tooltip[Text[" ", pos], Style[placedLabel, style]], Text[Style[placedLabel, style], pos, offset]],
        l_ :> Text[Style[l, style], pos + labelOffset]
    }]
]

ConcavePolygon[points_, n_ : 1] := Block[{polygon = ConvexHullRegion[points], ordering, center},
	center = RegionCentroid[polygon];
    ordering = If[MatchQ[Dimensions[points], {_, 2}], OrderingBy[points, ArcTan @@ (# - center) &], Range[Length[points]]];
	{
        BSplineCurve[With[{from = #1, diff = #2 - #1}, MapAt[Mean[{#, center}] &, from + # diff & /@ Range[0, 1, 1 / (n + 1)], {2 ;; -2}]]] & @@@
            Partition[points[[ordering]], 2, 1, 1],
        ordering
    }
]

Options[SimpleHypergraphPlot] := Join[Options[Hypergraph], Options[Graphics], Options[Graphics3D]];

SimpleHypergraphPlot[h : {___List}, args___] := SimpleHypergraphPlot[Hypergraph[h], args]

SimpleHypergraphPlot[h_Hypergraph, plotOpts : OptionsPattern[]] := Enclose @ Block[{
    graph, plot,
    vertexEmbedding, edgeEmbedding,
    vertices = VertexList[h], edges = EdgeList[h], edgeTags = EdgeTags[h], taggedEdges = EdgeListTagged[h],
    nullEdges, longEdges, ws,
    colorFunction, edgeArrowsQ, edgeType, edgeMethod,
    vertexStyle, vertexLabels, vertexLabelStyle, vertexSize,
    edgeStyle, edgeLineStyle, edgeLabels, edgeLabelStyle, edgeSize, edgeSymmetries,
    vertexCoordinates, vertexLabelOffsets, vertexShapeFunction,
    allPoints, bounds, corner, center, range, size, dim,
    opts = FilterRules[{
        FilterRules[{plotOpts}, Except[Options[Hypergraph]]],
        AbsoluteOptions[Hypergraph[h, FilterRules[{plotOpts}, Options[Hypergraph]]]],
        Options[Hypergraph]
    },
        Options[SimpleHypergraphPlot]
    ],
    edgeIndex,
    makeEdge, renderEdge,
    totalCounts = <||>
},
    edgeIndex = PositionIndex[taggedEdges];
    colorFunction = OptionValue[SimpleHypergraphPlot, opts, ColorFunction];
    {
        vertexStyle, vertexLabels, vertexLabelStyle, vertexSize, vertexCoordinates, vertexShapeFunction,
        edgeStyle, edgeLineStyle, edgeLabels, edgeLabelStyle, edgeSize, edgeSymmetries
    } = Values /@ Lookup[opts, Join[$VertexAnnotations, $EdgeAnnotations]];
    edgeArrowsQ = TrueQ[OptionValue[SimpleHypergraphPlot, opts, "EdgeArrows"]];
    edgeType = OptionValue[SimpleHypergraphPlot, opts, "EdgeType"];
    edgeMethod = OptionValue[SimpleHypergraphPlot, opts, "EdgeMethod"];
    dim = ConfirmMatch[OptionValue[SimpleHypergraphPlot, opts, "LayoutDimension"], 2 | 3];

    nullEdges = \[FormalN] /@ Range[Count[edges, {}]];
    longEdges = Cases[edges, {_, _, __}];
    ws = Join[vertices, nullEdges, \[FormalE] /@ Range[Length[longEdges]]];

    vertexCoordinates = Join[Thread[vertices -> vertexCoordinates], Thread[Drop[ws, Length[vertices]] -> Automatic]];
    vertexCoordinates = Replace[vertexCoordinates, {(_ -> Automatic)...} -> Automatic];

	graph = ConfirmBy[Switch[dim, 2, Graph, 3, Graph3D][
        ws,
        Join[
            Annotation[DirectedEdge[##], EdgeWeight -> 1] & @@@ Cases[edges, {_, _}],
            Catenate[
                MapIndexed[{edge, i} |->
                    With[{
                        (* clickEdges = Catenate[{#, If[SameQ @@ #, Nothing, Reverse[#]]} & /@ #] & @ Subsets[edge, {2}], *)
                        clickEdges = Join[Partition[edge, 2, 1, 1]],
                        weight = Length[edge]
                    },
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
        VertexLabels -> {_ -> Automatic, \[FormalE][_] -> None},
        GraphLayout -> {"SpringEmbedding", "EdgeWeighted" -> True}
    ], GraphQ];
    {vertexEmbedding, edgeEmbedding} = First[#, {}] & /@ Reap[plot = GraphPlot[graph], {"v", "e"}][[2]];
	edgeEmbedding = Join[Merge[edgeEmbedding, Identity], Association[vertexEmbedding][[Key /@ nullEdges]]];
    vertexEmbedding = Association[vertexEmbedding][[Key /@ vertices]];
    If[ dim == 2 && vertexCoordinates === Automatic,
        With[{vertexRearange =
            Catenate[
                Block[{points = Lookup[vertexEmbedding, #], center, ordering},
                    center = Mean[points];
                    ordering = OrderingBy[points, ArcTan @@ (# - center) &];
                    ordering = First @ MaximalBy[Catenate[{RotateLeft[ordering, #], RotateLeft[Reverse[ordering], #]} & /@ Range[Length[ordering]]], Count[MapIndexed[#1 == #2[[1]] &, #], True] &, 1];
                    Thread[Part[#, ordering] -> #]
                ] & /@ Select[edges, DuplicateFreeQ[#] && Length[#] > 3 &]
            ]
        },
            vertexEmbedding = Merge[{KeyMap[Replace[vertexRearange]] @ vertexEmbedding, vertexEmbedding}, First];
            edgeEmbedding = Association @ KeyValueMap[#1 -> If[MatchQ[#2, {__Real}], #2, ReplacePart[#2, Thread[{{_, 1}, {_, -1}} -> Lookup[vertexEmbedding, Extract[#1, {{1}, {2}}]]]]] &] @ edgeEmbedding;
        ]
    ];
    allPoints = DeleteDuplicates @ DeleteMissing @ Join[Values[vertexEmbedding], Catenate[If[MatchQ[#, {__Real}], {#}, Flatten[#, 1]] & /@ Values[edgeEmbedding]]];
    bounds = Lookup[AbsoluteOptions[graph, PlotRange], PlotRange];
    corner = bounds[[All, 1]];
    center = Mean /@ bounds;
    range = #2 - #1 & @@@ bounds;
    size = Max[range];
    If[size == 0, size = 1];
    vertexLabelOffsets = 0.03 Normalize[# - Mean @ Nearest[allPoints, #, 5]] & /@ vertexEmbedding;
    makeEdge[edge_, tag_, symm_, i_, initPrimitive_, lines_ : {}] := Block[{
        primitive = If[RegionQ[initPrimitive], Identity, DiscretizeGraphics @* ReplaceAll[Arrow[l_] :> l]] @ initPrimitive,
        pos,
        edgeTagged, style, lineStyle, label, labelStyle, labelPrimitive
    },
        pos = Replace[RegionCentroid[primitive /. Offset[r_] :> r], {} -> corner];
        If[ Length[edge] == 2 && dim == 2,
            pos += With[{points = Sort[MeshCoordinates[If[RegionQ[primitive], DiscretizeRegion, DiscretizeGraphics] @ primitive]][[{1, -1}]]},
                0.03 size Normalize[If[TrueQ[VectorAngle[#, pos - center] > Pi], #, - #] & [Subtract @@ RotationTransform[Pi / 2, pos][points]]]
            ]
        ];
        edgeTagged = If[tag === None, edge, edge -> tag];
        style = With[{defStyle = Directive[colorFunction[i], EdgeForm[Transparent]]},
            Replace[edgeStyle[[i]], Automatic -> defStyle]
        ];
        lineStyle = With[{defStyle = Directive[colorFunction[i], EdgeForm[Transparent]]},
            Replace[edgeLineStyle[[i]], Automatic -> defStyle]
        ];
        label = edgeLabels[[i]]; 
        labelStyle = Replace[edgeLabelStyle[[i]], Automatic | None -> Black];
        labelPrimitive = Replace[label /. {"Name" -> edge, "EdgeTag" -> tag, "EdgeSymmetry" -> symm}, {
            None -> {},
            Automatic :> Text[edge, pos],
            Placed[placedLabel_, offset_] :> Text[Replace[placedLabel, None -> ""], pos, offset],
            label_ :> Text[label, pos]
        }];
        primitive = If[ edgeArrowsQ || MatchQ[symm, "Ordered" | "Directed" | {}],
            {   initPrimitive,
                MapIndexed[{Arrowheads[{{Switch[dim, 3, 0.015, _, 0.02] (Log[#2[[1]]] + 1), .5}}], lineStyle, Arrow[#1]} &, lines]
            },
            initPrimitive
        ] /. _EmptyRegion -> {};
        Sow[pos, "EdgeLabelPosition"];
        {
            If[MatchQ[pos, {_, _, _}], EdgeForm[], Nothing],
            style,
            If[ MatchQ[label, Placed[_, Tooltip]],
                Tooltip[primitive, Replace[labelPrimitive, Text[expr_, ___] :> Style[expr, labelStyle]]],
                {primitive, Replace[labelPrimitive, Text[expr_, args___] :> Text[Style[expr, labelStyle], args]]}
            ]
        }
    ];

    renderEdge[{edge_List, tag_} -> {mult_Integer, total_Integer : 0}, i_Integer, j_Integer] := Block[{
        edgeTagged, emb = Replace[edge, vertexEmbedding, {1}],
        primitive, addArrows
    },
        edgeTagged = If[tag === None, edge, edge -> tag];
        Switch[
            Length[edge],
            0 | 1, Block[{s, r, dr = size 0.01, symm},
                symm  = edgeSymmetries[[i]];
                s  = Replace[edgeSize[[i]], Automatic -> 0.03];
                If[ TrueQ[Positive[s]],
                    r = size s,
                    r = size 0.03 + (j - 1) * dr
                ];
                Sow[primitive = Switch[Length[edge],
                    0,
                        Switch[dim, 2, Circle, 3, Sphere][Sow[Lookup[edgeEmbedding, \[FormalN][j]], "NullEdge"], Offset[400 r]],
                    1,
                        Switch[dim, 2, Disk[First[emb], Offset[400 r]], 3, Sphere[First[emb], r], _, Nothing]
                ], "Primitive"];
                makeEdge[edge, tag, symm, i, primitive]
            ],
            2, Block[{points = Lookup[edgeEmbedding, DirectedEdge @@ edge], curve, symm},
                curve = With[{c = Lookup[totalCounts, Key[#], 0] + 1},
                    AppendTo[totalCounts, # -> c];
                    GraphComputation`GraphElementData["Line"][points[[c]], None][[1]] /. BezierCurve -> BSplineCurve
                ] & @ edge;
                symm = edgeSymmetries[[i]];
                Sow[primitive = If[edgeArrowsQ || MatchQ[symm, "Ordered" | "Directed" | {}], Arrow, Identity] @
                    If[ edgeMethod === "ConcavePolygon" && DuplicateFreeQ[edge] && total == 1,
                        MapAt[#[[{1, -1}]] &, curve, {1}],
                        curve
                    ],
                    "Primitive"
                ];
                {   Opacity[1],
                    Arrowheads[{{Small, .5}}],
                    makeEdge[edge, tag, symm, i, primitive]
                }
            ],
            _, Block[{counts = <||>, points, coords = Lookup[vertexEmbedding, edge], curves, ordering, lines, symm},
                If[
                    edgeMethod === "ConcavePolygon" && DuplicateFreeQ[edge],
                    With[{c = Lookup[totalCounts, Key[#], 0] + 1},
                        AppendTo[totalCounts, # -> c];
                        {curves, ordering} = ConcavePolygon[coords, c];
                        lines = Prepend[curves[[1]]][Insert[#[[2]], #[[1, 1, -1]], {1, 1}] & /@ Partition[curves, 2, 1]];
                        With[{part = Partition[ordering, 2, 1, 1]},
                            lines = Most @ Map[
                                With[{pos = FirstPosition[part, # | Reverse[#], {1}, Heads -> False]},
                                    If[OrderedQ[Extract[part, pos]], Identity, MapAt[Reverse, 1]] @ Extract[lines, pos]
                                ] &,
                                Partition[Range[Length[ordering]], 2, 1, 1]
                            ];
                        ]
                    ] & @ Sort[edge],
                    points = With[{c = Lookup[counts, #, 0] + 1},
                        AppendTo[counts, # -> c];
                        Lookup[edgeEmbedding, #][[c]]
                    ] & /@ (DirectedEdge[##, edge] & @@@ Partition[edge, 2, 1, If[edgeType === "Cyclic", 1, None]]);
                    curves = Catenate[GraphComputation`GraphElementData["Line"][#, None] /. BezierCurve -> BSplineCurve & /@ points];
                    lines = Insert[#[[2]], #[[1, 1, -1]], {1, 1}] & /@ Partition[curves, 2, 1, 1];
                ];
                symm = edgeSymmetries[[i]];
                addArrows = If[ edgeArrowsQ || MatchQ[symm, "Ordered" | "Directed" | {}],
                    {#, Arrow /@ lines} &,
                    Identity
                ];
                Switch[dim,
                    2, Sow[addArrows[primitive = FilledCurve[curves]], "Primitive"],
                    3, Block[{pts, region},
                        pts = MeshCoordinates @ DiscretizeGraphics @ curves;
                        region = DiscretizeRegion[#, MaxCellMeasure -> {"Area" -> Area[#] / (Length[pts] + 1)}] & @
                            Polygon[Prepend[First[pts]] /@ Partition[pts[[2 ;; -2]], 2, 1]];
                        Sow[addArrows[primitive = Quiet @ areaGradientDescent[region, .1, 20]], "Primitive"]
                    ]
                ];
                makeEdge[edge, tag, symm, i, primitive, lines]
            ]
        ]
    ];

	Switch[dim, 2, Graphics, 3, Graphics3D][{
		Opacity[.5],
		Arrowheads[{{Medium, .5}}],
		AbsoluteThickness[Medium],
        Block[{
            edgesWithTags,
            counts,
            counter = <||>
        },
            edgesWithTags = Thread[{edges, edgeTags}];
            counts = Merge[{Counts[edgesWithTags], First[#] -> Length[#] & /@ GatherBy[edgesWithTags, First /* Sort]}, Identity];
            MapIndexed[{edgeWithTag, i} |-> With[{j = Lookup[counter, Key[edgeWithTag], 0] + 1},
                    counter[edgeWithTag] = j;
                    renderEdge[edgeWithTag -> counts[edgeWithTag], i[[1]], j]
                ],
                edgesWithTags
            ]
        ],
        Opacity[1],
        MapThread[{vertex, style, vsf, sz, coord} |->
            {style, vsf[coord, vertex, Replace[sz, x_ ? NumericQ :> {x, x}]]},
            {Keys[vertexEmbedding], vertexStyle, vertexShapeFunction, vertexSize, Values[vertexEmbedding]}
        ],
        MapThread[{vertex, coord, label, style, offset} |-> (
            Sow[coord, "Vertex"];
            Sow[vertex -> offset, "VertexLabelOffset"];
            makeVertexLabel[vertex, label, style, coord, offset]
        ), {Keys[vertexEmbedding], Values[vertexEmbedding], vertexLabels, vertexLabelStyle, Lookup[vertexLabelOffsets, Keys[vertexEmbedding]]}]
	},
        FilterRules[{opts}, Options[Switch[dim, 2, Graphics, 3, Graphics3D]]],
        ImageSize -> Medium,
		Boxed -> False
	]
]


SimpleHypergraphPlot3D[h_, opts___] := SimpleHypergraphPlot[h, "LayoutDimension" -> 3, opts]


HypergraphEmbedding[hg_Hypergraph ? HypergraphQ] := First[Reap[SimpleHypergraphPlot[hg], "Vertex"][[2]], {}]

