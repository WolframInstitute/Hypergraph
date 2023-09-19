Package["WolframInstitute`Hypergraph`"]

PackageExport["SimpleHypergraphPlot"]
PackageExport["SimpleHypergraphPlot3D"]
PackageExport["HypergraphEmbedding"]

PackageScope["makeVertexLabel"]
PackageScope["makeAnnotationRules"]



makeVertexLabel[vertex_, label_, style_, pos_] := Replace[label /. "Name" -> vertex, {
    None -> Nothing,
    Automatic :> Text[Style[vertex, style], pos, {1, 1}],
    Placed[placedLabel_, offset_] :> If[offset === Tooltip, Tooltip[Text[" ", pos], Style[placedLabel, style]], Text[Style[placedLabel, style], pos, offset]],
    l_ :> Text[Style[l, style], pos, {1, 1}]
}]


makeAnnotationRules[opts_List, keys_ : All] := If[MatchQ[keys, _List | All], Association, #[[1, 2]] &] @ KeyValueMap[
    #1 -> Block[{automatic, default},
        If[ MatchQ[#2, {_, _}],
            {automatic, default} = #2,
            automatic = default = #2
        ];
        DeleteDuplicatesBy[Replace[{(Verbatim[_] -> _) :> _, _ :> Unique[]}]] @
            Append[Replace[Flatten[ReplaceList[#1, opts]], {Automatic -> _ -> automatic, s : Except[_Rule | _RuleDelayed] :> _ -> s}, {1}], _ -> default]
    ] &,
    If[keys === All, $DefaultHypergraphAnnotations, $DefaultHypergraphAnnotations[[ Key /@ Developer`ToList[keys] ]]]
]

ConcavePolygon[points_, n_ : 1] := Block[{polygon = ConvexHullRegion[points], center},
	center = RegionCentroid[polygon];
	BSplineCurve[With[{from = #1, diff = #2 - #1}, MapAt[Mean[{#, center}] &, from + # diff & /@ Range[0, 1, 1 / (n + 1)], {2 ;; -2}]]] & @@@ Partition[
        If[MatchQ[Dimensions[points], {_, 2}], SortBy[points, ArcTan @@ (# - center) &], points], 2, 1, 1]
]

applyIndexedRules[edge_, rules_, index_Integer, default_ : None] := Enclose[
    GroupBy[rules, First, If[Length[#] >= index, Return[#[[index]], CompoundExpression], If[Length[#] > 0, Return[Last[#], CompoundExpression]]] & @ ReplaceList[edge, #, index] &];
    default
]

Options[SimpleHypergraphPlot] := Join[Options[Hypergraph], Options[Graphics], Options[Graphics3D]];

SimpleHypergraphPlot[h : {___List}, args___] := SimpleHypergraphPlot[Hypergraph[h], args]

SimpleHypergraphPlot[h_Hypergraph, plotOpts : OptionsPattern[]] := Enclose @ Block[{
    graph,
    vertexEmbedding, edgeEmbedding,
    vs = VertexList[h], es = EdgeList[h], edgeTags = EdgeTags[h], taggedEdges = EdgeListTagged[h],
    nullEdges, longEdges, ws,
    colorFunction, edgeArrowsQ, edgeType, edgeMethod,
    vertexStyle, vertexLabels, vertexLabelStyle, vertexSize,
    edgeStyle, edgeLabels, edgeLabelStyle, edgeSize, edgeSymmetries,
    vertexCoordinates,
    bounds, corner, size, dim,
    opts = FilterRules[{plotOpts, h["Options"]}, Options[SimpleHypergraphPlot]],
    edgeIndex,
    makeEdge, renderEdge,
    totalCounts = <||>
},
    edgeIndex = PositionIndex[taggedEdges];
    colorFunction = OptionValue[SimpleHypergraphPlot, opts, ColorFunction];
    {
        vertexStyle, vertexLabels, vertexLabelStyle, vertexSize, vertexCoordinates,
        edgeStyle, edgeLabels, edgeLabelStyle, edgeSize, edgeSymmetries
    } = Values @ makeAnnotationRules[opts];
    edgeArrowsQ = TrueQ[OptionValue[SimpleHypergraphPlot, opts, "EdgeArrows"]];
    edgeType = OptionValue[SimpleHypergraphPlot, opts, "EdgeType"];
    edgeMethod = OptionValue[SimpleHypergraphPlot, opts, "EdgeMethod"];
    dim = ConfirmMatch[OptionValue[SimpleHypergraphPlot, opts, "LayoutDimension"], 2 | 3];

    nullEdges = \[FormalN] /@ Range[Count[es, {}]];
    longEdges = Cases[es, {_, _, __}];
    ws = Join[vs, nullEdges, \[FormalE] /@ Range[Length[longEdges]]];

    vertexCoordinates = Select[Replace[vertexCoordinates, Except[{___Rule}] -> {}], MemberQ[ws, Verbatim[#[[1]]]] &];
    vertexCoordinates = Join[vertexCoordinates, Thread[Complement[ws, vertexCoordinates[[All, 1]]] -> Automatic]];
    vertexCoordinates = MapAt[Replace[coords_List :> PadRight[coords, dim]], vertexCoordinates, {All, 2}];
    vertexCoordinates = Replace[vertexCoordinates, {(_ -> Automatic)...} -> Automatic];

	graph = ConfirmBy[Switch[dim, 2, Graph, 3, Graph3D][
        ws,
        Join[
            Annotation[DirectedEdge[##], EdgeWeight -> 1] & @@@ Cases[es, {_, _}],
            Catenate[
                MapIndexed[{edge, i} |->
                    With[{clickEdges = Catenate[{#, If[SameQ @@ #, Nothing, Reverse[#]]} & /@ #] & @ Subsets[edge, {2}], weight = Length[edge]},
                        Join[
                            Annotation[DirectedEdge[##, edge], EdgeWeight -> 1] & @@@ clickEdges,
                            If[ edgeMethod =!= "ConcavePolygon" && DuplicateFreeQ @ edge,
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
    ], GraphQ];
    {vertexEmbedding, edgeEmbedding} = First[#, {}] & /@ Reap[GraphPlot[graph], {"v", "e"}][[2]];
	edgeEmbedding = Join[Merge[edgeEmbedding, Identity], Association[vertexEmbedding][[Key /@ nullEdges]]];
    vertexEmbedding = Association[vertexEmbedding][[Key /@ vs]];
    bounds = CoordinateBounds[Values[vertexEmbedding]];
    corner = bounds[[All, 1]];
    size = Max[#2 - #1 & @@@ bounds];
    If[size == 0, size = 1];

    makeEdge[edge_, tag_, symm_, i_, j_, initPrimitive_] := Block[{
        primitive,
        pos = Replace[RegionCentroid[If[RegionQ[initPrimitive], Identity, DiscretizeGraphics @* ReplaceAll[Arrow[l_] :> l]] @ initPrimitive], {} -> corner],
        edgeTagged, style, label, labelStyle, labelPrimitive
    },
        edgeTagged = If[tag === None, edge, edge -> tag];
        style = With[{defStyle = Directive[colorFunction[i], EdgeForm[Transparent]]},
            Replace[applyIndexedRules[edgeTagged, edgeStyle, j, defStyle], Automatic -> defStyle]
        ];
        label = applyIndexedRules[edgeTagged, edgeLabels, j, None];
        labelStyle = applyIndexedRules[edgeTagged, edgeLabelStyle, j, {}];
        labelPrimitive = Replace[label /. {"Name" -> edge, "EdgeTag" -> tag, "EdgeSymmetry" -> symm}, {
            None -> {},
            Automatic :> Text[edge, pos],
            Placed[placedLabel_, offset_] :> Text[Replace[placedLabel, None -> ""], pos, offset],
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

    renderEdge[{edge_List, tag_} -> {mult_Integer, total_Integer : 0}, {i_Integer}] := Block[{
        edgeTagged, emb = Replace[edge, vertexEmbedding, {1}],
        position, primitive, arrowFunction
    },
        edgeTagged = If[tag === None, edge, edge -> tag];
        Table[
            Sow[position = edgeIndex[edgeTagged][[j]], "Position"];
            Switch[
                Length[edge],
                0 | 1, Block[{s, r, dr = size 0.01, symm},
                    symm  = applyIndexedRules[edgeTagged, edgeSymmetries, j, {}];
                    s  = applyIndexedRules[edgeTagged, edgeSize, j, 0.03];
                    If[ TrueQ[Positive[s]],
                        r = size s,
                        r = size 0.03 + (j - 1) * dr
                    ];
                    Sow[primitive = Switch[Length[edge],
                        0,
                            Switch[dim, 2, Circle, 3, Sphere][Sow[Lookup[edgeEmbedding, \[FormalN][j]], "NullEdge"], r],
                        1,
                            Switch[dim, 2, Disk[First[emb], r], 3, Sphere[First[emb], r], _, Nothing]
                    ], "Primitive"];
                    makeEdge[edge, tag, symm, i, j, primitive]
                ],
                2, Block[{points = Lookup[edgeEmbedding, DirectedEdge @@ edge], curve, symm},
                    curve = With[{c = Lookup[totalCounts, Key[#], 0] + 1},
                        AppendTo[totalCounts, # -> c];
                        GraphComputation`GraphElementData["Line"][points[[c]], None][[1]] /. BezierCurve -> BSplineCurve
                    ] & @ Sort[edge];
                    symm = applyIndexedRules[edgeTagged, edgeSymmetries, j, {}];
                    Sow[primitive = If[edgeArrowsQ || MatchQ[symm, "Ordered" | "Directed" | {}], Arrow, Identity] @
                        If[ edgeMethod === "ConcavePolygon" && DuplicateFreeQ[edge] && total == 1,
                            MapAt[#[[{1, -1}]] &, curve, {1}],
                            curve
                        ],
                        "Primitive"
                    ];
                    {   Opacity[1],
                        Arrowheads[{{Small, .5}}],
                        makeEdge[edge, tag, symm, i, j, primitive]
                    }
                ],
                _, Block[{counts = <||>, points, coords = Lookup[vertexEmbedding, edge], curves, symm},
                    curves = If[
                        edgeMethod === "ConcavePolygon" && DuplicateFreeQ[edge],
                        With[{c = Lookup[totalCounts, Key[#], 0] + 1},
                            AppendTo[totalCounts, # -> c];
                            ConcavePolygon[coords, c]
                        ] & @ Sort[edge],
                        points = With[{c = Lookup[counts, #, 0] + 1},
                            AppendTo[counts, # -> c];
                            Lookup[edgeEmbedding, #][[c]]
                        ] & /@ (DirectedEdge[##, edge] & @@@ Partition[edge, 2, 1, If[edgeType === "Cyclic", 1, None]]);
                        Catenate[GraphComputation`GraphElementData["Line"][#, None] /. BezierCurve -> BSplineCurve & /@ points]
                    ];
                    symm = applyIndexedRules[edgeTagged, edgeSymmetries, j, {}];
                    arrowFunction = If[ edgeArrowsQ || MatchQ[symm, "Ordered" | "Directed" | {}],
                        With[{lengths = RegionMeasure @* DiscretizeGraphics /@ Most[curves]},
                            {   #,
                                Opacity[1],
                                Switch[
                                    dim,
                                    2, {
                                        Arrowheads[MapIndexed[{0.02 (Log[#2[[1]]] + 1), #1} &, (Prepend[Accumulate[Most[lengths]], 0] + lengths / 2) / Total[lengths]]],
                                        Arrow @ JoinedCurve[Most[curves]]
                                    },
                                    3, MapIndexed[{Arrowheads[{{0.015 (Log[#2[[1]]] + 1), .5}}], Arrow[#1]} &, Most[curves]]
                                ]
                            }
                        ] &,
                        Identity
                    ];
                    Switch[dim,
                        2, Sow[primitive = arrowFunction @ FilledCurve[curves], "Primitive"],
                        3, Block[{pts, region},
                            pts = MeshCoordinates @ DiscretizeGraphics @ curves;
                            region = DiscretizeRegion[#, MaxCellMeasure -> {"Area" -> Area[#] / (Length[pts] + 1)}] & @
                                Polygon[Prepend[First[pts]] /@ Partition[pts[[2 ;; -2]], 2, 1]];
                            Sow[primitive = arrowFunction @ Quiet @ areaGradientDescent[region, .1, 20], "Primitive"]
                        ]
                    ];
                    makeEdge[edge, tag, symm, i, j, primitive]
                ]
            ]
            ,
            {j, mult}
        ]
    ];

	Switch[dim, 2, Graphics, 3, Graphics3D][{
		Opacity[.5],
		Arrowheads[{{Medium, .5}}],
		AbsoluteThickness[Medium],
		MapIndexed[renderEdge, With[{counts = Counts[Thread[{es, edgeTags}]]},
            Normal @ Merge[{counts, First[#] -> Length[#] & /@ GatherBy[Thread[{es, edgeTags}], First /* Sort]}, Identity]]
        ],
        Opacity[1],
		KeyValueMap[{Replace[#1, vertexStyle], {AbsolutePointSize[Replace[Replace[#1, vertexSize], Automatic -> 3]], Point[Sow[#2, "Vertex"]]}} &, vertexEmbedding],
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


HypergraphEmbedding[hg_Hypergraph ? HypergraphQ] := First[Reap[SimpleHypergraphPlot[hg], "Vertex"][[2]], {}]

