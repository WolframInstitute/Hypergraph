Package["WolframInstitute`Hypergraph`"]

PackageExport["SimpleHypergraphPlot"]



Options[SimpleHypergraphPlot] = Join[{
    ColorFunction -> ColorData[97],
    "EdgeArrows" -> False,
    "EdgeType" -> "Cyclic",
    VertexLabels -> None
}, Options[Graph], Options[Graphics], Options[Graphics3D]];

SimpleHypergraphPlot[h : {___List}, args___] := SimpleHypergraphPlot[Hypergraph[h], args]

SimpleHypergraphPlot[h_Hypergraph, dim : 2 | 3 : 2, opts : OptionsPattern[]] := Block[{
    graph,
    vertexEmbedding, edgeEmbedding,
    colorFunction = OptionValue[ColorFunction],
    vs = h["VertexList"], es = h["EdgeList"],
    longEdges, ws,
    vertexLabels = OptionValue[VertexLabels],
    size
},
    longEdges = Cases[es, {_, _, __}];
    ws = Join[vs, \[FormalE] /@ Range[Length[longEdges]]];
	graph = Switch[dim, 2, Graph, 3, Graph3D][
        ws,
        Join[
            Annotation[DirectedEdge[##], EdgeWeight -> 1] & @@@ Cases[es, {_, _}],
            Catenate[
                MapIndexed[{edge, i} |->
                    With[{clickEdges = Tuples[edge, 2], weight = Length[edge]},
                        Join[
                            Annotation[DirectedEdge[##, edge], EdgeWeight -> 1] & @@@ clickEdges,
                            Annotation[DirectedEdge[#, \[FormalE] @@ i], EdgeWeight -> weight] & /@ edge
                        ]
                    ],
                    longEdges
                ]
            ]
        ],
        FilterRules[{opts}, Options[Graph]],
        VertexShapeFunction -> (Sow[#2 -> #1, "v"] &),
        EdgeShapeFunction -> (Sow[#2 -> #1, "e"] &),
        GraphLayout -> {"SpringEmbedding", "EdgeWeighted" -> True}
    ];
    {vertexEmbedding, edgeEmbedding} = First[#, {}] & /@ Reap[GraphPlot[graph], {"v", "e"}][[2]];
	vertexEmbedding = Association[vertexEmbedding][[Key /@ vs]];
    edgeEmbedding = Merge[edgeEmbedding, Identity];
    size = Max[1, #2 - #1 & @@@ CoordinateBounds[Values[vertexEmbedding]]];
	Switch[dim, 2, Graphics, 3, Graphics3D][{
		Opacity[.5],
		Arrowheads[{{Medium, .5}}],
		AbsoluteThickness[Medium],
		MapIndexed[With[{edge = #1[[1]], emb = Replace[#1[[1]], vertexEmbedding, {1}], mult = #1[[2]], i = #2[[1]]}, {
                colorFunction[i],
                Switch[Length[emb],
                    1, Block[{r = size 0.03, dr = size 0.01}, Table[Switch[dim, 2, Circle[First[emb], r += dr], 3, Sphere[First[emb], r += dr], _, Nothing], mult]],
                    2, If[TrueQ[OptionValue["EdgeArrows"]], Arrow, Line] /@ Lookup[edgeEmbedding, DirectedEdge @@ #1[[1]]],
                    _, {
                        Table[
                            {
                                Switch[dim,
                                    2, FilledCurve[Line /@ #[[All, j]]],
                                    3, Polygon @ Catenate @ #[[All, j]]
                                ],
                                If[TrueQ[OptionValue["EdgeArrows"]], Arrow, Line] /@ #[[All, j]]
                            } & @ Lookup[edgeEmbedding, DirectedEdge[##, edge] & @@@
                                Partition[#1[[1]], 2, 1, If[OptionValue["EdgeType"] === "Cyclic", 1, None]]
                            ],
                            {j, mult}
                        ]
                    }
                ]
            }] &,
            Tally[es]
        ],
        Black,
        Opacity[1],
		Point[Values[vertexEmbedding]],
        Switch[vertexLabels,
            Automatic, KeyValueMap[Text[##, {2, 2}] &, vertexEmbedding],
            Placed[Automatic, _Offset], KeyValueMap[Text[##, vertexLabels[[2, 1]]] &, vertexEmbedding],
            _, Nothing
        ]
	},
        FilterRules[{opts}, Options[Switch[dim, 2, Graphics, 3, Graphics3D]]],
        ImageSize -> 400,
		Boxed -> False
	]
]

