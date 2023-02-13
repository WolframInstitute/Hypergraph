Package["WolframInstitute`Hypergraph`"]

PackageExport["SimpleHypergraphPlot"]



Options[SimpleHypergraphPlot] = Join[{
    ColorFunction -> ColorData[97],
    "EdgeArrows" -> True,
    VertexLabels -> None
}, Options[Graph], Options[Graphics], Options[Graphics3D]];

SimpleHypergraphPlot[h : {___List}, args___] := SimpleHypergraphPlot[Hypergraph[h], args]

SimpleHypergraphPlot[h_Hypergraph, dim : 2 | 3 : 2, opts : OptionsPattern[]] := Block[{
    graph, embedding,
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
            Annotation[UndirectedEdge[##], EdgeWeight -> 1] & @@@ Cases[es, {_, _}],
            Catenate[
                MapIndexed[{edge, i} |->
                    With[{clickEdges = Subsets[edge, {2}], weight = Length[edge] - 1},
                        Join[
                            Annotation[UndirectedEdge @@ #, EdgeWeight -> 1] & /@ clickEdges,
                            Annotation[UndirectedEdge[#, \[FormalE] @@ i], EdgeWeight -> weight] & /@ edge
                        ]
                    ],
                    longEdges
                ]
            ]
        ],
        FilterRules[{opts}, Options[Graph]],
        GraphLayout -> {"SpringEmbedding", "EdgeWeighted" -> True}
    ];
	embedding = AssociationThread[vs -> Drop[GraphEmbedding[graph], -Length[longEdges]]];
    size = Max[#2 - #1 & @@@ CoordinateBounds[Values[embedding]]];
	Switch[dim, 2, Graphics, 3, Graphics3D][{
		Opacity[.5],
		Arrowheads[Medium],
		AbsoluteThickness[Medium],
		MapIndexed[With[{emb = Replace[#1[[1]], embedding, {1}], mult = #1[[2]], i = #2[[1]]}, {
                colorFunction[i],
                Switch[Length[emb],
                    1, Block[{r = size 0.03, dr = size 0.01}, Table[Switch[dim, 2, Circle[First[emb], r += dr], 3, Sphere[First[emb], r += dr], _, Nothing], mult]],
                    2, If[TrueQ[OptionValue["EdgeArrows"]], Arrow, Line][emb],
                    _, Polygon[With[{center = Mean[emb]}, SortBy[emb, ArcTan @@ (# - center)[[;; 2]] &]]]
                ]
            }] &,
            Tally[es]
        ],
        Black,
        Opacity[1],
		Point[Values[embedding]],
        Switch[vertexLabels,
            Automatic, KeyValueMap[Text[##, {2, 2}] &, embedding],
            Placed[Automatic, _Offset], KeyValueMap[Text[##, vertexLabels[[2, 1]]] &, embedding],
            _, Nothing
        ]
	},
        FilterRules[{opts}, Options[Switch[dim, 2, Graphics, 3, Graphics3D]]],
        ImageSize -> 400,
		Boxed -> False
	]
]

