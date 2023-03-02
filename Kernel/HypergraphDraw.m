Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphDraw"]



HypergraphDraw[initHg : _Hypergraph ? HypergraphQ : Hypergraph[]] := DynamicModule[{
	hg,
	vertices, edges,
    vertexStyles, edgeStyles,
	edgeStart = False, edgeUp = False, edgeNext = False, edgeFinish = False, vertexMove = False, edgeMove = False,
    multiSelect = False,
    recolor = False,
	edge = {}, tmpEdge = {}, vertexId, edgeId = Missing[], oldVertices = <||>,
	getVertex, down, move, up, undo, reset, update, addEdge,
    mousePosition,
	color,
	actions = {},
    mousePos,
    grid
},
	getVertex[] := If[Length[vertices] > 0,
		First[
			Nearest[Reverse /@ Normal[vertices], MousePosition["Graphics"], {1, .02}],
			Missing[]
		],
		Missing[]
	];
    mousePosition[] := Replace[MousePosition["Graphics"], {None -> mousePos, pos_ :> (mousePos = pos)}];
	down[i_] := (
		Which[

            i == 1 && edgeUp,
			edge = tmpEdge;
			edgeFinish = True,

            i == 1 && CurrentValue["ShiftKey"],
            edgeId = First[Nearest[Mean[Lookup[vertices, #]] & /@ edges -> "Index", mousePosition[], {1, .1}], Missing[]];
            If[ ! MissingQ[edgeId],
                edgeMove = True;
                oldVertices = vertices;
                If[ recolor,
                    AppendTo[actions, "EdgeRecolor"[edgeId, edgeStyles[[edgeId]]]];
                    edgeStyles[[edgeId]] = color
                ];
                update[]
            ],

            i == 1 && CurrentValue["AltKey"],
            vertexId = First[Nearest[Values[vertices] -> Keys[vertices], mousePosition[], {1, .02}], Missing[]];
            If[ ! MissingQ[vertexId],
                vertexMove = True;
                oldVertices = vertices;
                If[ recolor,
                    AppendTo[actions, "VertexRecolor"[vertexId, vertexStyles[vertexId]]];
                    vertexStyles[vertexId] = color
                ];
                update[]
            ],

            i == 2 && CurrentValue["ShiftKey"],
            edgeId = First[Nearest[Mean[Lookup[vertices, #]] & /@ edges -> "Index", mousePosition[], {1, .1}], Missing[]];
            If[ ! MissingQ[edgeId],
                AppendTo[actions, "EdgeDelete"[edges[[edgeId]], edgeStyles[[edgeId]]]];
                edges = Delete[edges, edgeId];
                edgeStyles = Delete[edgeStyles, edgeId];
                update[]
            ],

            i == 2 && CurrentValue["AltKey"],
            vertexId = First[Nearest[Values[vertices] -> Keys[vertices], mousePosition[], {1, .02}], Missing[]];
            If[ ! MissingQ[vertexId],
                AppendTo[actions, "VertexDelete"[vertexId -> vertices[vertexId], vertexStyles[vertexId], edges]];
                vertices = Delete[vertices, Key[vertexId]];
                vertexStyles = Delete[vertexStyles, Key[vertexId]];
                edges = Map[DeleteCases[#, vertexId] &, edges];
                update[]
            ],

            i == 1 && ! multiSelect,
			edgeStart = True;
            edgeNext = True;
			edge = {getVertex[] -> mousePosition[]}
		];
	);
	move[] := (
		If[ edgeStart, edgeUp = True; edgeStart = False];
        If[ Length[tmpEdge] == 0 || EuclideanDistance[tmpEdge[[-1, 2]], mousePosition[]] > .02,
            edgeNext = True
        ];
		If[ edgeUp && edgeNext,
			tmpEdge = Append[edge, getVertex[] -> mousePosition[]];
            edgeNext = False;
			edgeFinish = False;
		];
        If[ vertexMove,
            With[{diff = mousePosition[] - Lookup[oldVertices, vertexId]},
                vertices = MapAt[# + diff &, oldVertices, Key[vertexId]];
                update[]
            ]
        ];
        If[ edgeMove,
            With[{diff = mousePosition[] - Mean[Lookup[oldVertices, edges[[edgeId]]]]},
                vertices = MapAt[# + diff &, oldVertices, {Key[#]} & /@ edges[[edgeId]]];
                update[]
            ]
        ];
	);
	undo[] := (
		Replace[Last[actions, None], {
			"EdgeAdd" :>
			    If[Length[edges] > 0, edges = Most[edges]; edgeStyles = Most[edgeStyles]; actions = Most[actions]],
			"VertexAdd" :>
			    If[Length[vertices] > 0, vertices = Most[vertices]; vertexStyles = Most[vertexStyles]; actions = Most[actions]],
            "VertexRecolor"[vertexId_, oldStyle_] :> (vertexStyles[vertexId] = oldStyle; actions = Most[actions]),
            "EdgeRecolor"[edgeId_, oldStyle_] :> (edgeStyles[[edgeId]] = oldStyle; actions = Most[actions]),
            "VertexDelete"[vertex_Rule, style_, oldEdges_] :> (
                AppendTo[vertices, vertex];
                AppendTo[vertexStyles, vertex[[1]] -> style];
                edges = oldEdges;
                actions = Most[actions]
            ),
            "EdgeDelete"[edge_, style_] :> (AppendTo[edges, edge]; AppendTo[edgeStyles, style]; actions = Most[actions])
        }];
		update[]
	);
	up[] := (
        If[ multiSelect && ! (vertexMove || edgeMove),
            tmpEdge = Append[tmpEdge, getVertex[] -> mousePosition[]];
        ];
        If[ vertexMove,
            vertexMove = False;
            vertexId = Missing[];
        ];
        If[ edgeMove,
            edgeMove = False;
            edgeId = Missing[];
        ];
		If[ edgeStart,
			If[ MissingQ[getVertex[]],
				With[{v = Max[0, Select[Keys[vertices], IntegerQ]] + 1},
                    AppendTo[vertices, v -> mousePosition[]];
                    AppendTo[vertexStyles, v -> color]
                ];
				AppendTo[actions, "VertexAdd"],

				AppendTo[edges, {getVertex[]}];
				AppendTo[edgeStyles, color];
				AppendTo[actions, "EdgeAdd"]
			]
		];
		If[ edgeFinish,
			addEdge[];
			edgeFinish = False;
			edgeUp = False
		];

		edge = tmpEdge;
		edgeStart = False;

		update[]
	);
    addEdge[] := (
        AppendTo[
            edges,
            Map[
                If[ MissingQ[#[[1]]],
                    With[{v = Max[0, Select[Keys[vertices], IntegerQ]] + 1},
                        AppendTo[vertices, v -> #[[2]]];
                        AppendTo[vertexStyles, v -> color];
                        AppendTo[actions, "VertexAdd"];
                        v
                    ],
                    #[[1]]
                ] &,
                tmpEdge
            ]
        ];
        AppendTo[edgeStyles, color];
		AppendTo[actions, "EdgeAdd"];
        tmpEdge = {};
    );
	update[] := (
		hg = Hypergraph[
			Keys[vertices], edges,
            FilterRules[initHg["Options"], Except[VertexCoordinates | VertexStyle | EdgeStyle]],
			VertexCoordinates -> Normal[vertices],
			VertexLabels -> Automatic,
			VertexStyle -> Normal[vertexStyles],
			EdgeStyle -> Thread[edges -> edgeStyles]
		]
	);
    reset[] := (
	    vertices = Association @ Lookup[initHg["Options"], VertexCoordinates, <||>];
        vertexStyles = Replace[Lookup[initHg["Options"], VertexStyle, Automatic], {
            rules : {___Rule} :> Association[rules],
            Automatic :> AssociationThread[vertices, Array[ColorData[97], Length[vertices]]],
            style_ :> AssociationThread[Range[Length[vertices]], style]
        }];
        edges = EdgeList[initHg];
        edgeStyles = Replace[Lookup[initHg["Options"], EdgeStyle, Automatic], {
            rules : {___Rule} :> rules[[All, 2]],
            Automatic :> Array[ColorData[97], Length[edges]],
            style_ :> ConstantArray[style, Length[edges]]
        }];
        color = FirstCase[vertexStyles, _ ? ColorQ, Black, All];
        actions = {};

        If[ Not[VertexCount[initHg] == Length[vertices] == Length[vertexStyles] && EdgeCount[initHg] == Length[edges] == Length[edgeStyles]],
            vertices = AssociationThread[
                VertexList[initHg],
                If[# === {}, {}, RescalingTransform[If[#2 - #1 == 0, {#1, #1 + 1}, {##}] & @@@ CoordinateBounds[#], {{.1, .9}, {.1, .9}}][#]] & @ HypergraphEmbedding[initHg]
            ];
            vertexStyles = AssociationThread[Range[Length[vertices]], color];
            edgeStyles = PadRight[edgeStyles, Length[edges], color];
        ];

        update[];
    );

    reset[];

	grid = Grid[{{
		EventHandler[
			Dynamic @ Show[{
				SimpleHypergraphPlot[hg],
				Graphics[{
					If[ edgeUp, {color, Line[Append[Values[tmpEdge], MousePosition["Graphics"]]]}, Nothing],
                    If[ edgeUp || multiSelect && Length[tmpEdge] > 0,
                        {   color, Dotted,
                            KeyValueMap[
                                With[{p = Lookup[vertices, #1]}, Table[Circle[p, 0.02 r], {r, #2}]] &,
                                Counts[Select[Keys[tmpEdge], Not @* MissingQ]]
                            ]
                        },
                        Nothing
                    ],
                    If[ multiSelect, {color, Dashed, Circle[#, 0.02] & /@ Values[Select[tmpEdge, MissingQ[#[[1]]] &]]}, Nothing]
				}]
            },
				PlotRange -> {{0, 1}, {0, 1}},
				ImageSize -> Scaled[.4],
				Frame -> True,
				FrameTicks -> None
			], {
				{"MouseDown", 1} :> down[1],
				{"MouseDown", 2} :> down[2],
				"MouseDragged" :> move[],
				"MouseMoved" :> move[],
				"MouseUp" :> up[]
			},
            PassEventsDown -> True
        ],
		Framed[Button["Undo (z)", undo[], ImageSize -> Scaled[.15]], FrameStyle -> Transparent]
	},
        {SpanFromAbove, Framed[Button["Reset (r)", reset[], ImageSize -> Scaled[.15]], FrameStyle -> Transparent]},
		{SpanFromAbove, Dynamic @ Framed[ClickToCopy[Row[{"Click to copy Hypergraph: ", TraditionalForm[hg]}], hg], FrameStyle -> Transparent]},
		{SpanFromAbove, Row[{ColorSlider[Dynamic @ color], Spacer[10], Column[{"Recolor", Checkbox[Dynamic @ recolor]}, Alignment -> Center]}]},
        {SpanFromAbove, Row[{"Multiselect mode", Checkbox[Dynamic[multiSelect]]}, Alignment -> Center]},
        {SpanFromAbove, Dynamic @ Pane[If[multiSelect, "
🖱️(left-click) to create or select a vertex
Press \[EscapeKey] to cancel vertex selection
Press 'e' to create an edge
",
"
🖱️(left-click) to create a vertex
Drag&Move to make an edge
Press \[AltKey]/\[CommandKey] to change a vertex
"] <> "
Press \[ShiftKey] to change an edge
\[AltKey]/\[CommandKey]+🖱️(right-click) removes a vertex
\[ShiftKey]+🖱️(right-click) removes an edge\n\n", ImageSize -> Scaled[.3], Alignment -> Center]}
	},
	    Alignment -> Top
	];
    CellPrint @ Cell[BoxData @ ToBoxes @ grid, "Output",
        CellEventActions -> {
            "KeyDown" :> Switch[
                CurrentValue["EventKey"],
                "e", addEdge[]; update[],
                "\[RawEscape]", tmpEdge = {}; update[],
                "r", reset[],
                "z", undo[]; update[]
            ],
            PassEventsDown -> True
        },
        ShowSelection -> False,
        Selectable -> False,
        ContextMenu -> {}
    ]
]

HypergraphDraw[args___] := HypergraphDraw[Hypergraph[args]]

