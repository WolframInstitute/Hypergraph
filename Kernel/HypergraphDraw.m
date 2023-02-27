Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphDraw"]



HypergraphDraw[initHg : _Hypergraph ? HypergraphQ : Hypergraph[]] := DynamicModule[{
	hg,
	vertices, edges,
    vertexStyles, edgeStyles,
	edgeStart = False, edgeUp = False, edgeNext = False, edgeFinish = False, vertexMove = False, edgeMove = False,
	edge = {}, tmpEdge = {}, edgeId = Missing[], oldVertices = <||>,
	getVertex, down, move, up, undo, reset, update,
	color = Black,
	actions = {}
},
	getVertex[] := If[Length[vertices] > 0,
		First[
			Nearest[Reverse /@ Normal[vertices], MousePosition["Graphics"], {1, .01}],
			Missing[]
		],
		Missing[]
	];
	down[] := (
		Which[
            edgeUp,
			edge = tmpEdge;
			edgeFinish = True,

            CurrentValue["AltKey"] || CurrentValue["OptionKey"],
            vertexMove = True;
            vertexId = First[Nearest[Values[vertices] -> Keys[vertices], MousePosition["Graphics"]], Missing[]];
            If[ ! MissingQ[vertexId],
                oldVertices = vertices;
                vertexStyles[[vertexId]] = color;
                update[]
            ],

            CurrentValue["ShiftKey"],
            edgeMove = True;
            edgeId = First[Nearest[Mean[Lookup[vertices, #]] & /@ edges -> "Index", MousePosition["Graphics"]], Missing[]];
            If[ ! MissingQ[edgeId],
                oldVertices = vertices;
                edgeStyles[[edgeId]] = color;
                update[]
            ],

            True,
			edgeStart = True;
            edgeNest = True;
			edge = {getVertex[] -> MousePosition["Graphics"]}
		];
	);
	move[] := (
		If[ edgeStart, edgeUp = True; edgeStart = False];
        If[ Length[tmpEdge] == 0 || EuclideanDistance[tmpEdge[[-1, 2]], MousePosition["Graphics"]] > .01,
            edgeNext = True
        ];
		If[ edgeUp && edgeNext,
			tmpEdge = Append[edge, getVertex[] -> MousePosition["Graphics"]];
            edgeNext = False;
			edgeFinish = False;
		];
        If[ vertexMove,
            With[{diff = MousePosition["Graphics"] - Lookup[oldVertices, vertexId]},
                vertices = MapAt[# + diff &, oldVertices, Key[vertexId]];
                update[]
            ]
        ];
        If[ edgeMove,
            With[{diff = MousePosition["Graphics"] - Mean[Lookup[oldVertices, edges[[edgeId]]]]},
                vertices = MapAt[# + diff &, oldVertices, {Key[#]} & /@ edges[[edgeId]]];
                update[]
            ]
        ];
	);
	undo[] := (
		Switch[Last[actions, None],
			"EdgeAdd",
			If[Length[edges] > 0, edges = Most[edges]; edgeStyles = Most[edgeStyles]; actions = Most[actions]],
			"VertexAdd",
			If[Length[vertices] > 0, vertices = Most[vertices]; vertexStyles = Most[vertexStyles]; actions = Most[actions]]
		];
		update[]
	);
	up[] := (
        If[ vertexMove,
            vertexMove = False;
            vertxId = Missing[];
        ];
        If[ edgeMove,
            edgeMove = False;
            edgeId = Missing[];
        ];
		If[ edgeStart,
			If[ MissingQ[getVertex[]],
				AppendTo[vertices, Max[0, Keys[vertices]] + 1 -> MousePosition["Graphics"]];
				AppendTo[vertexStyles, color];
				AppendTo[actions, "VertexAdd"],

				AppendTo[edges, {getVertex[]}];
				AppendTo[edgeStyles, color];
				AppendTo[actions, "EdgeAdd"]
			]
		];
		If[ edgeFinish,
			AppendTo[
				edges,
				Map[
					If[ MissingQ[#[[1]]],
						With[{w = Max[0, Keys[vertices]] + 1},
							AppendTo[vertices, w -> #[[2]]];
							AppendTo[vertexStyles, color];
							AppendTo[actions, "VertexAdd"];
							w
						],
						#[[1]]
					] &,
					DeleteAdjacentDuplicates[tmpEdge, MissingQ[#1[[1]]] && MissingQ[#2[[1]]] &]
				]
			];
			AppendTo[edgeStyles, color];
			AppendTo[actions, "EdgeAdd"];
			edgeFinish = False;
			edgeUp = False
		];

		edge = tmpEdge;
		edgeStart = False;

		update[]
	);
	update[] := (
		hg = Hypergraph[
			Keys[vertices], edges,
			VertexCoordinates -> Normal[vertices],
			VertexLabels -> Automatic,
			VertexStyle -> Thread[Keys[vertices] -> vertexStyles],
			EdgeStyle -> Thread[edges -> edgeStyles]
		]
	);
    reset[] := (
	    vertices = Association @ Lookup[initHg["Options"], VertexCoordinates, <||>];
        vertexStyles = Replace[Lookup[initHg["Options"], VertexStyle, Automatic], {
            rules : {___Rule} :> rules[[All, 2]],
            Automatic :> Array[ColorData[97], Length[vertices]]
        }];
        edges = EdgeList[initHg];
        edgeStyles = Replace[Lookup[initHg["Options"], EdgeStyle, Automatic], {
            rules : {___Rule} :> rules[[All, 2]],
            Automatic :> Array[ColorData[97], Length[edges]]
        }];
        actions = {};

        If[ Not[VertexCount[initHg] == Length[vertices] == Length[vertexStyles] && EdgeCount[initHg] == Length[edges] == Length[edgeStyles]],
            vertices = AssociationThread[VertexList[initHg], # - Threaded[Mean[#] - {.5, .5}] & @ Rescale[HypergraphEmbedding[initHg]]];
            vertexStyles = PadRight[vertexStyles, Length[vertices], color];
            edgeStyles = PadRight[edgeStyles, Length[edges], color];
        ];

        update[];
    );

    reset[];

	Grid[{{
		Dynamic @ EventHandler[
			Show[{
				SimpleHypergraphPlot[hg],
				Graphics[{
					If[edgeUp, {color, Line[Append[Values[tmpEdge], MousePosition["Graphics"]]], Dashed, Blue, Circle[#, 0.01] & /@
                        Lookup[vertices, Select[Keys[tmpEdge], Not @* MissingQ]]}, Nothing]
				}]
            },
				PlotRange -> {{0, 1}, {0, 1}},
				ImageSize -> Scaled[.4],
				Frame -> True,
				FrameTicks -> None
			], {
				"MouseDown" :> down[],
				"MouseDragged" :> move[],
				"MouseMoved" :> move[],
				"MouseUp" :> up[]
			}
        ],
		Framed[Button["Undo", undo[], ImageSize -> Scaled[.15]], FrameStyle -> Transparent]
	},
        {SpanFromAbove, Framed[Button["Reset", reset[], ImageSize -> Scaled[.15]], FrameStyle -> Transparent]},
		{SpanFromAbove, Dynamic @ Framed[ClickToCopy[Row[{"Click to copy Hypergraph: ", TraditionalForm[hg]}], hg], FrameStyle -> Transparent]},
		{SpanFromAbove, ColorSlider[Dynamic @ color]},
        {SpanFromAbove, Pane["Click to create a vertex\nDrag&Move to make an edge\nPress \[AltKey] or \[OptionKey] to change vertex\nPress \[ShiftKey] to change edge"]}
	},
	    Alignment->Top
	]
]

