Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphDraw"]



HypergraphDraw[] := DynamicModule[{
	vertices = <||>, edges = {},
	edgeStart = False, edgeUp = False, edgeFinish = False,
	edge = {}, tmpEdge = {},
	getVertex, down, move, up, undo, update,
	color = Black, vertexStyles = {}, edgeStyles = {},
	actions = {},
	hg = Hypergraph[]
},
	getVertex[] := If[Length[vertices] > 0,
		First[
			Nearest[Reverse /@ Normal[vertices], MousePosition["Graphics"], {1, .01}],
			Missing[]
		],
		Missing[]
	];
	down[] := (
		If[ edgeUp,
			edge = tmpEdge;
			edgeFinish = True,

			edgeStart = True;
			edge = {getVertex[] -> MousePosition["Graphics"]}
		];
	);
	move[] := (
		If[ edgeStart, edgeUp = True; edgeStart = False];
		If[ edgeUp,
			tmpEdge = Append[edge, getVertex[] -> MousePosition["Graphics"]];
			edgeFinish = False
		]
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
	Grid[{{
		Dynamic @ EventHandler[
			Show[{
				SimpleHypergraphPlot[hg],
				Graphics[{
					If[edgeUp, {color, Line[Values[tmpEdge]], Blue, Circle[#, 0.01] & /@
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
		{SpanFromAbove, Dynamic @ Framed[ClickToCopy[Row[{"Click to copy Hypergraph: ", TraditionalForm[hg]}], hg], FrameStyle -> Transparent]},
		{SpanFromAbove, ColorSlider[Dynamic @ color]}
	},
	    Alignment->Top
	]
]

