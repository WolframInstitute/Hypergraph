Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphDraw"]



Options[HypergraphDraw] := Join[{"InitialColor" -> Automatic}, Options[Hypergraph]]

HypergraphDraw[initHg : _Hypergraph ? HypergraphQ : Hypergraph[], opts : OptionsPattern[]] := DynamicModule[{
	hg,
	vertices, edges,
    vertexStyles, edgeStyles,
	edgeStart = False, edgeUp = False, edgeNext = False, edgeFinish = False, vertexMove = False, edgeMove = False,
    multiSelect = True, vertexMode = False,
	edge = {}, tmpEdge = {}, vertexId, edgeId = Missing[], oldVertices = <||>,
	getVertex, down, move, up, undo, reset, update, addEdge,
    mousePosition,
	color,
	actions = {},
    mousePos, startMousePos,
    plot, grid,
    edgeRegions
},
    mousePosition[] := Replace[MousePosition["Graphics"], {None -> mousePos, pos_ :> (mousePos = pos)}];
	getVertex[pos_ : mousePosition[]] := If[Length[vertices] > 0,
		First[
			Nearest[Reverse /@ Normal[vertices], pos, {1, .02}],
			Missing[]
		],
		Missing[]
	];
	down[i_] := (
        startMousePos = mousePosition[];
        edgeId = First[MapIndexed[If[RegionMember[#, startMousePos], #2[[1]], Nothing] &, edgeRegions], Missing[]];
        vertexId = getVertex[startMousePos];
		Which[

            i == 1 && edgeUp,
			edge = tmpEdge;
			edgeFinish = True,

            i == 1 && (vertexMode || CurrentValue["AltKey"]) && ! MissingQ[vertexId],
            vertexMove = True;
            oldVertices = vertices;
            update[],

            i == 1 && ! multiSelect && (vertexMode || CurrentValue["AltKey"] || MissingQ[edgeId] || ! MissingQ[vertexId]),
			edgeStart = True;
            edgeNext = True;
			edge = {vertexId -> mousePosition[]},

            i == 1 && (! vertexMode || CurrentValue["AltKey"]) && ! MissingQ[edgeId] && MissingQ[vertexId],
            edgeMove = True;
            oldVertices = vertices;
            update[],

            i == 2 && (vertexMode || CurrentValue["AltKey"]) && ! MissingQ[vertexId],
            tmpEdge = {};
            AppendTo[actions, "VertexDelete"[vertexId -> vertices[vertexId], vertexStyles[vertexId], edges]];
            vertices = Delete[vertices, Key[vertexId]];
            vertexStyles = Delete[vertexStyles, Key[vertexId]];
            edges = Map[DeleteCases[#, vertexId] &, edges];
            update[],

            i == 2 && (! vertexMode || CurrentValue["AltKey"]) && ! MissingQ[edgeId],
            AppendTo[actions, "EdgeDelete"[edges[[edgeId]], edgeStyles[[edgeId]]]];
            edges = Delete[edges, edgeId];
            edgeStyles = Delete[edgeStyles, edgeId];
            update[]
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
            With[{diff = mousePosition[] - startMousePos},
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
            "EdgeDelete"[edge_, style_] :> (AppendTo[edges, edge]; AppendTo[edgeStyles, style]; actions = Most[actions]),
            "VertexSelect" :> (tmpEdge = Most[tmpEdge]; actions = Most[actions]),
            "VertexMove"[vertexId_, oldPos_] :> (vertices[vertexId] = oldPos; actions = Most[actions]),
            "EdgeMove"[edge_, oldPos_] :> (MapThread[(vertices[#1] = #2) &, {edge, oldPos}]; actions = Most[actions])
        }];
		update[]
	);
	up[] := (
        If[ multiSelect && (! vertexMove && ! edgeMove || vertexMode || CurrentValue["AltKey"]),
            AppendTo[tmpEdge, getVertex[] -> mousePosition[]];
            AppendTo[actions, "VertexSelect"];
        ];
        If[ vertexMove,
            vertexMove = False;
            If[ CurrentValue["ShiftKey"],
                AppendTo[actions, "VertexRecolor"[vertexId, vertexStyles[vertexId]]];
                vertexStyles[vertexId] = color
            ];
            AppendTo[actions, "VertexMove"[vertexId, startMousePos]];
            vertexId = Missing[];
        ];
        If[ edgeMove,
            edgeMove = False;
            If[ CurrentValue["ShiftKey"],
                AppendTo[actions, "EdgeRecolor"[edgeId, edgeStyles[[edgeId]]]];
                edgeStyles[[edgeId]] = color
            ];
            AppendTo[actions, "EdgeMove"[edges[[edgeId]], Lookup[oldVertices, edges[[edgeId]]]]];
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
    addEdge[] := With[{
        newEdge = KeyValueMap[
            If[ MissingQ[#[[1]]],
                    With[{v = Max[0, Select[Keys[vertices], IntegerQ]] + 1},
                    AppendTo[vertices, v -> #[[2]]];
                    AppendTo[vertexStyles, v -> color];
                    AppendTo[actions, "VertexAdd"];
                    Splice @ Table[v, #2]
                ],
                #[[1]]
            ] &,
            Counts[tmpEdge]
        ]
    },
        AppendTo[edges, newEdge];
        AppendTo[edgeStyles, color];
		AppendTo[actions, "EdgeAdd"];
        tmpEdge = {};
        actions = DeleteCases[actions, "VertexSelect"];
    ];
	update[] := (
		hg = Hypergraph[
			Keys[vertices], edges,
            "LayoutDimension" -> 2,
            FilterRules[{opts}, Options[Hypergraph]],
            FilterRules[initHg["Options"], Except[VertexCoordinates | VertexStyle | EdgeStyle | "LayoutDimension"]],
			VertexCoordinates -> Normal[vertices],
			VertexLabels -> Automatic,
			VertexStyle -> Normal[vertexStyles],
			EdgeStyle -> Thread[edges -> edgeStyles]
		];
        Block[{positions},
            {edgeRegions, positions} = First[#, {}] & /@ Reap[plot = SimpleHypergraphPlot[hg], {"Primitive", "Position"}][[2]];
            edgeRegions = edgeRegions[[Ordering[positions]]];
            edgeRegions = If[MatchQ[#, _Line | _BSplineCurve], RegionDilation[#, 0.02] &, Identity] @ DiscretizeGraphics[#] & /@ edgeRegions
        ];
	);
    reset[] := (
        tmpEdge = {};
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
        color = Replace[OptionValue["InitialColor"], Automatic -> FirstCase[vertexStyles, _ ? ColorQ, Black, All]];
        actions = {};

        If[ Not[VertexCount[initHg] == Length[vertices] == Length[vertexStyles] && EdgeCount[initHg] == Length[edges] == Length[edgeStyles]],
            vertices = AssociationThread[
                VertexList[initHg],
                If[# === {}, {}, RescalingTransform[If[#2 - #1 == 0, {#1, #1 + 1}, {##}] & @@@ CoordinateBounds[#], {{.1, .9}, {.1, .9}}][#]] & @ HypergraphEmbedding[initHg][[All, ;; 2]]
            ];
            vertexStyles = AssociationThread[Keys[vertices], color];
            edgeStyles = PadRight[edgeStyles, Length[edges], color];
        ];

        update[];
    );

    reset[];

	grid = Grid[{{
		EventHandler[
			Dynamic @ Show[{
				plot,
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
                    If[ multiSelect, {
                        color, Dashed,
                        KeyValueMap[Table[Circle[#1, 0.02 r], {r, #2}] &, Counts[Values[Select[tmpEdge, MissingQ[#[[1]]] &]]]]
                    },
                        Nothing
                    ]
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
		{SpanFromAbove, Dynamic @ Framed[ClickToCopy[Column[{"Click to copy Hypergraph:", TraditionalForm[hg]}, Alignment -> Center], hg], FrameStyle -> Transparent]},
		{SpanFromAbove, ColorSlider[Dynamic @ color]},
        {SpanFromAbove, Row[{"Multiselect mode", Checkbox[Dynamic[multiSelect]]}, Alignment -> Center]},
        {SpanFromAbove, RadioButtonBar[Dynamic[vertexMode], {True -> "Vertex mode", False -> "Edge mode"}]},
        {SpanFromAbove, Dynamic @ Pane[If[multiSelect, "
🖱️(left-click) to create or select a vertex
Press \[EscapeKey] or 'q' to cancel vertex selection
Press \[ReturnKey] or 'e' to create an edge
",
"
🖱️(left-click) to create a vertex
🖱️(left-click) on a vertex and 🖱️(move) to make an edge
"] <> (StringTemplate["
🖱️(left-click) on `1` to drag it
\[AltKey]/\[CommandKey] + 🖱️(left-click) on `2` to drag it
🖱️(right-click) to remove `1`
\[AltKey]/\[CommandKey] + ️🖱(right-click) to remove `2`
\[ShiftKey] applies current color selection

"] @@ If[vertexMode, Identity, Reverse] @ {"a vertex", "an edge"}), ImageSize -> Scaled[.4], Alignment -> Center]}
	},
	    Alignment -> Top
	];
    CellPrint @ Cell[BoxData @ ToBoxes @ grid, "Output",
        CellEventActions -> {
            "KeyDown" :> Switch[
                CurrentValue["EventKey"],
                "\r" | "e", addEdge[]; update[],
                "\[RawEscape]" | "q", tmpEdge = {}; actions = DeleteCases[actions, "VertexSelect"]; update[],
                "r", reset[],
                "z", undo[]; update[]
            ]
        },
        ShowSelection -> False,
        Selectable -> False,
        ContextMenu -> {},
        GeneratedCell -> True
    ]
]

HypergraphDraw[arg___, opts : OptionsPattern[]] := HypergraphDraw[Hypergraph[arg], opts]

