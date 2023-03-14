Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphDraw"]



Options[HypergraphDraw] := Join[{"InitialColor" -> Automatic}, Options[Hypergraph]]

HypergraphDraw[initHg : _Hypergraph ? HypergraphQ : Hypergraph[], opts : OptionsPattern[]] := DynamicModule[{
	hg,
	vertices, edges,
    vertexStyles, edgeStyles,
	edgeStart = False, edgeUp = False, edgeNext = False, edgeFinish = False,
    vertexSelect = False, edgeSelect = False, vertexMove = False, edgeMove = False,
    multiSelect = True, vertexMode = False,
	edge = {}, tmpEdge = {}, vertexId, edgeId = Missing[], oldVertices = <||>,
	getVertex, down, move, up,
    do, undo, reset, update, addEdge,
    mousePosition,
	color,
	actions = {}, actionId = None, addAction,
    mousePos, startMousePos,
    plot, grid,
    edgeRegions,
    edgeArrowsQ = False
},
    mousePosition[] := Replace[MousePosition["Graphics"], {None -> mousePos, pos_ :> (mousePos = pos)}];
	getVertex[pos_ : mousePosition[]] := If[Length[vertices] > 0,
		First[
			Nearest[Reverse /@ Normal[vertices], pos, {1, .02}],
			Missing[]
		],
		Missing[]
	];
    addAction[action_] := (
        If[actionId === None, actionId = 0];
        actions = Take[actions, actionId];
        AppendTo[actions, action];
        do[]
    );
	down[i_] := (
        startMousePos = mousePosition[];
        edgeId = First[MapIndexed[If[RegionMember[#, startMousePos], #2[[1]], Nothing] &, edgeRegions], Missing[]];
        vertexId = getVertex[startMousePos];
		Which[

            i == 1 && edgeUp,
			edge = tmpEdge;
			edgeFinish = True,

            i == 1 && ! multiSelect && (MissingQ[edgeId] || ! MissingQ[vertexId]) && ! CurrentValue["AltKey"],
			edgeStart = True;
            edgeNext = True;
			edge = {vertexId -> mousePosition[]},

            i == 1 && (vertexMode || CurrentValue["AltKey"]) && ! MissingQ[vertexId],
            vertexSelect = True;
            oldVertices = vertices;
            update[],

            i == 1 && (! vertexMode || CurrentValue["AltKey"]) && ! MissingQ[edgeId] && MissingQ[vertexId],
            edgeSelect = True;
            oldVertices = vertices;
            update[],

            i == 2 && (vertexMode || CurrentValue["AltKey"]) && ! MissingQ[vertexId],
            addAction["ResetSelect"[tmpEdge]];
            tmpEdge = DeleteCases[tmpEdge, vertexId -> _];
            addAction["VertexDelete"[vertexId -> vertices[vertexId], vertexStyles[vertexId], edges]],

            i == 2 && (! vertexMode || CurrentValue["AltKey"]) && ! MissingQ[edgeId],
            addAction["EdgeDelete"[edgeId, edges[[edgeId]], edgeStyles[[edgeId]]]]
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
        If[ vertexSelect,
            vertexMove = True
        ];
        If[ edgeSelect,
            edgeMove = True
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
    do[] := (
        If[actionId === None || actionId >= Length[actions], Return[]];
		Replace[actions[[actionId + 1]], {
			"EdgeAdd"[edge_, edgeStyle_] :> (AppendTo[edges, edge]; AppendTo[edgeStyles, edgeStyle]),
			"VertexAdd"[vertex_Rule, vertexStyle_] :> (AppendTo[vertices, vertex]; AppendTo[vertexStyles, vertexStyle]),
            "VertexRecolor"[vertexId_, _, newStyle_] :> (vertexStyles[vertexId] = newStyle),
            "EdgeRecolor"[edgeId_, _, newStyle_] :> (edgeStyles[[edgeId]] = newStyle),
            "VertexDelete"[vertexId_ -> _, _, newEdges_] :> (
                vertices = Delete[vertices, Key[vertexId]];
                vertexStyles = Delete[vertexStyles, Key[vertexId]];
                edges = Map[DeleteCases[#, vertexId] &, newEdges]
            ),
            "EdgeDelete"[edgeId_, __] :> (edges = Delete[edges, edgeId]; edgeStyles = Delete[edgeStyles, edgeId]),
            "VertexSelect"[vertex_] :> AppendTo[tmpEdge, vertex],
            "ResetSelect"[_] :> (tmpEdge = {}),
            "VertexMove"[vertexId_, _, newPos_] :> (vertices[vertexId] = newPos),
            "EdgeMove"[edgeId_, _, newPos_] :> (MapThread[(vertices[#1] = #2) &, {edges[[edgeId]], newPos}])
        }];
        actionId += 1;
		update[]
	);
	undo[] := (
        If[actionId === None || actionId <= 0, Return[]];
		Replace[actions[[actionId]], {
			"EdgeAdd"[__] :>
			    If[Length[edges] > 0, edges = Most[edges]; edgeStyles = Most[edgeStyles]],
			"VertexAdd"[__] :>
			    If[Length[vertices] > 0, vertices = Most[vertices]; vertexStyles = Most[vertexStyles]],
            "VertexRecolor"[vertexId_, oldStyle_, _] :> (vertexStyles[vertexId] = oldStyle),
            "EdgeRecolor"[edgeId_, oldStyle_, _] :> (edgeStyles[[edgeId]] = oldStyle),
            "VertexDelete"[vertex : (vertexId_ -> _), style_, newEdges_] :> (
                AppendTo[vertices, vertex];
                AppendTo[vertexStyles, vertexId -> style];
                edges = newEdges
            ),
            "EdgeDelete"[_, edge_, style_] :> (AppendTo[edges, edge]; AppendTo[edgeStyles, style]),
            "VertexSelect"[_] :> (tmpEdge = Most[tmpEdge]),
            "ResetSelect"[oldTmpEdge_] :> (tmpEdge = oldTmpEdge),
            "VertexMove"[vertexId_, oldPos_, _] :> (vertices[vertexId] = oldPos),
            "EdgeMove"[edgeId_, oldPos_, _] :> (MapThread[(vertices[#1] = #2) &, {edges[[edgeId]], oldPos}])
        }];
        actionId -= 1;
		update[]
	);
	up[] := (
        If[ multiSelect && (! edgeSelect || vertexSelect || MissingQ[vertexId]) && ! vertexMove && ! edgeMove,
            addAction["VertexSelect"[vertexId -> mousePosition[]]];
        ];
        If[ vertexMove,
            If[ startMousePos =!= mousePos,
                addAction["VertexMove"[vertexId, startMousePos, mousePos]]
            ];
            If[ CurrentValue["ShiftKey"] && vertexStyles[vertexId] =!= color,
                addAction["VertexRecolor"[vertexId, vertexStyles[vertexId], color]]
            ];
            vertexId = Missing[];
        ];
        If[ edgeMove,
            With[{oldPos = Lookup[oldVertices, edges[[edgeId]]], newPos = Lookup[vertices, edges[[edgeId]]]},
                If[ oldPos =!= newPos,
                    addAction["EdgeMove"[edgeId, oldPos, newPos]]
                ]
            ];
            If[ CurrentValue["ShiftKey"] && edgeStyles[[edgeId]] =!= color,
                addAction["EdgeRecolor"[edgeId, edgeStyles[[edgeId]], color]]
            ];
            edgeId = Missing[];
        ];
		If[ edgeStart,
			If[ MissingQ[getVertex[]],
				With[{v = Max[0, Select[Keys[vertices], IntegerQ]] + 1},
                    addAction["VertexAdd"[v -> mousePos, v -> color]]
                ],
				addAction["EdgeAdd"[{getVertex[]}, color]]
			]
		];
		If[ edgeFinish,
			addEdge[];
			edgeFinish = False;
			edgeUp = False
		];

		edge = tmpEdge;
		edgeStart = vertexSelect = edgeSelect = vertexMove = edgeMove = False;

		update[]
	);
    addEdge[] := With[{
        newEdge = KeyValueMap[
            If[ MissingQ[#[[1]]],
                With[{v = Max[0, Select[Keys[vertices], IntegerQ]] + 1},
                    addAction["VertexAdd"[v -> #[[2]], v -> color]];
                    Splice @ Table[v, #2]
                ],
                Splice @ Table[#[[1]], #2]
            ] &,
            Counts[tmpEdge]
        ]
    },
        If[ tmpEdge =!= {},
		    addAction["ResetSelect"[tmpEdge]]
        ];
		addAction["EdgeAdd"[newEdge, color]];
    ];
	update[] := (
		hg = Hypergraph[
			Keys[vertices], edges,
            "LayoutDimension" -> 2,
            "EdgeArrows" -> edgeArrowsQ,
			VertexStyle -> Normal[vertexStyles],
			EdgeStyle -> Thread[edges -> edgeStyles],
			VertexCoordinates -> Normal[vertices],
            FilterRules[{opts}, Options[Hypergraph]],
            initHg["Options"],
			VertexLabels -> Automatic
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
        actionId = None;

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
        {SpanFromAbove, Framed[Button["Redo (d)", do[], ImageSize -> Scaled[.15]], FrameStyle -> Transparent]},
        {SpanFromAbove, Framed[Button["Reset (r)", reset[], ImageSize -> Scaled[.15]], FrameStyle -> Transparent]},
		{SpanFromAbove, Dynamic @ Framed[ClickToCopy[Column[{"Click to copy Hypergraph:", TraditionalForm[hg]}, Alignment -> Center], hg], FrameStyle -> Transparent]},
		{SpanFromAbove, Row[{"Edge arrows", Checkbox[Dynamic[edgeArrowsQ, (edgeArrowsQ = #; update[]) &]]}, Alignment -> Center]},
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
                "\r" | "e", addEdge[],
                "\[RawEscape]" | "q", addAction["ResetSelect"[tmpEdge]],
                "r", reset[],
                "z", undo[],
                "d", do[]
            ]
        },
        ShowSelection -> False,
        Selectable -> False,
        ContextMenu -> {},
        GeneratedCell -> True
    ]
]

HypergraphDraw[arg___, opts : OptionsPattern[]] := HypergraphDraw[Hypergraph[arg], opts]

