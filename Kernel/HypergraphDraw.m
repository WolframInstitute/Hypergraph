Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphDraw"]



Options[HypergraphDraw] := Join[{"InitialColor" -> Automatic}, Options[Hypergraph]]

HypergraphDraw[initHg : _Hypergraph ? HypergraphQ : Hypergraph[], opts : OptionsPattern[]] := Module[{
	hg,
	vertices, edges, nullEdges,
    vertexStyles, edgeStyles, vertexLabels,
	edgeStart = False, edgeUp = False, edgeNext = False, edgeFinish = False,
    vertexSelect = False, edgeSelect = False, vertexMove = False, edgeMove = False,
    multiSelect = True, vertexMode = False,
    vertexName = Null, edgeName = Null, vertexLabel = Null,
    points,
	edge = {}, tmpEdge = {}, vertexId, edgeId = Missing[], oldVertices = <||>, oldNullEdges = <||>,
	getVertex, down, move, up,
    do, undo, reset, update, addVertices, addEdge,
    renderEdges, renderLocalEdges,
    getVertexName, vertexRename, vertexRelabel,
    mousePosition,
	color,
	actions = {}, actionId = None, addAction,
    mousePos, startMousePos,
    grid,
    edgeRegions = {},
    edgeArrowsQ = False,
    graphicsSelectedQ = True
},
    points = Catenate @ MapThread[Take, {First[#, {}] & /@ Reap[SimpleHypergraphPlot[initHg], {"Vertex", "NullEdge"}][[2]], {VertexCount[initHg], EdgeCount[initHg, {}]}}];
    mousePosition[] := Replace[MousePosition["Graphics"], {None -> mousePos, pos_ :> (mousePos = pos)}];
	getVertex[pos_ : mousePosition[]] := If[Length[vertices] > 0,
		First[
			Nearest[Reverse /@ Normal[vertices], pos, {1, .02}],
			Missing[]
		],
		Missing[]
	];
    addAction[action_, updateQ_ : True] := (
        If[actionId === None, actionId = 0];
        actions = Take[actions, actionId];
        AppendTo[actions, action];
        do[updateQ]
    );
	down[i_] := (
        startMousePos = mousePosition[];
        edgeId = First[MapIndexed[If[RegionDistance[#, startMousePos] < 0.01, #2[[1]], Nothing] &, edgeRegions], Missing[]];
        vertexId = getVertex[startMousePos];
        If[! MissingQ[vertexId], vertexName = vertexId; vertexLabel = vertexLabels[vertexName]];
        If[! MissingQ[edgeId], edgeName = edges[[edgeId]]];
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
            oldNullEdges = nullEdges;
            update[],

            i == 1 && (! vertexMode || CurrentValue["AltKey"]) && ! MissingQ[edgeId] && MissingQ[vertexId],
            edgeSelect = True;
            oldVertices = vertices;
            oldNullEdges = nullEdges;
            update[],

            i == 2 && (vertexMode || CurrentValue["AltKey"]) && ! MissingQ[vertexId],
            addAction["ResetSelect"[tmpEdge]];
            tmpEdge = DeleteCases[tmpEdge, vertexId -> _];
            addAction["VertexDelete"[vertexId -> vertices[vertexId], vertexStyles[vertexId], vertexLabels[vertexId], edges]],

            i == 2 && (! vertexMode || CurrentValue["AltKey"]) && ! MissingQ[edgeId],
            addAction["EdgeDelete"[edgeId, edges[[edgeId]], edgeStyles[[edgeId]], edgeRegions[[edgeId]]]]
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
                renderLocalEdges[{vertexId}]
            ]
        ];
        If[ edgeMove,
            With[{
                diff = mousePosition[] - startMousePos,
                edge = edges[[edgeId]]
            },
                vertices = MapAt[# + diff &, oldVertices, {Key[#]} & /@ edge];
                If[ edge === {},
                    nullEdges = MapAt[# + diff &, oldNullEdges, Key[\[FormalN][Count[edges[[;; edgeId]], {}]]]];
                    renderEdges[{edgeId}],
                    renderLocalEdges[edge]
                ];
            ]
        ];
	);
    renderEdges[edgeIds_] := Block[{edgePositions, edgePrimitives},
        edgeRegions = PadRight[edgeRegions, Length[edges], EmptyRegion[2]];
        {edgePositions, edgePrimitives} = First[#, {}] & /@ Reap[
            SimpleHypergraphPlot[
                edges[[edgeIds]],
                VertexCoordinates -> Normal[Join[vertices, nullEdges]]
            ],
            {"Position", "Primitive"}
        ][[2]];

        (* TODO: figure out what causing weird double Reap *)
        If[ Length[edgePositions] > Length[edgeIds],
            edgePositions = Take[edgePositions, - Length[edgeIds]];
            edgePrimitives = Take[edgePrimitives, - Length[edgeIds]]
        ];

        edgeRegions[[edgeIds]] = Replace[edgePrimitives[[Ordering[edgePositions]]], primitive : _FilledCurve :> DiscretizeGraphics[primitive], {1}]
    ];
    renderLocalEdges[vertexIds_] := renderEdges[First /@ Position[edges, edge_ /; IntersectingQ[edge, vertexIds], {1}, Heads -> False]];
    do[updateQ_ : True] := (
        If[actionId === None || actionId >= Length[actions], Return[]];
        actionId += 1;
		Replace[actions[[actionId]], {
			"EdgeAdd"[edge_, edgeStyle_] :> (
                AppendTo[edges, edge]; AppendTo[edgeStyles, edgeStyle];
                If[edge === {}, AppendTo[nullEdges, \[FormalN][Length[nullEdges] + 1] -> mousePos]];
                renderEdges[{Length[edges]}];
                edgeName = edge
            ),
			"VertexAdd"[vertex_, coord_, vertexStyle_, vertexLabel_] :> (
                AppendTo[vertices, vertex -> coord];
                AppendTo[vertexStyles, vertex -> vertexStyle];
                AppendTo[vertexLabels, vertex -> vertexLabel]
            ),
            "VertexRecolor"[vertexId_, _, newStyle_] :> (vertexStyles[vertexId] = newStyle),
            "EdgeRecolor"[edgeId_, _, newStyle_] :> (edgeStyles[[edgeId]] = newStyle),
            "VertexDelete"[vertexId_ -> _, _, _, newEdges_] :> (
                vertices = Delete[vertices, Key[vertexId]];
                vertexStyles = Delete[vertexStyles, Key[vertexId]];
                vertexLabels = Delete[vertexLabels, Key[vertexId]];
                edges = Map[DeleteCases[#, vertexId] &, newEdges]
            ),
            "EdgeDelete"[edgeId_, __] :> (edges = Delete[edges, edgeId]; edgeStyles = Delete[edgeStyles, edgeId]; edgeRegions = Delete[edgeRegions, edgeId]),
            "VertexSelect"[vertex_] :> (AppendTo[tmpEdge, vertex]; If[! MissingQ[vertex[[1]]], vertexName = vertex[[1]]; vertexLabel = vertexLabels[vertexName]]),
            "ResetSelect"[_] :> (tmpEdge = {}),
            "VertexMove"[vertexId_, _, newPos_] :> (
                vertices[vertexId] = newPos;
                renderLocalEdges[{vertexId}]
            ),
            "EdgeMove"[edgeId_, _, newPos_] :> With[{edge = edges[[edgeId]]},
                If[ edge === {},
                    nullEdges[\[FormalN][Count[edges[[;; edgeId]], {}]]] = newPos;
                    renderEdges[{edgeId}],
                    MapThread[(vertices[#1] = #2) &, {edge, newPos}];
                    renderLocalEdges[edge]
                ];
            ],
            "VertexRename"[newVertexId_, oldVertices_, _, _, _] :> (
                addAction["ResetSelect"[ReplacePart[tmpEdge, {_, 1} -> newVertexId]], False];
                With[{repl = Alternatives @@ Keys[oldVertices] -> newVertexId},
                    vertices = KeyMap[Replace[repl], ReverseSortBy[vertices, # === newVertexId &]];
                    vertexStyles = KeyMap[Replace[repl], ReverseSortBy[vertexStyles, # === newVertexId &]];
                    vertexLabels = KeyMap[Replace[repl], ReverseSortBy[vertexLabels, # === newVertexId &]];
                    edges = Replace[edges, repl, {2}]
                ];
            ),
            "VertexRelabel"[vertices_List, newVertexLabel_, _] :> (
                addAction["ResetSelect"[tmpEdge], False];
                vertexLabels = ReplacePart[vertexLabels, Thread[vertices -> newVertexLabel, List, 1]]
            )
        }];
		If[updateQ, update[]]
	);
	undo[updateQ_ : True] := (
        If[actionId === None || actionId <= 0, Return[]];
		Replace[actions[[actionId]], {
			"EdgeAdd"[__] :> (
                If[Last[edges] === {}, nullEdges = Most[nullEdges]];
			    If[Length[edges] > 0, edges = Most[edges]; edgeStyles = Most[edgeStyles]; edgeRegions = Most[edgeRegions]]
            ),
			"VertexAdd"[__] :>
			    If[Length[vertices] > 0, vertices = Most[vertices]; vertexStyles = Most[vertexStyles]; vertexLabels = Most[vertexLabels]],
            "VertexRecolor"[vertexId_, oldStyle_, _] :> (vertexStyles[vertexId] = oldStyle),
            "EdgeRecolor"[edgeId_, oldStyle_, _] :> (edgeStyles[[edgeId]] = oldStyle),
            "VertexDelete"[vertex : (vertexId_ -> _), style_, label_, newEdges_] :> (
                AppendTo[vertices, vertex];
                AppendTo[vertexStyles, vertexId -> style];
                AppendTo[vertexLabels, vertexId -> label];
                edges = newEdges
            ),
            "EdgeDelete"[_, edge_, style_, primitive_] :> (AppendTo[edges, edge]; AppendTo[edgeStyles, style]; AppendTo[edgeRegions, primitive]),
            "VertexSelect"[_] :> (tmpEdge = Most[tmpEdge]),
            "ResetSelect"[oldTmpEdge_] :> (tmpEdge = oldTmpEdge),
            "VertexMove"[vertexId_, oldPos_, _] :> (
                vertices[vertexId] = oldPos;
                renderLocalEdges[{vertexId}]
            ),
            "EdgeMove"[edgeId_, oldPos_, _] :> With[{edge = edges[[edgeId]]},
                If[ edge === {},
                    nullEdges[\[FormalN][Count[edges[[;; edgeId]], {}]]] = oldPos;
                    renderEdges[{edgeId}],
                    MapThread[(vertices[#1] = #2) &, {edge, oldPos}];
                    renderLocalEdges[edge]
                ];
            ],
            "VertexRename"[newVertexId_, oldVertices_, oldVertexStyles_, oldVertexLabels_, oldEdges_] :> (
                vertices = <|Delete[vertices, Key[newVertexId]], oldVertices|>;
                vertexStyles = <|Delete[vertexStyles, Key[newVertexId]], oldVertexStyles|>;
                vertexLabels = <|Delete[vertexLabels, Key[newVertexId]], oldVertexLabels|>;
                edges = oldEdges
            ),
            "VertexRelabel"[vertices_List, _, oldVertexLabels_] :> (
                vertexLabels = ReplacePart[vertexLabels, Thread[vertices -> Lookup[oldVertexLabels, vertices]]]
            )
        }];
        actionId -= 1;
		If[updateQ, update[]]
	);
	up[] := (
        If[ multiSelect && (! edgeSelect || vertexSelect || MissingQ[vertexId]) && ! vertexMove && ! edgeMove && ! CurrentValue["ShiftKey"],
            addAction["VertexSelect"[vertexId -> mousePosition[]]];
        ];
        If[ vertexSelect,
            If[ CurrentValue["ShiftKey"] && vertexStyles[vertexId] =!= color,
                addAction["VertexRecolor"[vertexId, vertexStyles[vertexId], color]]
            ]
        ];
        If[ vertexMove,
            If[ startMousePos =!= mousePos,
                addAction["VertexMove"[vertexId, startMousePos, mousePos]]
            ];
            vertexId = Missing[];
        ];
        If[ edgeSelect,
            If[ CurrentValue["ShiftKey"] && edgeStyles[[edgeId]] =!= color,
                addAction["EdgeRecolor"[edgeId, edgeStyles[[edgeId]], color]]
            ];
        ];
        If[ edgeMove,
            With[{key = If[edges[[edgeId]] === {}, \[FormalN][Count[edges[[;; edgeId]], {}]], edges[[edgeId]]]},
                With[{oldPos = Lookup[Join[oldVertices, oldNullEdges], key], newPos = Lookup[Join[vertices, nullEdges], key]},
                    If[ oldPos =!= newPos,
                        addAction["EdgeMove"[edgeId, oldPos, newPos]]
                    ]
                ]
            ];
            edgeId = Missing[];
        ];
		If[ edgeStart,
			If[ MissingQ[getVertex[]],
				With[{v = getVertexName[]},
                    addAction["VertexAdd"[v, mousePos, color, v]]
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
    getVertexName[] := Max[0, Select[Keys[vertices], IntegerQ]] + 1;
    vertexRename[] := With[{keys = Key /@ Intersection[Keys[vertices], Append[Keys[tmpEdge], vertexName]]},
        addAction["VertexRename"[vertexName, vertices[[keys]], vertexStyles[[keys]], vertexLabels[[keys]], edges]]
    ];
    vertexRelabel[] := With[{keys = Key /@ Intersection[Keys[vertices], Append[Keys[tmpEdge], vertexName]]},
        addAction["VertexRelabel"[keys, vertexLabel, vertexLabels]]
    ];
    addVertices[] := With[{
        newEdge = KeyValueMap[
            If[ MissingQ[#[[1]]],
                With[{v = getVertexName[]},
                    addAction["VertexAdd"[v, #[[2]], color, v]];
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
        newEdge
    ];
    addEdge[] := addAction["EdgeAdd"[addVertices[], color]];
	update[] := (
		hg = Hypergraph[
			Keys[vertices], edges,
            "LayoutDimension" -> 2,
            "EdgeArrows" -> edgeArrowsQ,
			VertexStyle -> Normal[vertexStyles],
            VertexLabels -> Normal[vertexLabels],
			EdgeStyle -> Thread[edges -> edgeStyles],
			VertexCoordinates -> Normal[Join[vertices, nullEdges]],
            FilterRules[{opts}, Options[Hypergraph]],
            Options[initHg]
		];
        SelectionMove[EvaluationBox[], All, CellContents];
        graphicsSelectedQ = True
	);
    reset[] := (
        tmpEdge = {};
	    vertices = Association @ Lookup[Options[initHg], VertexCoordinates, <||>];
        nullEdges = <||>;
        vertexStyles = Replace[Lookup[Options[initHg], VertexStyle, Automatic], {
            rules : {(Rule | RuleDelayed) ...} :> Association[rules],
            Automatic :> AssociationThread[vertices, Black],
            style_ :> AssociationThread[vertices, style]
        }];
        vertexLabels = Replace[Lookup[Options[initHg], VertexLabels, Automatic],
            {
                rules : {(Rule | RuleDelayed) ...} :> Association[rules],
                Automatic :> AssociationThread[vertices, vertices],
                label_ :> AssociationThread[vertices, label]
            }
        ];
        edges = EdgeList[initHg];
        edgeStyles = Replace[Lookup[Options[initHg], EdgeStyle, Automatic], {
            rules : {(Rule | RuleDelayed) ...} :> rules[[All, 2]],
            Automatic :> Array[ColorData[97], Length[edges]],
            style_ :> ConstantArray[style, Length[edges]]
        }];
        color = Replace[OptionValue["InitialColor"], Automatic -> FirstCase[vertexStyles, _ ? ColorQ, Black, All]];
        actions = {};
        actionId = None;

        If[ Not[VertexCount[initHg] == Length[vertices] == Length[vertexStyles] == Length[vertexLabels] && EdgeCount[initHg] == Length[edges] == Length[edgeStyles]],
            Block[{
                scaledCoordinates = If[
                    points === {},
                    {},
                    RescalingTransform[If[#2 - #1 == 0, {#1, #1 + 1}, {##}] & @@@ CoordinateBounds[points], {{.1, .9}, {.1, .9}}][points]
                ],
                scaledVertexCoordinates, nullEdgeCoordinates
            },
                {scaledVertexCoordinates, nullEdgeCoordinates} = TakeDrop[scaledCoordinates, VertexCount[initHg]];
                vertices = AssociationThread[VertexList[initHg], scaledVertexCoordinates];
                nullEdges = AssociationThread[\[FormalN] /@ Range[Count[edges, {}]], nullEdgeCoordinates];
            ];
            vertexStyles = AssociationThread[Keys[vertices], color];
            vertexLabels = AssociationThread[Keys[vertices], Keys[vertices]];
            edgeStyles = PadRight[edgeStyles, Length[edges], color];
        ];
        renderEdges[Range[Length[edges]]];

        update[];
    );

    reset[];

	grid = Row[{
		EventHandler[
			Graphics[{
                Opacity[.5],
                Arrowheads[{{Medium, .5}}],
                AbsoluteThickness[Medium],
                Dynamic @ Thread[{edgeStyles, edgeRegions}],
                Opacity[1],
                Dynamic @ Thread[{Values[vertexStyles], Point /@ Values[vertices]}],
                Dynamic @ MapThread[makeVertexLabel, {Keys[vertices], Values[vertexLabels], Values[vertexStyles], Values[vertices]}],
                Dynamic @ {
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
                }
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
				"MouseUp" :> up[],
                "MouseEntered" :> (graphicsSelectedQ = True),
                "MouseExited" :> (graphicsSelectedQ = False)
            },
            PassEventsDown -> False,
            PassEventsUp -> False
        ],
	Column[{
		Button["Undo (z)", undo[], ImageSize -> Scaled[.15]],
        Button["Redo (d)", do[], ImageSize -> Scaled[.15]],
        Button["Reset (r)", reset[], ImageSize -> Scaled[.15]],
        Row[{"Vertex: ",
            Column[{
                InputField[Dynamic[vertexName, (vertexName = #; vertexRename[]) &], FieldSize -> 5, ReturnEntersInput -> False],
                Button["Rename", vertexRename[]]
            }],
            Column[{
                InputField[Dynamic[vertexLabel, (vertexLabel = #; vertexRelabel[]) &], FieldSize -> 5, ReturnEntersInput -> False],
                Button["Relabel", vertexRelabel[]]
            }]
        }],
        Row[{"Edge: ", InputField[Dynamic[edgeName], FieldSize -> 10, Enabled -> False]}],
		Dynamic @ ClickToCopy[Column[{"Click to copy Hypergraph:", Pane[TraditionalForm[hg], Scaled[.3], Alignment -> Center]}, Alignment -> Center], hg],
		Row[{"Edge arrows", Checkbox[Dynamic[edgeArrowsQ, (edgeArrowsQ = #; update[]) &]]}],
		ColorSlider[Dynamic @ color],
        Row[{"Multiselect mode", Checkbox[Dynamic[multiSelect]]}],
        RadioButtonBar[Dynamic[vertexMode], {True -> "Vertex mode", False -> "Edge mode"}],
        Dynamic @ Pane[Style[
            If[multiSelect, "
🖱️(left-click) to select a vertex
Press \[EscapeKey] or 'q' to cancel vertex selection
Press 'v' to create vertices
Press \[ReturnKey] or 'e' to create an edge
", "
🖱️(left-click) to create a vertex
🖱️(left-click) on a vertex and 🖱️(move) to make an edge
"] <> StringTemplate["
🖱️(left-click) on `1` to drag it
\[AltKey]/\[CommandKey] + 🖱️(left-click) on `2` to drag it
🖱️(right-click) to remove `1`
\[AltKey]/\[CommandKey] + 🖱(right-click) to remove `2`
\[ShiftKey] applies current color selection

"] @@ If[vertexMode, Identity, Reverse] @ {"a vertex", "an edge"},
            FontFamily -> "Source Sans Pro", FontSize -> 10
        ],
        ImageSize -> Scaled[.4], Alignment -> Center
    ]},
        Alignment -> Center
    ]
	},
	    Alignment -> Top,
        ImageSize -> 1280
	];
    CellPrint @ Cell[BoxData @ ToBoxes @ grid, "Output",
        CellEventActions -> {
            "KeyDown" :> If[graphicsSelectedQ,
                Switch[
                    CurrentValue["EventKey"],
                    "v", addVertices[],
                    "e", addEdge[],
                    "\[RawEscape]" | "q", addAction["ResetSelect"[tmpEdge]],
                    "r", reset[],
                    "z", undo[],
                    "d", do[]
                ]
            ],
            PassEventsDown :> ! graphicsSelectedQ,
            PassEventsUp -> False
        },
        ShowSelection -> True,
        Selectable -> False,
        Editable -> False,
        ContextMenu -> {},
        GeneratedCell -> True
    ]
]

HypergraphDraw[arg___, opts : OptionsPattern[]] := HypergraphDraw[Hypergraph[arg], opts]

