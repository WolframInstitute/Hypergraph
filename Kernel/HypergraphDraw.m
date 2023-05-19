Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphDraw"]



Options[HypergraphDraw] := Join[{"InitialColor" -> Automatic}, Options[Hypergraph]]

HypergraphDraw[initHg : _Hypergraph ? HypergraphQ : Hypergraph[], opts : OptionsPattern[]] := DynamicModule[{
	hg, points,
	vertices, edges, nullEdges,
    vertexStyles, vertexLabels,
    edgeStyles, edgeSymmetries,
    vertexSelect = False, edgeSelect = False, vertexMove = False, edgeMove = False,
    symmetry,
    vertexName = Null, vertexLabel = Null,
	vertexSelection = {}, vertexId = Missing[], edgeSelection = {}, edgeId = Missing[],
    oldVertices = <||>, oldNullEdges = <||>,
	getVertex, down, move, up,
    do, undo, reset, update, addVertices, addEdge,
    deleteSelection,
    renderEdges, renderLocalEdges,
    getVertexName, vertexRename, vertexRelabel,
    mousePosition,
	color,
	actions = {}, actionId = None, addAction,
    mousePos, startMousePos,
    grid,
    edgeRegions = {},
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
        edgeId = First[MapIndexed[If[RegionDistance[Once[DiscretizeGraphics[#]], startMousePos] < 0.01, #2[[1]], Nothing] &, edgeRegions], Missing[]];
        vertexId = getVertex[startMousePos];
        If[! MissingQ[vertexId], vertexName = vertexId; vertexLabel = vertexLabels[vertexName]];
        If[! MissingQ[edgeId], symmetry = edgeSymmetries[[edgeId]]];
        oldVertices = vertices;
        oldNullEdges = nullEdges;
		Which[
            i == 1 && ! MissingQ[edgeId] && MissingQ[vertexId] && CurrentValue["AltKey"],
            With[{pos = FirstPosition[edgeSelection, edgeId, None, {1}, Heads -> False]},
                If[pos =!= None, addAction["EdgeDeselect"[edgeId, First[pos]]]]
            ],

            i == 1 && ! MissingQ[edgeId] && MissingQ[vertexId],
            edgeSelect = True;
            addAction["EdgeSelect"[edgeId, Length[edgeSelection] + 1]],

            i == 1 && CurrentValue["AltKey"],
            Block[{index, distance},
                {index, distance} = If[vertexSelection === {}, {Missing[], Infinity}, First @ Nearest[vertexSelection[[All, 2]] -> {"Index", "Distance"}, startMousePos, 1]];
                If[ distance < 0.02,
                    addAction["VertexDeselect"[index, vertexSelection[[index]]]],
                    With[{v = getVertexName[]},
                        addAction["VertexAdd"[v, mousePos, color, v]]
                    ]
                ]
            ],

            i == 1,
            vertexSelect = True;
            addAction["VertexSelect"[vertexId -> mousePosition[]]]
		];
	);
    move[] := (
        If[ vertexSelect && ! MissingQ[vertexId],
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
    up[] := (
        If[ vertexSelect,
            If[ CurrentValue["ShiftKey"] && vertexStyles[vertexId] =!= color,
                addAction["VertexRecolor"[vertexId, vertexStyles[vertexId], color]],
                color = vertexStyles[vertexId]
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
                addAction["EdgeRecolor"[edgeId, edgeStyles[[edgeId]], color]],
                color = edgeStyles[[edgeId]];
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

		vertexSelect = edgeSelect = vertexMove = edgeMove = False;

		update[]
	);
    renderEdges[edgeIds_] := Block[{edgePositions, edgePrimitives},
        edgeRegions = PadRight[edgeRegions, Length[edges], EmptyRegion[2]];
        {edgePositions, edgePrimitives} = First[#, {}] & /@ Reap[
            SimpleHypergraphPlot[
                edges[[edgeIds]],
                VertexCoordinates -> Join[
                    Normal @ vertices,
                    With[{nullKeys = \[FormalN] /@ DeleteMissing[Lookup[PositionIndex[Lookup[PositionIndex[edges], Key[{}], {}]], edgeIds]][[All, 1]]},
                        Thread[\[FormalN] /@ Range[Length[nullKeys]] -> Lookup[nullEdges, nullKeys]]
                    ]
                ],
                "EdgeSymmetry" -> Thread[edges[[edgeIds]] -> edgeSymmetries[[edgeIds]]]
            ],
            {"Position", "Primitive"}
        ][[2]];

        (* TODO: figure out what causing weird double Reap *)
        If[ Length[edgePositions] > Length[edgeIds],
            edgePositions = Take[edgePositions, - Length[edgeIds]];
            edgePrimitives = Take[edgePrimitives, - Length[edgeIds]]
        ];

        edgeRegions[[edgeIds]] = edgePrimitives[[Ordering[edgePositions]]]
    ];
    renderLocalEdges[vertexIds_] := renderEdges[First /@ Position[edges, edge_ /; IntersectingQ[edge, vertexIds], {1}, Heads -> False]];
    do[updateQ_ : True] := (
        If[actionId === None || actionId >= Length[actions], Return[]];
        actionId += 1;
		Replace[actions[[actionId]], {
			"EdgeAdd"[edge_, edgeStyle_, edgeSymmetry_] :> (
                AppendTo[edges, edge]; AppendTo[edgeStyles, edgeStyle]; AppendTo[edgeSymmetries, edgeSymmetry];
                If[edge === {}, AppendTo[nullEdges, \[FormalN][Length[nullEdges] + 1] -> mousePos]];
                renderLocalEdges[edge];
                If[ CurrentValue["ShiftKey"],
                    addAction["EdgeSelect"[Length[edges], Length[edgeSelection] + 1]]
                ];
                If[ ! CurrentValue["ControlKey"],
                    addAction["ResetSelect"[vertexSelection, edgeSelection]]
                ]
            ),
			"VertexAdd"[vertex_, coord_, vertexStyle_, vertexLabel_] :> (
                AppendTo[vertices, vertex -> coord];
                AppendTo[vertexStyles, vertex -> vertexStyle];
                AppendTo[vertexLabels, vertex -> vertexLabel]
            ),
            "VertexRecolor"[vertexId_, _, newStyle_] :> (vertexStyles[vertexId] = newStyle),
            "EdgeRecolor"[edgeId_, _, newStyle_] :> (edgeStyles[[edgeId]] = newStyle),
            "VertexDelete"[vs_, _, _, oldEdges_, _, _] :> Block[{ids = Keys[vs], positions, pos = First[vs]},
                positions = {Key[#]} & /@ ids;
                vertices = Delete[vertices, positions];
                vertexStyles = Delete[vertexStyles, positions];
                vertexLabels = Delete[vertexLabels, positions];
                vertexSelection = DeleteCases[vertexSelection, Alternatives @@ ids -> _];
                edges = Map[oldEdge |->
                    If[ IntersectingQ[oldEdge, ids],
                        With[{newEdge = DeleteElements[oldEdge, ids]},
                            If[ newEdge === {},
                                AppendTo[nullEdges, \[FormalN][Length[nullEdges] + 1] -> pos];
                            ];
                            newEdge
                        ],
                        oldEdge
                    ],
                    oldEdges
                ];
                renderEdges[First /@ Position[oldEdges, edge_ /; IntersectingQ[edge, ids], {1}, Heads -> False]];
                ],
            "EdgeDelete"[edgeIds_, __] :> (
                edges = Delete[edges, List /@ edgeIds];
                edgeStyles = Delete[edgeStyles, List /@ edgeIds];
                edgeSymmetries = Delete[edgeSymmetries, List /@ edgeIds];
                edgeRegions = Delete[edgeRegions, List /@ edgeIds];
                edgeSelection = DeleteElements[edgeSelection, edgeIds]
            ),
            "VertexSelect"[vertex : (vertexId_ -> _)] :> (
                AppendTo[vertexSelection, vertex];
                If[! MissingQ[vertexId], vertexName = vertexId; vertexLabel = vertexLabels[vertexName]];
            ),
            "VertexDeselect"[index_, _] :> (vertexSelection = Delete[vertexSelection, index]),
            "EdgeSelect"[edgeId_, index_] :> (
                If[ ! MemberQ[edgeSelection, edgeId],
                    edgeSelection = Insert[edgeSelection, edgeId, index]
                ]
            ),
            "EdgeDeselect"[_, index_] :> (
                edgeSelection = Delete[edgeSelection, index];
            ),
            "ResetSelect"[__] :> (vertexSelection = {}; edgeSelection = {}),
            "VertexMove"[vertexId_, _, newPos_] :> (
                vertices[vertexId] = newPos;
                vertexSelection = Replace[vertexSelection, (vertexId -> _) -> vertexId -> newPos, {1}];
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
                addAction["ResetSelect"[ReplacePart[vertexSelection, {_, 1} -> newVertexId], edgeSelection], False];
                With[{repl = Alternatives @@ Keys[oldVertices] -> newVertexId},
                    vertices = KeyMap[Replace[repl], ReverseSortBy[vertices, # === newVertexId &]];
                    vertexStyles = KeyMap[Replace[repl], ReverseSortBy[vertexStyles, # === newVertexId &]];
                    vertexLabels = KeyMap[Replace[repl], ReverseSortBy[vertexLabels, # === newVertexId &]];
                    edges = Replace[edges, repl, {2}]
                ];
            ),
            "VertexRelabel"[vertices_List, newVertexLabel_, _] :> (
                addAction["ResetSelect"[vertexSelection, edgeSelection], False];
                vertexLabels = ReplacePart[vertexLabels, Thread[vertices -> newVertexLabel, List, 1]]
            )
        }];
		If[updateQ, update[]]
	);
	undo[updateQ_ : True] := (
        If[actionId === None || actionId <= 0, Return[]];
		Replace[actions[[actionId]], {
			"EdgeAdd"[__] :> (
                If[ Last[edges] === {}, nullEdges = Most[nullEdges]];
			    If[ Length[edges] > 0,
                    edges = Most[edges];
                    edgeStyles = Most[edgeStyles];
                    edgeSymmetries = Most[edgeSymmetries];
                    edgeRegions = Most[edgeRegions]
                ]
            ),
			"VertexAdd"[__] :>
			    If[Length[vertices] > 0, vertices = Most[vertices]; vertexStyles = Most[vertexStyles]; vertexLabels = Most[vertexLabels]],
            "VertexRecolor"[vertexId_, oldStyle_, _] :> (vertexStyles[vertexId] = oldStyle),
            "EdgeRecolor"[edgeId_, oldStyle_, _] :> (edgeStyles[[edgeId]] = oldStyle),
            "VertexDelete"[vs_, styles_, labels_, oldEdges_, oldNullEdges_, oldSelection_] :> (
                vertices = Join[vertices, vs];
                vertexStyles = Join[vertexStyles, AssociationThread[Keys[vs] -> styles]];
                vertexLabels = Join[vertexLabels, AssociationThread[Keys[vs] -> labels]];
                edges = oldEdges;
                nullEdges = oldNullEdges;
                renderLocalEdges[Keys[vs]];
                vertexSelection = oldSelection;
            ),
            "EdgeDelete"[edgeIds_, deletedEdges_, styles_, symmetries_, primitives_] :> With[{order = Ordering[edgeIds]},
                edges = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edges, Thread[{edgeIds, deletedEdges}][[order]]];
                edgeStyles = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edgeStyles, Thread[{edgeIds, styles}][[order]]];
                edgeSymmetries = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edgeSymmetries, Thread[{edgeIds, symmetries}][[order]]];
                edgeRegions = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edgeRegions, Thread[{edgeIds, primitives}][[order]]];
                edgeSelection = edgeIds
            ],
            "VertexSelect"[_] :> (vertexSelection = Most[vertexSelection]),
            "VertexDeselect"[index_, vertex_] :> (vertexSelection = Insert[vertexSelection, vertex, index]),
            "EdgeSelect"[_, index_] :> (
                edgeSelection = Delete[edgeSelection, index];
            ),
            "EdgeDeselect"[edgeId_, index_] :> (
                edgeSelection = Insert[edgeSelection, edgeId, index];
            ),
            "ResetSelect"[oldVertexSelection_, oldEdgeSelection_] :> (
                vertexSelection = oldVertexSelection;
                edgeSelection = oldEdgeSelection
            ),
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
    getVertexName[] := Max[0, Select[Keys[vertices], IntegerQ]] + 1;
    vertexRename[] := With[{keys = Key /@ Intersection[Keys[vertices], Append[Keys[vertexSelection], vertexName]]},
        addAction["VertexRename"[vertexName, vertices[[keys]], vertexStyles[[keys]], vertexLabels[[keys]], edges]]
    ];
    vertexRelabel[] := With[{keys = Key /@ Intersection[Keys[vertices], Append[Keys[vertexSelection], vertexName]]},
        addAction["VertexRelabel"[keys, vertexLabel, vertexLabels]]
    ];
    addVertices[] := KeyValueMap[
        If[ MissingQ[#[[1]]],
            With[{v = getVertexName[]},
                addAction["VertexAdd"[v, #[[2]], color, v]];
                vertexSelection = Replace[vertexSelection, # -> v -> #[[2]], {1}];
                Splice @ Table[v, #2]
            ],
            Splice @ Table[#[[1]], #2]
        ] &,
        Counts[vertexSelection]
    ];
    addEdge[] := addAction["EdgeAdd"[addVertices[], color, symmetry]];
    deleteSelection[] := Block[{deleteVertices = DeleteCases[vertexSelection, _Missing -> _], keys},
        If[ edgeSelection =!= {},
            addAction["EdgeDelete"[edgeSelection, edges[[edgeSelection]], edgeStyles[[edgeSelection]], edgeSymmetries[[edgeSelection]], edgeRegions[[edgeSelection]]]]
        ];
        If[ deleteVertices =!= {},
            keys = Key /@ Keys[deleteVertices];
            addAction["VertexDelete"[vertices[[keys]], Values[vertexStyles[[keys]]], Values[vertexLabels[[keys]]], edges, nullEdges, vertexSelection]]
        ]
    ];
	update[] := (
		hg = Hypergraph[
			Keys[vertices], edges,
            "LayoutDimension" -> 2,
			VertexStyle -> Normal[vertexStyles],
            VertexLabels -> Normal[vertexLabels],
			EdgeStyle -> Thread[edges -> edgeStyles],
            "EdgeSymmetry" -> Thread[edges -> edgeSymmetries],
			VertexCoordinates -> Normal[Join[vertices, nullEdges]],
            FilterRules[{opts}, Options[Hypergraph]],
            Options[initHg]
		];
        SelectionMove[EvaluationBox[], All, CellContents];
        graphicsSelectedQ = True
	);
    reset[] := (
        vertexSelection = {};
        edgeSelection = {};
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
        edgeSymmetries = Replace[Replace[edges, initHg["EdgeSymmetry"], {1}], Except["Unordered" | "Cyclic" | "Ordered"] -> "Unordered", {1}];
        color = Replace[OptionValue["InitialColor"], Automatic -> FirstCase[vertexStyles, _ ? ColorQ, Black, All]];
        symmetry = "Unordered";
        actions = {};
        actionId = None;

        If[ Not[VertexCount[initHg] == Length[vertices] == Length[vertexStyles] == Length[vertexLabels] && EdgeCount[initHg] == Length[edges] == Length[edgeStyles] == Length[edgeSymmetries]],
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
            edgeSymmetries = PadRight[edgeSymmetries, Length[edges], "Unordered"];
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
                Dynamic @ Thread[{MapAt[Directive[Dashed, EdgeForm[Directive[Dashed, Black]], #] & , edgeStyles, List /@ edgeSelection], edgeRegions}],
                Opacity[1],
                Dynamic @ Thread[{Values[vertexStyles], Point /@ Values[vertices]}],
                Dynamic @ MapThread[makeVertexLabel, {Keys[vertices], Values[vertexLabels], Values[vertexStyles], Values[vertices]}],
                Dynamic @ {
                    If[ Length[vertexSelection] > 0,
                        {   color, Dotted,
                            KeyValueMap[
                                With[{p = Lookup[vertices, #1]}, Table[Circle[p, 0.02 r], {r, #2}]] &,
                                Counts[Select[Keys[vertexSelection], Not @* MissingQ]]
                            ]
                        },
                        Nothing
                    ],
                    {
                        color, Dashed,
                        KeyValueMap[Table[Circle[#1, 0.02 r], {r, #2}] &, Counts[Values[Select[vertexSelection, MissingQ[#[[1]]] &]]]]
                    }
                }
            },
				PlotRange -> {{0, 1}, {0, 1}},
				ImageSize -> Scaled[.5],
				Frame -> True,
                FrameStyle -> Dashed,
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
        Row[{"Vertex: ",
            Column[{
                InputField[Dynamic[vertexName, (vertexName = #; vertexRename[]) &], FieldSize -> Scaled[.005], ReturnEntersInput -> False],
                Button["Rename", vertexRename[]]
            }],
            Column[{
                InputField[Dynamic[vertexLabel, (vertexLabel = #; vertexRelabel[]) &], FieldSize -> Scaled[.005], ReturnEntersInput -> False],
                Button["Relabel", vertexRelabel[]]
            }]
        }],
        Row[{"Vertices: ", InputField[Dynamic[DeleteMissing @ vertexSelection[[All, 1]]], FieldSize -> Scaled[.01], Enabled -> False]}],
        Row[{"Edges: ", InputField[Dynamic[edges[[edgeSelection]]], FieldSize -> Scaled[.01], Enabled -> False]}],
		Dynamic @ ClickToCopy[Column[{"Click to copy Hypergraph:", Pane[TraditionalForm[hg], Scaled[.3], Alignment -> Center]}, Alignment -> Center], hg],
        Pane[RadioButtonBar[Dynamic[symmetry], {"Unordered", "Cyclic", "Ordered"}], ImageSize -> Scaled[.3]],
		ColorSlider[Dynamic[color, (
            color = #;
            vertexStyles = ReplacePart[vertexStyles, {Key[#]} & /@ DeleteMissing[vertexSelection[[All, 1]]] -> color];
            edgeStyles = ReplacePart[edgeStyles, List /@ edgeSelection -> color]
        ) & ], ImageSize -> Scaled[.3]],
        Pane[Style["TODO: new instructions",
            FontFamily -> "Source Sans Pro", FontSize -> 10
        ],
            ImageSize -> Scaled[.45], Alignment -> Center
        ],
        Row[{
            Button["Undo (z)", undo[], ImageSize -> Scaled[.1]],
            Button["Redo (d)", do[], ImageSize -> Scaled[.1]],
            Button["Reset (r)", reset[], ImageSize -> Scaled[.1]]
        }],
        Row[{
            Button["Vertex (v)", addVertices[], ImageSize -> Scaled[.1]],
            Button["Edge (e)", addEdge[], ImageSize -> Scaled[.1]],
            Button["Delete (\[DeleteKey])", deleteSelection[], ImageSize -> Scaled[.1]]
        }]
    },
        Alignment -> Center
    ]
	},
	    Alignment -> Top
	];
    CellPrint @ Cell[BoxData @ ToBoxes @ grid, "Output",
        CellEventActions -> {
            "KeyDown" :> If[graphicsSelectedQ,
                Switch[
                    CurrentValue["EventKey"],
                    "v", addVertices[],
                    "e", addEdge[],
                    "\[RawEscape]" | "q", addAction["ResetSelect"[vertexSelection, edgeSelection]],
                    "r", reset[],
                    "z", undo[],
                    "d", do[],
                    "\b" | "", deleteSelection[]
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

