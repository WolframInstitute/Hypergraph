Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphDraw"]



Options[HypergraphDraw] := Join[{
    "InitialColor" -> Automatic,
    "EdgeLabels" -> {"f", "g", "h"},
    "VertexLabels" -> {"A", "B", "C"}
}, Options[Hypergraph]]

HypergraphDraw[hg : _Hypergraph ? HypergraphQ : Hypergraph[], opts : OptionsPattern[]] :=
    Module[{tmpHg = hg},
        HypergraphDraw[Dynamic[tmpHg], opts]
    ]

HypergraphDraw[Dynamic[hg_Symbol], opts : OptionsPattern[]] := DynamicModule[{
	initHg = hg, initOpts = Options[hg],
    points,
	vertices = <||>, edges, nullEdges,
    vertexStyles, vertexLabels,
    edgeStyles, edgeSymmetries, edgeLabels, edgeLabelPositions,
    vertexSelect = False, edgeSelect = False, vertexMove = False, edgeMove = False,
    edgeSymmetry, edgeLabel = None,
    vertexName = Null, vertexLabel = Automatic, vertexLabelOffsets = {},
	vertexSelection = {}, vertexId = Missing[], edgeSelection = {}, edgeId = Missing[],
    oldVertices = <||>, oldNullEdges = <||>,
	getVertex, down, move, up,
    do, undo, reset, update, addVertices, addEdge,
    deleteSelection,
    renderEdges, renderLocalEdges,
    getVertexName, vertexRename, vertexRelabel,
    edgeRelabel,
    mousePosition,
	color,
	actions = {}, actionId = None, addAction,
    mouseTmpPos = {0, 0}, startMousePos = None,
    canvas, settingsWidget,
    edgeRegions = {},
    graphicsEnteredQ = False, graphicsControlEnteredQ = False,
    flash = 0.2,
    edgeIndex = 1,
    reap,
    plotRange = {{0, 1}, {0, 1}}, pane = False,
    contextMenu = {}
},
    reap = Reap[SimpleHypergraphPlot[hg], _, Rule][[2]];
    points = Catenate @ MapThread[Take, {Lookup[reap, {"Vertex", "NullEdge"}, {}], {VertexCount[hg], EdgeCount[hg, {}]}}];
    vertexLabelOffsets = Lookup[reap, "VertexLabelOffset", {}];
    mousePosition[] := Replace[MousePosition["Graphics"], {None -> mouseTmpPos, pos : {_ ? NumericQ, _ ? NumericQ} :> (mouseTmpPos = pos)}];
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
        With[{tmpPos = mousePosition[]},
            If[startMousePos =!= None && EuclideanDistance[tmpPos, startMousePos] < 0.01, edgeIndex++, edgeIndex = 1];
            startMousePos = tmpPos
        ];
        edgeId = If[Length[#] == 0, Missing[], #[[ Mod[edgeIndex - 1, Length[#]] + 1 ]]] & @
            MapIndexed[If[RegionDistance[Once[DiscretizeGraphics[#]], startMousePos] < 0.01, #2[[1]], Nothing] &, edgeRegions /. Arrow[a_] :> a];
        vertexId = getVertex[startMousePos];
        If[! MissingQ[vertexId], vertexName = vertexId; vertexLabel = vertexLabels[vertexName]];
        oldVertices = vertices;
        oldNullEdges = nullEdges;
        contextMenu = Which[
            ! MissingQ[vertexId],
            {
                MenuItem[ToString[vertexName], KernelExecute[Null]],
                Delimiter,
                MenuItem[StringTemplate["Label: ``"][vertexLabel], KernelExecute[Null]],
                Menu["Relabel",
                    Map[
                        With[{command = ToString[Unevaluated[addAction["VertexRelabel"[{vertexId}, {vertexLabel}, #]]], InputForm]},
                            MenuItem[#, KernelExecute[ToExpression[command]], MenuEvaluator -> Automatic]
                        ] &,
                        OptionValue["VertexLabels"]
                    ]
                ],
                With[{command = ToString[Unevaluated[addAction["VertexDelete"[vertices[[{vertexId}]], {vertexStyles[[vertexId]]}, {vertexLabels[[vertexId]]}, edges, nullEdges, vertexSelection]]], InputForm]},
                    MenuItem["Delete", KernelExecute[ToExpression[command]], MenuEvaluator -> Automatic]
                ]
            },
            ! MissingQ[edgeId],
            With[{edgeLabel = edgeLabels[[edgeId]]},
                {
                    MenuItem[ToString[edges[[edgeId]]], KernelExecute[Null]],
                    Delimiter,
                    MenuItem[StringTemplate["Label: ``"][edgeLabel],  KernelExecute[Null]],
                    Menu["Relabel",
                        Map[
                            With[{command = ToString[Unevaluated[addAction["EdgeRelabel"[{edgeId}, {edgeLabel}, #]]], InputForm]},
                                MenuItem[#, KernelExecute[ToExpression[command]], MenuEvaluator -> Automatic]
                            ] &,
                            OptionValue["EdgeLabels"]
                        ]
                    ],
                    With[{command = ToString[Unevaluated[addAction["EdgeDelete"[{edgeId}, edges[[{edgeId}]], edgeStyles[[{edgeId}]], edgeLabels[[{edgeId}]], edgeSymmetries[[{edgeId}]], edgeRegions[[{edgeId}]]]]], InputForm]},
                        MenuItem["Delete", KernelExecute[ToExpression[command]], MenuEvaluator -> Automatic]
                    ]
                }
            ],
            True,
            {}
        ];
		Which[
            graphicsControlEnteredQ,
            Null,
            i == 1 && ! MissingQ[edgeId] && MissingQ[vertexId],
            With[{pos = FirstPosition[edgeSelection, edgeId, None, {1}, Heads -> False]},
                If[ pos =!= None,
                    addAction["EdgeDeselect"[edgeId, First[pos]]],
                    edgeSelect = True;
                    If[ ! CurrentValue["OptionKey"],
                        addAction["ResetEdgeSelect"[edgeSelection]]
                    ];
                    addAction["EdgeSelect"[edgeId, Length[edgeSelection] + 1]]
                ]
            ],

            i == 1 && MissingQ[vertexId] && CurrentValue["OptionKey"],
            With[{v = getVertexName[]},
                addAction["VertexAdd"[v, mouseTmpPos, color, Automatic]]
            ],

            i == 1 && MissingQ[vertexId] && CurrentValue["ShiftKey"],
            pane = True,

            i == 1 && MissingQ[vertexId],
            With[{v = getVertexName[]},
                addAction["VertexAdd"[v, mouseTmpPos, color, Automatic]];
                addAction["VertexSelect"[v -> mouseTmpPos]]
            ],

            i == 1 && CurrentValue["OptionKey"],
            vertexSelect = True;
            addAction["VertexSelect"[vertexId -> vertices[vertexId]]],

            i == 1,
            vertexSelect = True;
            With[{pos = FirstPosition[vertexSelection, vertexId -> _, None, {1}, Heads -> False]},
                If[ pos =!= None,
                    addAction["VertexDeselect"[First[pos], vertexSelection[[First[pos]]]]],
                    addAction["VertexSelect"[vertexId -> vertices[vertexId]]]
                ]
            ]
		];
	);
    move[] := (
        If[ pane, plotRange += 0.05 (startMousePos - mousePosition[])];
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
        If[ vertexSelect && ! MissingQ[vertexId],
            If[ CurrentValue["ShiftKey"] && vertexStyles[vertexId] =!= color,
                addAction["VertexRecolor"[vertexId, vertexStyles[vertexId], color]],
                color = vertexStyles[vertexId]
            ]
        ];
        If[ vertexMove,
            If[ startMousePos =!= mouseTmpPos,
                addAction["VertexMove"[vertexId, startMousePos, mouseTmpPos]]
            ];
            vertexId = Missing[];
        ];
        If[ edgeSelect,
            If[ CurrentValue["ShiftKey"] && edgeStyles[[edgeId]] =!= color,
                addAction["EdgeRecolor"[edgeId, edgeStyles[[edgeId]], color]],
                color = edgeStyles[[edgeId]];
                edgeLabel = edgeLabels[[edgeId]];
                edgeSymmetry = edgeSymmetries[[edgeId]]
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

		vertexSelect = edgeSelect = vertexMove = edgeMove = pane = False;

		update[]
	);
    renderEdges[edgeIds_] := Block[{edgePositions, edgePrimitives, newVertexLabelOffsets, newEdgeLabelPositions},
        edgeRegions = PadRight[edgeRegions, Length[edges], EmptyRegion[2]];
        {edgePositions, edgePrimitives, newVertexLabelOffsets, newEdgeLabelPositions} = First[#, {}] & /@ Reap[
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
            {"Position", "Primitive", "VertexLabelOffset", "EdgeLabelPosition"}
        ][[2]];

        vertexLabelOffsets = DeleteDuplicatesBy[First] @ Join[newVertexLabelOffsets, vertexLabelOffsets];

        (* TODO: figure out what causing weird double Reap *)
        If[ Length[edgePositions] > Length[edgeIds],
            edgePositions = Take[edgePositions, - Length[edgeIds]];
            edgePrimitives = Take[edgePrimitives, - Length[edgeIds]];
            newEdgeLabelPositions = Take[newEdgeLabelPositions, - Length[edgeIds]]
        ];
        edgePositions = Ordering[edgePositions];
        edgeRegions[[edgeIds]] = edgePrimitives[[edgePositions]];
        edgeLabelPositions[[edgeIds]] = newEdgeLabelPositions[[edgePositions]];
    ];
    renderLocalEdges[vertexIds_] := renderEdges[First /@ Position[edges, edge_ /; IntersectingQ[edge, vertexIds], {1}, Heads -> False]];
    do[updateQ_ : True] := (
        If[actionId === None || actionId >= Length[actions], Return[]];
        actionId += 1;
		Replace[actions[[actionId]], {
			"EdgeAdd"[edge_, edgeStyle_, symm_] :> (
                AppendTo[edges, edge]; AppendTo[edgeStyles, edgeStyle]; AppendTo[edgeLabels, edgeLabel];
                AppendTo[edgeSymmetries, symm]; AppendTo[edgeLabelPositions, Automatic];
                If[edge === {}, AppendTo[nullEdges, \[FormalN][Length[nullEdges] + 1] -> mouseTmpPos]; renderEdges[{Length[edges]}], renderLocalEdges[edge]];
                If[ CurrentValue["OptionKey"],
                    addAction["EdgeSelect"[Length[edges], Length[edgeSelection] + 1]]
                ];
                If[ ! CurrentValue["ControlKey"],
                    addAction["ResetSelect"[vertexSelection, edgeSelection]]
                ]
            ),
			"VertexAdd"[vertex_, coord_, vertexStyle_, label_] :> (
                AppendTo[vertices, vertex -> coord];
                AppendTo[vertexStyles, vertex -> vertexStyle];
                AppendTo[vertexLabels, vertex -> label]
            ),
            "VertexRecolor"[id_, _, newStyle_] :> (vertexStyles[id] = newStyle),
            "EdgeRecolor"[id_, _, newStyle_] :> (edgeStyles[[id]] = newStyle),
            "EdgeRelabel"[edgeIds_, _, newLabel_] :> (edgeLabels[[edgeIds]] = newLabel),
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
                edgeLabels = Delete[edgeLabels, List /@ edgeIds];
                edgeLabelPositions = Delete[edgeLabelPositions, List /@ edgeIds];
                edgeSymmetries = Delete[edgeSymmetries, List /@ edgeIds];
                edgeRegions = Delete[edgeRegions, List /@ edgeIds];
                edgeSelection = DeleteElements[edgeSelection, edgeIds];
            ),
            "VertexSelect"[vertex : (id_ -> _)] :> (
                AppendTo[vertexSelection, vertex];
                If[! MissingQ[id], vertexName = id; vertexLabel = vertexLabels[vertexName]];
            ),
            "VertexDeselect"[index_, _] :> (vertexSelection = Delete[vertexSelection, index]),
            "EdgeSelect"[id_, index_] :> (
                If[ ! MemberQ[edgeSelection, id],
                    edgeSelection = Insert[edgeSelection, id, index]
                ]
            ),
            "EdgeDeselect"[_, index_] :> (
                edgeSelection = Delete[edgeSelection, index];
            ),
            "ResetSelect"[__] :> (vertexSelection = {}; edgeSelection = {}),
            "ResetVertexSelect"[_] :> (vertexSelection = {};),
            "ResetEdgeSelect"[_] :> (edgeSelection = {}),
            "VertexMove"[id_, _, newPos_] :> (
                vertices[id] = newPos;
                vertexSelection = Replace[vertexSelection, (id -> _) -> id -> newPos, {1}];
                renderLocalEdges[{id}]
            ),
            "EdgeMove"[id_, _, newPos_] :> With[{edge = edges[[id]]},
                If[ edge === {},
                    nullEdges[\[FormalN][Count[edges[[;; id]], {}]]] = newPos;
                    renderEdges[{id}],
                    MapThread[(vertices[#1] = #2) &, {edge, newPos}];
                    renderLocalEdges[edge]
                ];
            ],
            "VertexRename"[newVertexId_, old_, _, _, _] :> (
                addAction["ResetSelect"[ReplacePart[vertexSelection, {_, 1} -> newVertexId], edgeSelection], False];
                With[{repl = Alternatives @@ Keys[old] -> newVertexId},
                    vertices = KeyMap[Replace[repl], ReverseSortBy[vertices, # === newVertexId &]];
                    vertexStyles = KeyMap[Replace[repl], ReverseSortBy[vertexStyles, # === newVertexId &]];
                    vertexLabels = KeyMap[Replace[repl], ReverseSortBy[vertexLabels, # === newVertexId &]];
                    edges = Replace[edges, repl, {2}];
                    renderLocalEdges[{newVertexId}]
                ];
            ),
            "VertexRelabel"[vs_List, _, newVertexLabel_] :> (
                addAction["ResetSelect"[vertexSelection, edgeSelection], False];
                vertexLabels = ReplacePart[vertexLabels, Thread[vs -> newVertexLabel, List, 1]]
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
                    edgeLabels = Most[edgeLabels];
                    edgeLabelPositions = Most[edgeLabelPositions];
                    edgeSymmetries = Most[edgeSymmetries];
                    edgeRegions = Most[edgeRegions];
                ]
            ),
			"VertexAdd"[__] :>
			    If[ Length[vertices] > 0,
                    vertexSelection = DeleteCases[vertexSelection, Last[Keys[vertices]] -> _];
                    vertices = Most[vertices]; vertexStyles = Most[vertexStyles]; vertexLabels = Most[vertexLabels]
                ],
            "VertexRecolor"[id_, oldStyle_, _] :> (vertexStyles[id] = oldStyle),
            "EdgeRecolor"[id_, oldStyle_, _] :> (edgeStyles[[id]] = oldStyle),
            "EdgeRelabel"[edgeIds_, oldLabels_, _] :> MapThread[(edgeLabels[[#1]] = #2) &, {edgeIds, oldLabels}],
            "VertexDelete"[vs_, styles_, labels_, oldEdges_, oldNull_, oldSelection_] :> (
                vertices = Join[vertices, vs];
                vertexStyles = Join[vertexStyles, AssociationThread[Keys[vs] -> styles]];
                vertexLabels = Join[vertexLabels, AssociationThread[Keys[vs] -> labels]];
                edges = oldEdges;
                nullEdges = oldNull;
                renderLocalEdges[Keys[vs]];
                vertexSelection = oldSelection;
            ),
            "EdgeDelete"[edgeIds_, deletedEdges_, styles_, labels_, symmetries_, primitives_] :> With[{order = Ordering[edgeIds]},
                edges = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edges, Thread[{edgeIds, deletedEdges}][[order]]];
                edgeStyles = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edgeStyles, Thread[{edgeIds, styles}][[order]]];
                edgeLabels = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edgeLabels, Thread[{edgeIds, labels}][[order]]];
                edgeLabelPositions = Fold[Insert[#1, Automatic, #2] &, edgeLabelPositions, edgeIds[[order]]];
                edgeSymmetries = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edgeSymmetries, Thread[{edgeIds, symmetries}][[order]]];
                edgeRegions = Fold[Insert[#1, #2[[2]], #2[[1]]] &, edgeRegions, Thread[{edgeIds, primitives}][[order]]];
                edgeSelection = edgeIds;
                renderLocalEdges[edgeIds]
            ],
            "VertexSelect"[_] :> If[vertexSelection =!= {}, vertexSelection = Most[vertexSelection]],
            "VertexDeselect"[index_, vertex_] :> (vertexSelection = Insert[vertexSelection, vertex, index]),
            "EdgeSelect"[_, index_] :> (
                edgeSelection = Delete[edgeSelection, index];
            ),
            "EdgeDeselect"[id_, index_] :> (
                edgeSelection = Insert[edgeSelection, id, index];
            ),
            "ResetSelect"[oldVertexSelection_, oldEdgeSelection_] :> (
                vertexSelection = oldVertexSelection;
                edgeSelection = oldEdgeSelection
            ),
            "ResetVertexSelect"[oldVertexSelection_] :> (
                vertexSelection = oldVertexSelection
            ),
            "ResetEdgeSelect"[oldEdgeSelection_] :> (
                edgeSelection = oldEdgeSelection
            ),
            "VertexMove"[id_, oldPos_, _] :> (
                vertices[id] = oldPos;
                renderLocalEdges[{id}]
            ),
            "EdgeMove"[id_, oldPos_, _] :> With[{edge = edges[[id]]},
                If[ edge === {},
                    nullEdges[\[FormalN][Count[edges[[;; id]], {}]]] = oldPos;
                    renderEdges[{id}],
                    MapThread[(vertices[#1] = #2) &, {edge, oldPos}];
                    renderLocalEdges[edge]
                ];
            ],
            "VertexRename"[newVertexId_, old_, oldVertexStyles_, oldVertexLabels_, oldEdges_] :> (
                vertices = <|Delete[vertices, Key[newVertexId]], old|>;
                vertexStyles = <|Delete[vertexStyles, Key[newVertexId]], oldVertexStyles|>;
                vertexLabels = <|Delete[vertexLabels, Key[newVertexId]], oldVertexLabels|>;
                edges = oldEdges;
                renderLocalEdges[Keys[old]]
            ),
            "VertexRelabel"[vs_List, oldVertexLabels_, _] :> (
                vertexLabels = ReplacePart[vertexLabels, Thread[vs -> Lookup[oldVertexLabels, vs]]]
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
        addAction["VertexRelabel"[keys, vertexLabels, vertexLabel]]
    ];
    edgeRelabel[] := If[edgeSelection =!= {}, addAction["EdgeRelabel"[edgeSelection, edgeLabels[[edgeSelection]], edgeLabel]]];
    addVertices[] := KeyValueMap[
        If[ MissingQ[#[[1]]],
            With[{v = getVertexName[]},
                addAction["VertexAdd"[v, #[[2]], color, Automatic]];
                vertexSelection = Replace[vertexSelection, # -> v -> #[[2]], {1}];
                Splice @ Table[v, #2]
            ],
            Splice @ Table[#[[1]], #2]
        ] &,
        Counts[vertexSelection]
    ];
    addEdge[] := addAction["EdgeAdd"[addVertices[], color, edgeSymmetry]];
    deleteSelection[] := Block[{deleteVertices = DeleteCases[vertexSelection, _Missing -> _], keys},
        If[ edgeSelection =!= {},
            addAction["EdgeDelete"[edgeSelection, edges[[edgeSelection]], edgeStyles[[edgeSelection]], edgeLabels[[edgeSelection]],
                edgeSymmetries[[edgeSelection]], edgeRegions[[edgeSelection]]]]
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
            VertexLabelStyle -> Normal[vertexStyles],
			EdgeStyle -> Thread[edges -> edgeStyles],
            "EdgeLineStyle" -> Thread[edges -> edgeStyles],
            EdgeLabels -> Thread[edges -> edgeLabels],
            EdgeLabelStyle -> Thread[edges -> Darker /@ edgeStyles],
            "EdgeSymmetry" -> Thread[edges -> edgeSymmetries],
			VertexCoordinates -> Normal[Join[vertices, nullEdges]],
            FilterRules[{opts}, Options[Hypergraph]],
            initOpts
		]
	);
    reset[] := (
        plotRange = {{0, 1}, {0, 1}};
        flash = 0;
        vertexSelection = {};
        edgeSelection = {};
	    vertices = Association @ Lookup[initOpts, VertexCoordinates, <||>];
        nullEdges = <||>;
        vertexStyles = Replace[Lookup[initOpts, VertexStyle, Automatic], {
            rules : {(_Rule | _RuleDelayed) ...} :> Association[rules],
            Automatic :> AssociationThread[vertices, Black],
            style_ :> AssociationThread[vertices, style]
        }];
        vertexLabels = Replace[Lookup[initOpts, VertexLabels, None],
            {
                rules : {(_Rule | _RuleDelayed) ...} :> Association[rules],
                Automatic :> AssociationThread[vertices, vertices],
                None :> AssociationThread[vertices, None],
                label_ :> AssociationThread[vertices, label]
            }
        ];
        edges = EdgeList[initHg];
        edgeStyles = Replace[Lookup[initOpts, EdgeStyle, Automatic], {
            rules : {(_Rule | _RuleDelayed) ...} :> rules[[All, 2]],
            Automatic :> Array[ColorData[97], Length[edges]],
            style_ :> ConstantArray[style, Length[edges]]
        }];
        edgeLabels = Replace[Lookup[initOpts, EdgeLabels, None], {
            rules : {(_Rule | _RuleDelayed) ...} :> rules[[All, 2]],
            Automatic :> edges,
            None :> ConstantArray[None, Length[edges]],
            label_ :> ConstantArray[label, Length[edges]]
        }];
        edgeLabelPositions = ConstantArray[Automatic, Length[edgeLabels]];
        edgeSymmetries = Replace[Replace[edges, initHg["EdgeSymmetry"], {1}], Except["Unordered" | "Cyclic" | "Ordered"] -> "Unordered", {1}];
        color = Replace[OptionValue["InitialColor"], Automatic -> FirstCase[vertexStyles, _ ? ColorQ, Black, All]];
        edgeSymmetry = "Unordered";
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
            edgeLabels = PadRight[edgeLabels, Length[edges], None];
            edgeLabelPositions = PadRight[edgeLabelPositions, Length[edges], Automatic];
            edgeStyles = PadRight[edgeStyles, Length[edges], color];
            edgeSymmetries = PadRight[edgeSymmetries, Length[edges], "Unordered"];
        ];
        renderEdges[Range[Length[edges]]];

        update[];
    );

    reset[];

    canvas = Style[EventHandler[
        Style[Graphics[{
            Opacity[.5],
            Arrowheads[{{Medium, .5}}],
            AbsoluteThickness[Medium],
            Dynamic[
                Refresh[flash = Mod[flash + 0.01, 2 Pi];, UpdateInterval -> 0.02];
                Thread[{MapAt[Directive[Opacity[Clip[Sin[flash] ^ 2, {0.05, 0.95}]], Dashed, EdgeForm[Directive[Dashed, Black]], #] &, edgeStyles, List /@ edgeSelection], edgeRegions}]
            ],
            Opacity[1],
            Dynamic @ Thread[{Values @ MapAt[Directive[Opacity[Clip[Sin[flash] ^ 2, {0.01, 0.99}]], #] &, vertexStyles, {Key[#]} & /@ DeleteDuplicates[DeleteMissing[Keys[vertexSelection]]]], Point /@ Values[vertices]}],
            Dynamic @ MapThread[makeVertexLabel, {Keys[vertices], Values[vertexLabels], Values[vertexStyles], Values[vertices], Lookup[vertexLabelOffsets, Keys[vertices], 0.03]}],
            Dynamic @ MapThread[
                If[ #1 === None,
                    {},
                    Text[Style[#1, Darker[#3]], #2]
                ] &, {edgeLabels, edgeLabelPositions, edgeStyles}
            ],
            Dynamic @ {
                If[ Length[vertexSelection] > 0,
                    {   color, Dotted,
                        KeyValueMap[
                            With[{p = Lookup[vertices, #1]}, Table[Circle[p, 0.02 Max[#2 - #1 & @@@ plotRange] r], {r, #2}]] &,
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
            Epilog -> Dynamic @ With[{mouseEvents = Sequence["MouseEntered" :> (graphicsControlEnteredQ = True), "MouseExited" :> (graphicsControlEnteredQ = False)]}, {
                Inset[Column[{
                    EventHandler[Framed[Style["+", 24]], {
                        "MouseDown" :> With[{p = Mean[Transpose[plotRange]], s = 0.8}, plotRange = ScalingTransform[s, {1, 0}, p] @ ScalingTransform[s, {0, 1}, p] @ plotRange],
                        mouseEvents
                    }],
                    EventHandler[Framed[Style["-", 24]], {
                        "MouseDown" :> With[{p = Mean[Transpose[plotRange]], s = 1.1}, plotRange = ScalingTransform[s, {1, 0}, p] @ ScalingTransform[s, {0, 1}, p] @ plotRange],
                        mouseEvents
                    }]
                }],
                    {Right, Top}, Scaled[{1.2, 1.1}]
                ],
                Inset[
                    EventHandler[Framed[Style["Return", 24]], {
                        "MouseDown" :> CellPrint[ExpressionCell[hg, "Input"]],
                        mouseEvents
                    }],
                    {Right, Bottom}, Scaled[{1.1, - .2}]
                ],
                Inset[Column[{
                        "Selection:",
                        Row[{"Vertices: ", DeleteMissing @ vertexSelection[[All, 1]]}],
                        Row[{"Edges: ", InputForm @ edges[[edgeSelection]]}]
                    }],
                    Scaled[{.01, 0.99}], Scaled[{0, 1}]
                ]
            }],
            PlotRange -> Dynamic[plotRange],
            ImageSize -> Scaled[.33],
            Frame -> True,
            FrameStyle -> Dashed,
            FrameTicks -> None,
            ContentSelectable -> False
        ], Selectable :> CurrentValue["ShiftKey"], ShowSelection -> True], {
            {"MouseDown", 1} :> down[1],
            {"MouseDown", 2} :> down[2],
            "MouseUp" :> up[],
            "MouseDragged" :> move[],
            "MouseMoved" :> move[],
            "MouseEntered" :> (graphicsEnteredQ = True),
            "MouseExited" :> (graphicsEnteredQ = False),
            "KeyDown" :> Switch[
                CurrentValue["EventKey"],
                " " | "e", addEdge[],
                "\[RawEscape]" | "q", addAction["ResetSelect"[vertexSelection, edgeSelection]],
                "r", reset[],
                "z", undo[],
                "d", do[],
                "\b" | "", deleteSelection[]
            ]
        },
        PassEventsUp -> False,
        PassEventsDown :> graphicsControlEnteredQ || CurrentValue["ShiftKey"]
    ], ContextMenu -> Dynamic[contextMenu]];

    settingsWidget = Panel @ Column[{
        Column[{
            Row[{
                "Vertex name: ",
                Style[
                    InputField[Dynamic[vertexName, (vertexName = #; vertexRename[]) &], FieldSize -> Scaled[.005], ReturnEntersInput -> False],
                    ShowSelection -> True
                ],
                Button["Rename", vertexRename[]]
            }],
            Row[{
                "Vertex label: ",
                Style[
                    InputField[Dynamic[vertexLabel, (vertexLabel = #; vertexRelabel[]) &], FieldSize -> Scaled[.005], ReturnEntersInput -> False],
                    ShowSelection -> True
                ],
                Button["Relabel", vertexRelabel[]]
            }]
        },
            Alignment -> Center
        ],
        Row[{"Edge label: ",
            Style[
                InputField[Dynamic[edgeLabel, (edgeLabel = #; edgeRelabel[]) &], FieldSize -> Scaled[.005], ReturnEntersInput -> False],
                ShowSelection -> True
            ],
            Button["Relabel", vertexRelabel[]]
        }],
        Pane[Row[{"Symmetry: ",
            RadioButtonBar[
                Dynamic[edgeSymmetry, (edgeSymmetry = #; edgeSymmetries[[edgeSelection]] = #; renderEdges[edgeSelection]; update[]) &],
                {"Unordered", "Cyclic", "Ordered"}
            ]}],
            ImageSize -> Scaled[.3],
            Alignment -> Center
        ],
		ColorSlider[Dynamic[color, (
            color = #;
            vertexStyles = ReplacePart[vertexStyles, {Key[#]} & /@ DeleteMissing[vertexSelection[[All, 1]]] -> color];
            edgeStyles = ReplacePart[edgeStyles, List /@ edgeSelection -> color];
            update[]
        ) & ], ImageSize -> Scaled[.3]]
    },
        Alignment -> Center
    ];
    Column[{
        ExpressionCell[canvas, ShowSelection -> False],
        OpenerView[{Spacer[0], settingsWidget}]
    }]
]

