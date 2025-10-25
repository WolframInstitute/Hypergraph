(* ::Package:: *)

Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphIncidence"]
PackageExport["CanonicalHypergraph"]
PackageExport["CanonicalHypergraphRule"]
PackageExport["IsomorphicHypergraphQ"]
PackageExport["ToOrderedHypergraph"]
PackageExport["EdgeSymmetry"]
PackageExport["EdgeListTagged"]
PackageExport["HyperedgeList"]
PackageExport["EdgeMultiplicity"]
PackageExport["SimpleHypergraph"]
PackageExport["SimpleHypergraphQ"]
PackageExport["ConnectedHypergraphQ"]
PackageExport["HypergraphArityReduce"]
PackageExport["HypergraphUnion"]
PackageExport["HypergraphHadamardProduct"]
PackageExport["HypergraphToGraph"]
PackageExport["OrderedHypergraphToGraph"]

PackageScope["CanonicalEdge"]



HoldPattern[Options[hg_Hypergraph ? HypergraphQ]] ^:= hg["Options"]
HoldPattern[Options[hg_Hypergraph ? HypergraphQ, patt__]] ^:= FilterRules[Options[hg], patt]

HoldPattern[AbsoluteOptions[hg_Hypergraph ? HypergraphQ]] ^:= hg["AbsoluteOptions"]
HoldPattern[AbsoluteOptions[hg_Hypergraph ? HypergraphQ, patt__]] ^:= FilterRules[AbsoluteOptions[hg], patt]

HoldPattern[OptionValue[hg_Hypergraph ? HypergraphQ, names_]] ^:= OptionValue[AbsoluteOptions[hg], names]

Hypergraph /: AnnotationValue[hg_Hypergraph ? HypergraphQ, key_] := Lookup[AbsoluteOptions[hg], key]
(* AnnotationValue[{hg_Hypergraph ? HypergraphQ, items_}, key_] := Lookup[Lookup[AbsoluteOptions[hg], key, {}], items] *)

EdgeList[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeList"]

EdgeList[hg_Hypergraph ? HypergraphQ, patt_] ^:= Cases[hg["EdgeList"], patt]

EdgeListTagged[hg_Hypergraph ? HypergraphQ] := hg["EdgeListTagged"]

EdgeListTagged[hg_Hypergraph ? HypergraphQ, patt_] := Cases[hg["EdgeListTagged"], patt]

HyperedgeList[hg_Hypergraph ? HypergraphQ] := MapThread[
    Hyperedge[Replace[#1, (edge_ -> None) :> edge], Replace[#2, "Unordered" -> Sequence[]]] &,
    {EdgeListTagged[hg], hg["EdgeSymmetry"]}
]

HyperedgeList[hg_Hypergraph ? HypergraphQ, patt_] := Cases[HyperedgeList[hg], patt]

EdgeCount[hg_Hypergraph ? HypergraphQ, args___] ^:= Length @ EdgeList[hg, args]

EdgeTags[hg_Hypergraph ? HypergraphQ] ^:= hg["EdgeTags"]

EdgeSymmetry[hg_Hypergraph ? HypergraphQ] := hg["FullEdgeSymmetry"]

VertexList[hg_Hypergraph ? HypergraphQ] ^:= hg["VertexList"]

VertexList[hg_Hypergraph ? HypergraphQ, patt_] ^:= Cases[hg["VertexList"], patt]

VertexCount[hg_Hypergraph ? HypergraphQ, args___] ^:= Length @ VertexList[hg, args]


HypergraphIncidence[hg_ ? HypergraphQ] := Replace[Merge[(u |-> AssociationMap[u &, u]) /@ EdgeList[hg], Identity][[Key /@ VertexList[hg]]], _Missing -> {}, {1}]

EdgeMultiplicity[hg_ ? HypergraphQ] := Counts[CanonicalEdgeTagged @@@ Thread[EdgeList[hg] -> EdgeSymmetry[hg]]]


VertexDegree[hg_Hypergraph ? HypergraphQ] ^:= Values[Length /@ HypergraphIncidence[hg]]

VertexDegree[hg_Hypergraph ? HypergraphQ, vertex_] ^:= If[ListQ[vertex], Map[Length], Length] @ Lookup[HypergraphIncidence[hg], vertex]


mapEdgeOptions[f_, opt_] := Replace[Flatten[{opt}], {((edge_List -> tag_) -> v_) :> (f[edge] -> tag) -> v, (edge_List -> v_) :> f[edge] -> v}, {1}]

mapVertexOptions[f_, opt_] := Replace[Flatten[{opt}], (vertex_ -> v_) :> f[vertex] -> v, {1}]


CanonicalEdges[edge_List | (edge_List -> _), symm : {___Cycles}] := Sort[Permute[edge, #] & /@ GroupElements[PermutationGroup[symm]]]

CanonicalEdgesTagged[edge_List, symm : {___Cycles}] := CanonicalEdges[edge, symm]

CanonicalEdgesTagged[edge_List -> tag_, symm : {___Cycles}] := # -> tag & /@ CanonicalEdges[edge, symm]

CanonicalEdge[(edge_List -> _) | edge_List, symm : {___Cycles}] := First[CanonicalEdges[edge, symm]]

CanonicalEdgeTagged[edge_List, symm : {___Cycles}] := CanonicalEdge[edge, symm]

CanonicalEdgeTagged[edge_List -> tag_, symm : {___Cycles}] := CanonicalEdge[edge, symm] -> tag



Options[CanonicalHypergraph] = {Method -> Automatic, "Annotations" -> False}

CanonicalHypergraph[hg_ ? HypergraphQ, OptionsPattern[]] := Enclose @ Block[{
	vs = VertexList[hg], edges = EdgeList[hg], tags = EdgeTags[hg],
    taggedEdgePositions, emptyEdgePositions,
	orderedEdges, counts, iso, emptyEdges, newVertices, newEdges, ordering,
	tagVertices, freeVertices,
    opts = AbsoluteOptions[hg],
    vertexAnnotations, edgeAnnotations
},
    vertexAnnotations = Join[$VertexAnnotations, Lookup[opts, "VertexAnnotations", {}]];
    edgeAnnotations = Join[$EdgeAnnotations, Lookup[opts, "EdgeAnnotations", {}]];
	tagVertices = Catenate @ Reap[orderedEdges = MapThread[
		{edge, tag, symm} |-> With[{tagVertex = If[tag === None, None, Sow[Unique[]]]},
            If[edge === {}, If[tag === None, Nothing, {{tagVertex}}], If[tag === None, Identity, Append[tagVertex]] @ Permute[edge, #] & /@ GroupElements[PermutationGroup[symm]]]
        ],
		{edges, tags, EdgeSymmetry[hg]}
	]][[2]];
    counts = Length /@ orderedEdges;
    orderedEdges = Catenate @ orderedEdges;
    taggedEdgePositions = Position[tags, Except[None], {1}, Heads -> False];
    emptyEdgePositions = Position[edges, {}, {1}, Heads -> False];
    edges = Delete[edges, taggedEdgePositions];
    tags = Delete[tags, Complement[emptyEdgePositions, taggedEdgePositions]];
	emptyEdges = Cases[edges, {}];
    edges = Delete[edges, emptyEdgePositions];
    freeVertices = DeleteElements[vs, Union @@ edges];
	iso = Confirm @ Replace[OptionValue[Method], {
        Automatic | "Graph" -> CanonicalHypergraphGraphIsomorphism,
        "MultiGraph" -> CanonicalHypergraphMultiGraphIsomorphism,
        "Combinatorial" -> ResourceFunction["FindCanonicalHypergraphIsomorphism"]
    }][Join[List /@ freeVertices, orderedEdges]];
    iso = KeySelect[iso, ! MemberQ[tagVertices, #] &];
    Sow[iso, "Isomorphism"];
    newEdges = First @* Sort /@ TakeList[Map[Replace[iso], orderedEdges, {2}], counts];
    ordering = OrderingBy[newEdges, DeleteElements[#, tagVertices] &];
	newEdges = MapThread[If[#2 === None, #1, Most[#1] -> #2] &, {newEdges[[ordering]], tags[[ordering]]}];
    newVertices = Sort[Values[iso]];
    Hypergraph[
        newVertices,
        Join[emptyEdges, newEdges],
        With[{
            annotations = KeySort @ KeyTake[
                opts,
                If[ TrueQ[OptionValue["Annotations"]],
                    Join[vertexAnnotations, edgeAnnotations, {"VertexAnnotations", "EdgeAnnotations"}],
                    {"EdgeSymmetry"}
                ]
            ],
            processVertexAnnotations = Sort @ mapVertexOptions[Replace[#, iso] &, Thread[vs -> Replace[vs, #, 1]]] &,
            processEdgeAnnotations =  Catenate @ Values @ GroupBy[
                Join[Thread[emptyEdges -> Replace[emptyEdges, #, 1]], Thread[newEdges -> Replace[edges, #, 1][[ordering]]]],
                First,
                Sort
            ] &
        },               
            annotations // KeyValueMap[
                Switch[#1,
                    "VertexAnnotations" | "EdgeAnnotations", #1 -> Union[#2],
                    Alternatives @@ vertexAnnotations, #1 -> processVertexAnnotations[#2],
                    Alternatives @@ edgeAnnotations, #1 -> processEdgeAnnotations[#2],
                    _, Nothing
                ] &
            ]
        ]
   ]
]

CanonicalHypergraph[args___] := CanonicalHypergraph[Hypergraph[args]]


CanonicalHypergraphGraphIsomorphism[edges_] := Enclose @ Catch @ Block[{g = OrderedHypergraphToGraph[edges], cg, iso},
	cg = Check[CanonicalGraph[g], Throw[ResourceFunction["FindCanonicalHypergraphIsomorphism"][edges]], CanonicalGraph::ngen];
	iso = Sort @ Confirm @ First[FindGraphIsomorphism[g, cg], {}];
	KeyMap[Last] @ KeySelect[iso, MatchQ[{"Vertex", _}]]
]

CanonicalHypergraphMultiGraphIsomorphism[edges_] := Enclose @ Catch @ Block[{g = OrderedHypergraphToGraph[edges], mult, iso},
    mult = ResourceFunction["EdgeMultiplicity"][g];
    iso = Sort @ Confirm @ First[FindGraphIsomorphism[Keys[mult], CanonicalGraph[Keys[mult]]], {}];
	KeyMap[Last] @ KeySelect[iso, MatchQ[{"Vertex", _}]]
]


Options[CanonicalHypergraphRule] = {Method -> Automatic, "Annotations" -> False}

CanonicalHypergraphRule[HoldPattern[HypergraphRule[in_Hypergraph, out_Hypergraph, ___]] ? HypergraphRuleQ, OptionsPattern[]] := Enclose @ Block[{
	vsIn = VertexList[in], edgesIn = EdgeList[in], tagsIn = EdgeTags[in], symmIn = EdgeSymmetry[in],
    vsOut = VertexList[out], edgesOut = EdgeList[out], tagsOut = EdgeTags[out], symmOut = EdgeSymmetry[out],
    vs, edges, tags, symmetry,
    taggedEdgePositionsIn, emptyEdgePositionsIn, taggedEdgePositionsOut, emptyEdgePositionsOut,
	orderedEdges, counts, iso,
    emptyEdgesIn, emptyEdgesOut,
    orderingIn, orderingOut,
    newEdges, newEdgesIn, newEdgesOut,
	freeVerticesIn, freeVerticesOut,
    tagVertices,
    inOpts = AbsoluteOptions[in],
    outOpts = AbsoluteOptions[out],
    edgeAnnotations, vertexAnnotations
},
    vs = Join[vsIn, vsOut];
    edges = Join[edgesIn, edgesOut];
    tags = Join[tagsIn, tagsOut];
    symmetry = Join[symmIn, symmOut];
	tagVertices = Catenate @ Reap[orderedEdges = MapThread[
		{edge, tag, symm} |-> With[{tagVertex = If[tag === None, None, Sow[Unique[]]]},
            If[edge === {}, If[tag === None, Nothing, {{tagVertex}}], If[tag === None, Identity, Append[tagVertex]] @ Permute[edge, #] & /@ GroupElements[PermutationGroup[symm]]]
        ],
		{edges, tags, symmetry}
	]][[2]];
    counts = Length /@ orderedEdges;
    orderedEdges = Catenate @ orderedEdges;
    taggedEdgePositionsIn = Position[tagsIn, Except[None], {1}, Heads -> False];
    emptyEdgePositionsIn = Position[edgesIn, {}, {1}, Heads -> False];
    taggedEdgePositionsOut = Position[tagsOut, Except[None], {1}, Heads -> False];
    emptyEdgePositionsOut = Position[edgesOut, {}, {1}, Heads -> False];
    edgesIn = Delete[edgesIn, taggedEdgePositionsIn];
    tagsIn = Delete[tagsIn, Complement[taggedEdgePositionsIn, taggedEdgePositionsIn]];
    edgesOut = Delete[edgesOut, taggedEdgePositionsOut];
    tagsOut = Delete[tagsOut, Complement[taggedEdgePositionsOut, taggedEdgePositionsOut]];
	emptyEdgesIn = Cases[edgesIn, {}];
    emptyEdgesOut = Cases[edgesOut, {}];
    freeVerticesIn = DeleteElements[vsIn, Union @@ edgesIn];
    freeVerticesOut = DeleteElements[vsOut, Union @@ edgesOut];
    iso = Confirm @ Replace[OptionValue[Method], {
        Automatic | "Graph" -> CanonicalHypergraphGraphIsomorphism,
        "MultiGraph" -> CanonicalHypergraphMultiGraphIsomorphism,
        "Combinatorial" -> ResourceFunction["FindCanonicalHypergraphIsomorphism"]
    }][Join[List /@ freeVerticesIn, List /@ freeVerticesOut, orderedEdges]];
    iso = KeySelect[iso, ! MemberQ[tagVertices, #] &];
    newEdges = First @* Sort /@ TakeList[Map[Replace[iso], orderedEdges, {2}], counts];
    {newEdgesIn, newEdgesOut} = TakeDrop[newEdges, Length[edgesIn] + Length[taggedEdgePositionsIn] - Length[emptyEdgesIn]];
    orderingIn = Ordering[newEdgesIn];
    orderingOut = Ordering[newEdgesOut];
    newEdgesIn = MapThread[If[#2 === None, #1, Most[#1] -> #2] &, {newEdgesIn[[orderingIn]], tagsIn[[orderingIn]]}];
    newEdgesOut = MapThread[If[#2 === None, #1, Most[#1] -> #2] &, {newEdgesOut[[orderingOut]], tagsOut[[orderingOut]]}];
    If[ TrueQ[OptionValue["Annotations"]],
        edgeAnnotations = Join[$EdgeAnnotations, Lookup[inOpts, "EdgeAnnotations", {}], Lookup[outOpts, "EdgeAnnotations", {}], {"EdgeAnnotations"}, Lookup[outOpts, "EdgeAnnotations", {}], {"EdgeAnnotations"}];
        vertexAnnotations = Append[$VertexAnnotations, Lookup[inOpts, "VertexAnnotations", {}], Lookup[outOpts, "VertexAnnotations", {}], {"VertexAnnotations"}, Lookup[outOpts, "VertexAnnotations", {}], {"VertexAnnotations"}];
        ,
        edgeAnnotations = {"EdgeSymmetry"};
        vertexAnnotations = {}
    ];
    HypergraphRule[
        Hypergraph[
            Values[KeySelect[iso, MemberQ[vsIn, #] &]],
            Join[emptyEdgesIn, newEdgesIn],
            With[{
                annotations = KeySort @ KeyTake[inOpts, Join[edgeAnnotations, vertexAnnotations]],
                processVertexAnnotations = Sort @ mapVertexOptions[Replace[#, iso] &, #] &,
                processEdgeAnnotations = Join[Cases[#, HoldPattern[{} -> _]], Thread[newEdgesIn -> Cases[#, (Except[{}] -> x_) :> x][[orderingIn]]]] &
            },               
                annotations // KeyValueMap[
                    Switch[#1,
                        "VertexAnnotations" | "EdgeAnnotations", #1 -> Union[#2],
                        Alternatives @@ edgeAnnotations, #1 -> processVertexAnnotations[#2],
                        Alternatives @@ vertexAnnotations, #1 -> processEdgeAnnotations[#2],
                        _, Nothing
                    ] &
                ]
            ]
        ],
        Hypergraph[
            Values[KeySelect[iso, MemberQ[vsOut, #] &]],
            Join[emptyEdgesOut, newEdgesOut],
            With[{
                annotations = KeySort @ KeyTake[outOpts, Join[edgeAnnotations, vertexAnnotations]],
                processVertexAnnotations = Sort @ mapVertexOptions[Replace[#, iso] &, #] &,
                processEdgeAnnotations = Join[Cases[#, HoldPattern[{} -> _]], Thread[newEdgesOut -> Cases[#, (Except[{}] -> x_) :> x][[orderingOut]]]] &
            },               
                annotations // KeyValueMap[
                    Switch[#1,
                        "VertexAnnotations" | "EdgeAnnotations", #1 -> Union[#2],
                        Alternatives @@ Join[vertexAnnotations, Lookup[#2, "VertexAnnotations", {}]], #1 -> processVertexAnnotations[#2],
                        Alternatives @@ Join[edgeAnnotations, Lookup[#2, "EdgeAnnotations", {}]], #1 -> processEdgeAnnotations[#2],
                        _, Nothing
                    ] &
                ]
            ]
        ]
    ]
]

CanonicalHypergraphRule[in_, out_] := CanonicalHypergraphRule[HypergraphRule[in, out]]

CanonicalHypergraphRule[in_ -> out_] := CanonicalHypergraphRule[HypergraphRule[in, out]]


IsomorphicHypergraphQ[hg1_ ? HypergraphQ, hg2_ ? HypergraphQ] :=
    Through[{VertexList, EdgeList, EdgeSymmetry} @ CanonicalHypergraph[hg1]] === Through[{VertexList, EdgeList, EdgeSymmetry} @ CanonicalHypergraph[hg2]]


ToOrderedHypergraph[hg_ ? HypergraphQ] := Hypergraph[
	VertexList[hg],
	Catenate @ MapThread[
		{edge, tag, symm} |-> (If[tag === None, #, # -> tag] & @ Permute[edge, #] & /@ GroupElements[PermutationGroup[symm]]),
		{EdgeList[hg], EdgeTags[hg], EdgeSymmetry[hg]}
	],
	"EdgeSymmetry" -> "Ordered",
	Options[hg]
]


HypergraphArityReduce[hg_ ? HypergraphQ, arity_Integer ? Positive] := With[{
    edgeMapping = MapThread[
        {edge, tag} |-> With[{subsets = Subsets[edge, {arity}]},
            If[tag === None, edge -> subsets, (edge -> tag) -> Thread[subsets -> tag, List, 1]]
        ],
        {EdgeList[hg], EdgeTags[hg]}
    ]
},
    Hypergraph[
        VertexList[hg],
        Catenate @ Replace[EdgeListTagged[hg], edgeMapping, {1}],
        Replace[
            Options[hg],
            (option : EdgeStyle | EdgeLabels | EdgeLabelStyle | "EdgeSymmetry" -> annotations_) :>
                option -> Catenate @ Replace[Flatten[{annotations}], (edge_ -> annotation_) :> (# -> annotation & /@ Replace[edge, edgeMapping]), {1}],
            {1}
        ]
    ]
]


Hypergraph /: VertexReplace[hg_Hypergraph, rules_, opts : OptionsPattern[]] := With[{vertices = VertexList[hg]}, {
    newVertices = Replace[vertices, rules, 1],
    newEdges = (Replace[{(edge_ -> tag_) :> Replace[edge, rules, 1] -> tag, edge_ :> Replace[edge, rules, 1]}] /@ EdgeListTagged[hg])
}, 
    Hypergraph[
        DeleteDuplicates @ newVertices,
        newEdges,
        opts,
        Replace[
            AbsoluteOptions[hg],
            {
                (opt : Alternatives @@ $VertexAnnotations -> annotation_List) :>
                    opt -> Extract[Thread[newVertices -> annotation[[All, 2]]], Values @ KeySelect[PositionIndex[vertices], MemberQ[newVertices, Verbatim[#]] &]],
                (opt : Alternatives @@ $EdgeAnnotations -> annotation_List) :>
                    opt -> Thread[newEdges -> annotation[[All, 2]]]
            },
            1
        ]
    ]
]

Hypergraph /: HoldPattern[VertexReplace[rules_][hg_Hypergraph]] := VertexReplace[hg, rules]


SimpleHypergraph[hg_ ? HypergraphQ, opts : OptionsPattern[]] := With[{symm = Thread[EdgeList[hg] -> EdgeSymmetry[hg]]},
    Hypergraph[
        VertexList[hg],
        Select[DuplicateFreeQ] @ DeleteDuplicates @ MapThread[CanonicalEdge, {Replace[EdgeListTagged[hg], (edge_ -> _) :> edge, {1}], Values[symm]}],
        opts,
        Replace[
            Options[hg],
            (opt : (EdgeStyle | EdgeLabels | EdgeLabelStyle | "EdgeSymmetry") -> annotation_List) :>
                    opt -> DeleteDuplicatesBy[First] @ Replace[
                        annotation,
                        ((edge_ -> _) | edge_ -> a_) /; EdgeQ[hg, Verbatim[edge]] :>
                            If[DuplicateFreeQ[edge], CanonicalEdge[edge, Lookup[symm, Key[edge]]] -> a, Nothing],
                        {1}
                    ],
            {1}
        ]
    ]
]

SimpleHypergraph[args___, opts : OptionsPattern[]] := SimpleHypergraph[Hypergraph[args], opts]

SimpleHypergraphQ[hg_ ? HypergraphQ] := EdgeCount[hg] === EdgeCount[SimpleHypergraph[hg]]

ConnectedHypergraphQ[hg_ ? HypergraphQ] := ConnectedGraphQ[Graph[VertexList[hg], Catenate[Which[Length[#] > 2, Partition[#, 2, 1], Length[#] == 2, {#}, True, Nothing] & /@ EdgeList[hg]]]]


Hypergraph /: VertexIndex[hg_Hypergraph, v : Except[_List]] := First @ FirstPosition[VertexList[hg], v, {Missing[v]}, {1}, Heads -> False]

Hypergraph /: VertexIndex[hg_Hypergraph, vs_List] := VertexIndex[hg, #] & /@ vs


Hypergraph /: EdgeIndex[hg_Hypergraph, e : Except[_List]] := First @ FirstPosition[EdgeListTagged[hg], e, {Missing[e]}, {1}, Heads -> False]

Hypergraph /: EdgeIndex[hg_Hypergraph, es_List] := EdgeIndex[hg, #] & /@ es


Hypergraph /: VertexAdd[hg_Hypergraph, vertices_List, opts : OptionsPattern[]] :=
    Hypergraph[Join[VertexList[hg], vertices], EdgeListTagged[hg], opts, hg["Options"]]

Hypergraph /: VertexAdd[hg_Hypergraph, vertex_, opts : OptionsPattern[]] := VertexAdd[hg, {vertex}, opts]


Hypergraph /: EdgeAdd[hg_Hypergraph, edges_List, opts : OptionsPattern[]] :=
    Hypergraph[VertexList[hg], Join[EdgeListTagged[hg], edges], opts, hg["Options"]]

Hypergraph /: EdgeAdd[hg_Hypergraph, edge_, opts : OptionsPattern[]] := EdgeAdd[hg, {edge}, opts]


Hypergraph /: EdgeDelete[hg_Hypergraph, edges_List, opts : OptionsPattern[]] :=
    Hypergraph[VertexList[hg], DeleteCases[EdgeListTagged[hg], Alternatives @@ edges], opts, hg["Options"]]

Hypergraph /: EdgeDelete[hg_Hypergraph, edge_, opts : OptionsPattern[]] := EdgeDelete[hg, {edge}, opts]


Hypergraph /: VertexQ[hg_Hypergraph, vertex_] := MemberQ[VertexList[hg], vertex]

Hypergraph /: EdgeQ[hg_Hypergraph, edge_List] := MemberQ[EdgeList[hg], edge]

Hypergraph /: EdgeQ[hg_Hypergraph, edge_Rule] := MemberQ[EdgeListTagged[hg], edge]


HypergraphUnion[hs___Hypergraph] := Hypergraph[
	Union @@ (VertexList /@ {hs}),
	Through[Unevaluated @ Plus[hs]["Edges"]],
	With[{
        annotations = Merge[AbsoluteOptions /@ {hs}, Identity],
        processVertexAnnotations = DeleteDuplicatesBy[First] @* DeleteCases[_ -> None] @* Catenate,
        processEdgeAnnotations = DeleteCases[_ -> None] @* Catenate
    }, {
        vertexAnnotations = Join[$VertexAnnotations, Union @ Catenate[Lookup[annotations, "VertexAnnotations", {}]]],
        edgeAnnotations = Join[$EdgeAnnotations, Union @ Catenate[Lookup[annotations, "EdgeAnnotations", {}]]]
    },
        annotations // KeyValueMap[
	        Switch[#1,
                "VertexAnnotations" | "EdgeAnnotations", #1 -> Union @@ #2,
                Alternatives @@ vertexAnnotations, #1 -> processVertexAnnotations[#2],
                Alternatives @@ edgeAnnotations, #1 -> processEdgeAnnotations[#2],
                _, #1 -> First[#2]
            ] & 
        ]
    ]
]

HypergraphHadamardProduct[h1_Hypergraph, h2_Hypergraph] := Hypergraph[
    Union[VertexList[h1], VertexList[h2]],
    Catenate @ Values @ Merge[KeyUnion[{GroupBy[EdgeListTagged[h1], Replace[(edge_ -> _) :> edge]], Counts[EdgeList[h2]]}],
        If[MissingQ[#[[1]]] || MissingQ[#[[2]]], Nothing, Catenate[Table @@ #]] &
    ],
    With[{
        annotations = Merge[AbsoluteOptions /@ {hs}, Identity],
        processVertexAnnotations = DeleteDuplicatesBy[First] @* DeleteCases[_ -> None] @* Catenate
    }, {
        vertexAnnotations = Join[$VertexAnnotations, Union @ Catenate[Lookup[annotations, "VertexAnnotations", {}]]]
    },
        annotations // 
            KeyValueMap[
                Switch[#1,
                    "VertexAnnotations", #1 -> Union @@ #2,
                    Alternatives @@ vertexAnnotations, #1 -> processVertexAnnotations[#2],
                    _, #1 -> First[#2]
               ] &
             ] 
        ]
    ]

HypergraphHadamardProduct[hs___Hypergraph] := Fold[HypergraphHadamardProduct, {hs}]


(* SetReplace`HypergraphToGraph[g, "StructurePreserving"] *)


HypergraphToGraph[hg_ ? HypergraphQ, opts : OptionsPattern[]] := Graph[
    VertexList[hg],
    Catenate @ MapThread[
        {edge, symm} |-> Catenate[DirectedEdge @@@ Partition[Permute[edge, #], 2, 1] & /@ GroupElements[PermutationGroup[symm]]],
        {EdgeList[hg], EdgeSymmetry[hg]}
    ],
    opts
]

OrderedHypergraphToGraph[hg_ ? HypergraphQ, opts : OptionsPattern[]] := TransitiveReductionGraph @ Graph[
    {"Vertex", #} & /@ VertexList[hg],
    Catenate @ MapIndexed[{edge, i} |->
        With[{edges = {"Hyperedge", i[[1]], #} & /@ edge},
            Join[DirectedEdge @@@ Subsets[edges, {2}], # -> {"Vertex", #[[3]]} & /@ edges]
        ],
        EdgeList[hg]
    ],
    opts
]

OrderedHypergraphToGraph[args___] := Enclose @ OrderedHypergraphToGraph[ConfirmBy[Hypergraph[args], HypergraphQ]]

