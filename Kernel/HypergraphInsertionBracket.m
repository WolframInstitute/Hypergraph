(* ::Package:: *)

Package["WolframInstitute`Hypergraph`"]

PackageExport["HypergraphInsertionBracket"]
PackageExport["HypergraphInsertionBracketDegree"]

(* TODO
  1. Support isomorphism classes of rooted graded hypergraphs:
       - make CanonicalHypergraph return the vertex transformation so that its Koszul sign can be computed
       - add Koszul sign in convertTaggedUnaryEdgeToRoot (perhaps replace the transposition by cyclic permutation)
*)

Options[HypergraphInsertionBracket]={"Kind"->"Ordered"}
HypergraphInsertionBracket[x___,OptionsPattern[]]:=
	Switch[
		OptionValue["Kind"],
		"Ordered",
		InsertionBracketOrdered[x],
		"Rooted",
		InsertionBracketRooted[x],
		"Tagged",
		InsertionBracketTagged[x]
	];

(*
   Extends `op` to a multi-linear operation on associations.
   The output `out` of `op` is assumed to be an association and is
   treated as the trivial association `<| out -> 1 |>` otherwise.
*)
MultilinearExtension[op_] := Module[
   {opMulti},
   opMulti[first___, x_, last___] /; ! AssociationQ[x] := 
      opMulti[first, <|x -> 1|>, last];
   opMulti[inputs__Association] :=
      Map[
         tuple |-> (Times @@ (Last /@ tuple)) *
            With[
               {res = (op @@ (First /@ tuple))},
               If[AssociationQ[res], res, <|res -> 1|>]
            ],
         Tuples[Normal /@ List[inputs]]
      ] // DeleteCases[Merge[#, Total], 0] &;
   opMulti
];


(*
   Computes the Koszul sign of a given permutation.
*)
Options[KoszulSign] = {"Degree" -> Function[x, 0], "Parity" -> 0};

KoszulSign[permutation_, OptionsPattern[]] := Module[{
      deg = OptionValue["Degree"],
      par = OptionValue["Parity"],
      n = Length[permutation],
      factors
   },
   factors = Table[ If[permutation[[i]] > permutation[[j]],
      (-1)^(par + deg[i]*deg[j]), 1 ],
      {i, 1, n - 1}, {j, i + 1, n} ];
   Times @@ Flatten @ factors
];


(*
   Symmetrizes `op` in its inputs. The output `out` of `op` is 
   assumed to be an association and is treated as the trivial
   association `<| out -> 1 |>` otherwise.
*)
Options[Symmetrization] = {"Parity" -> 0, "Degree" -> Function[x, 0]};

Symmetrization[op_, OptionsPattern[]] := 
   Module[
      {opSym,
       deg = OptionValue["Degree"],
       par = OptionValue["Parity"]},
      opSym[input___] := With[
         {inputList = List[input]},
         Map[
            perm |-> 
               KoszulSign[perm, "Parity" -> par, 
                  "Degree" -> (i |-> deg[inputList[[i]]])] *
               With[
                  {res = op @@ (inputList[[#]] & /@ perm)},
                  If[AssociationQ[res], res, <|res -> 1|>]
               ],
            Permutations[Range[Length[inputList]]]
         ]
      ] // DeleteCases[Merge[#, Total], 0] &;
      opSym
   ];


(*
   Graded insertion bracket on Hypergraphs with ordered graded vertices.
*)
InsertionBracketOrdered =
   MultilinearExtension[
      Symmetrization[
         Composition[
            CollectHypergraphsBy[CanonicalHypergraphVertexOrdered],
            HypergraphInsertion
         ],
         "Degree" -> HypergraphInsertionBracketDegree,
         (* antisymmetrization *)
         "Parity" -> 1
      ]
   ];
   

(*
   Merges values whose keys agree after applying `f`.
*)
CollectHypergraphsBy[f_] := DeleteCases[0] @* Merge[Total] @* Map[f];


(*
   |G| = deg(G) - deg(root)
*)
HypergraphInsertionBracketDegree[hg_Hypergraph] := With[{ degree = HypergraphVertexDegree[hg] },
   Total[Values[degree]] - Lookup[degree, First @ VertexList[hg], 0]
];


(* 
   Inserts `hg[n]` into `hg[n-1]` into `hg[n-2]` and so on by root in all possible ways aggregating the insertion sign.
   Returns a list `{ <| hg -> sgn |>,...}` of all insertion results. 
*)
HypergraphInsertion[input__Hypergraph] := Fold[
   { toInsertList, target } |-> With[{
      targetVertices = VertexList[target],
      targetDegrees = HypergraphVertexDegree[target] },
      (* Insert each previously obtained hypergraph to the target in all possible ways *)
      First @ KeyValueMap[ {hg, c} |-> With[{
         insertedRootDegree = Lookup[HypergraphVertexDegree[hg],getRoot[hg],0] },
	    Map[
	       (* previous and external sign *)
     	       c * If[EvenQ[Lookup[targetDegrees, targetVertices[[#]], 0] * Total[Values[targetDegrees]]], 1, -1] *
	       (* Insertion with internal sign *)
	       hypergraphInsertionInner[target, #, hg] &,
	       (* Insert only at vertices with the same degree as the root *)
               Select[ Range[Length[targetVertices]], Lookup[targetDegrees, #, 0] == insertedRootDegree & ] 
	    ]
         ], #] & /@ toInsertList // Catenate
   ],
   (* hg[n] *)
   { <| Last[List[input]] -> 1 |> },
   (* hg[n-1], hg[n-2], ..., hg[1] *)
   Reverse @ Most[List[input]]
];


(* 
   Inserts `hg2` into `hg1` at a chosen vertex of index `i` and computes the 
   Koszul sign of this insertion. Returns `<| hg->sgn |>`. 
*)
hypergraphInsertionInner[hg1_Hypergraph, i_, hg2_Hypergraph] := Module[{
      vertices1 = VertexList[hg1],
      vertices2 = VertexList[hg2],
      deg1 = HypergraphVertexDegree[hg1],
      deg2 = HypergraphVertexDegree[hg2],
      edges1 = EdgeList[hg1],
      edges2 = EdgeList[hg2],
      insertedVertices, embedding,
      resultingSign, resultingVertices, resultingEdges, resultingDegree,
      u1, u2, ui
   },
   insertedVertices = Table[Unique["ins"], Length[vertices2]];
   embedding = AssociationThread[vertices2 -> insertedVertices];
   u1 = vertices1[[1 ;; i - 1]];
   u2 = vertices1[[i + 1 ;; -1]];
   ui = vertices1[[i]];
   resultingSign = If[ EvenQ[
      Lookup[deg1, ui, 0] * Total[Lookup[deg1, u1]] + Total[Values[deg2]] * Total[Lookup[deg1, u2]]
      ], 1, -1 ];
   resultingVertices = Join[
      vertices1[[1 ;; i - 1]],
      insertedVertices,
      vertices1[[i + 1 ;; -1]]
   ];
   resultingEdges = Join[ 
      Replace[edges1, ui -> insertedVertices[[1]], {2} ],
      Map[ Lookup[ embedding, #, # ]&, edges2, {2} ]
   ];
   resultingDegree = Merge[{
      KeyTake[deg1, u1],
      KeyMap[ Lookup[ embedding, #, # ] &, deg2 ], 
      KeyTake[deg1, u2]
   }, First ];
   <| Hypergraph[
      resultingVertices,
      resultingEdges,
      "VertexAnnotationRules" -> { "Degree" -> Normal @ resultingDegree }
   ] -> resultingSign |>
];

(*
   Returns the vertex degrees of a hypergraph; defaults to 0.
*)
HypergraphVertexDegree[h_Hypergraph] := Association@Lookup[Lookup[AbsoluteOptions[h], "VertexAnnotationRules", {}], 
   "Degree", Thread[VertexList[h] -> 0]];


(*
   Transforms `<| hg->c |>` into its canonical form as a vertex-ordered hypergraph.
   That is, it renames vertices of `hg` to {1,2,...} in the given order.
*)
CanonicalHypergraphVertexOrdered := KeyMap[ hg |-> 
   Module[{ resultingVertices, resultingEdges, resultingDegree, transformation},
      resultingVertices = Range[VertexCount[hg]];
      transformation = AssociationThread[VertexList[hg], resultingVertices];
      resultingEdges = ( ( ( ( Lookup[transformation, #, #]& ) /@ # ) // Sort )& /@ EdgeList[hg]) // Sort;
      resultingDegree = KeyMap[ Lookup[transformation, #, #]& , HypergraphVertexDegree[hg]];
      Hypergraph[
         resultingVertices,
         resultingEdges,
        "VertexAnnotationRules" -> { "Degree" -> Normal @ resultingDegree }
      ]
   ]
];


(*
   Insertion Lie bracket on isomorphism classes of rooted hypergraphs.
*)
InsertionBracketRooted =
   With[
      {rootTag = Unique["root"]},
      MultilinearExtension[
         Symmetrization[
            Composition[
               CollectHypergraphsBy[CanonicalHypergraphRooted],
	       (* TODO support grading for the rooted bracket *)
               HypergraphInsertionUNGRADED
            ],
            "Parity" -> 1
         ]
      ]
   ];

HypergraphInsertionUNGRADED[input__Hypergraph] := HypergraphInsertion @@ (Hypergraph[VertexList[#],EdgeList[#]]& /@ List[input]);

(*
   Returns the canonical form of a rooted hypergraph. That is, the
   canonical form of a hypergraph, which has always vertices `{1,2,...}`,
   with the leading `1` swapped with the root vertex `i`.
*)
CanonicalHypergraphRooted = With[ {tag = Unique["root"]},
   Composition[
      convertTaggedUnaryEdgeToRoot[tag],
      KeyMap[CanonicalHypergraph],
      convertRootToTaggedUnaryEdge[tag]
   ]
];


(*
   Transforms a root vertex into a tagged unary edge.
   Two rooted hypergraphs are isomorphic if and only if the
   transformed hypergraphs are isomorphic as hypergraphs.
*)
convertRootToTaggedUnaryEdge[tag_] := KeyMap[ hg |-> 
   Hypergraph[
      VertexList[hg], 
      Join[EdgeList[hg], {{ getRoot[hg] } -> tag}]
   ]
];
  
    
(*
   Transforms a tagged unary edge into a root vertex.
*)
convertTaggedUnaryEdgeToRoot[tag_] := KeyMap[ hg |->
   Module[ { edges = EdgeListTagged[hg] , vertices = VertexList[hg], root, first}, 
      root = First @ FirstCase[ EdgeListTagged[hg], (x_ -> tag) :> x ];
      first = First[vertices];
      Hypergraph[
           Replace[vertices, { first -> root, root -> first }, {1} ], 
           DeleteCases[edges, _ -> tag, {1} ]
         ]
   ]
];

(*
   Returns the root of a hypergraph.
*)
getRoot[hg_Hypergraph] := First[VertexList[hg]];


(*
   Tagged insertion bracket.
*)
InsertionBracketTagged =
  MultilinearExtension[
   Symmetrization[
    Composition[
     CollectHypergraphsBy[CanonicalHypergraphTagged],
     EdgeInsertion
     ],
    "Parity" -> 1
    ]
   ];

(*
   Inserts `hg_n` into `hg_n-1`  ... into `hg_1` in all possible ways.
   Returns a list `{hg -> 1,...}` of the insertions.
*)
EdgeInsertion[input__Hypergraph] := Fold[
   { toInsertList, target } |->
   ( First @ KeyValueMap[ {hg, c} |-> Map[Times[c,#]&, edgeInsertionInner[target, hg] ], # ] ) & /@ toInsertList // Catenate,
   { <| Last[List[input]] -> 1 |> },
   Reverse @ Most[List[input]]
];

edgeInsertionInner[hg1_, hg2_] :=
  Module[
    {vertices1 = VertexList[hg1],
     vertices2 = VertexList[hg2],
     edges1 = EdgeList[hg1],
     edges2 = EdgeList[hg2],
     copyVertices2, makeCopy, rootEdge, rootEdgeSymmetry, 
     rootEdgeSymmetryOption, copyRootEdge, complimentaryVertices2, 
     copyComplimentaryVertices2, copyEdges2},
    copyVertices2 = Table[Unique["ins"], Length[vertices2]];
    makeCopy = Thread[vertices2 -> copyVertices2];
    rootEdge = First[edges2];
    copyRootEdge = Replace[rootEdge, makeCopy, 2];
    rootEdgeSymmetry = First[EdgeSymmetry[hg2]];
    rootEdgeSymmetryOption = First[hg2["EdgeSymmetry"]];
    complimentaryVertices2 = DeleteElements[vertices2, rootEdge];
    copyComplimentaryVertices2 =
      Replace[complimentaryVertices2, makeCopy, 1];
    copyEdges2 = Replace[edges2, makeCopy, 2];
    MapApply[
      {insertEdgeIndex, rootEdgePermutation} |->
       With[
         {transform =
           Thread[edges1[[insertEdgeIndex]] ->
             Permute[copyRootEdge, rootEdgePermutation]]},
         <| Hypergraph[
           Join[
             Replace[vertices1, transform, 1],
             copyComplimentaryVertices2
           ],
           Join[
             Replace[edges1[[1 ;; insertEdgeIndex - 1]], transform, 2],
             copyEdges2,
             Replace[edges1[[insertEdgeIndex + 1 ;; -1]], transform, 2]
           ],
           "EdgeSymmetry" -> rootEdgeSymmetryOption
         ] -> 1 |>
       ],
      Tuples[{
        Flatten@Position[edges1, x_ /; Length[x] == Length[rootEdge], 1],
        GroupElements[PermutationGroup[rootEdgeSymmetry]]
      }]
    ]
  ];


(*
   Returns the canonical form of a tagged hypergraph. That is, the
   canonical form of a hypergraph where the leading edge is swapped with
   the tagged edge. This is a canonical form since the leading edge can be
   identified as the edge with the smallest arity and vertices in the lowest
   lexicographic order.
*)
CanonicalHypergraphTagged = With[
   {tag = Unique["root"]},
   Composition[
      convertTaggedEdgeToRootEdge[tag],
      KeyMap[CanonicalHypergraph],
      convertRootEdgeToTaggedEdge[tag]
   ]
];


(*
   Tags the root edge. The root edge, by our convention, is the first 
   edge in the list of edges.
*)
convertRootEdgeToTaggedEdge[tag_] := KeyMap[ hg |-> Hypergraph[
      VertexList[hg],
      ReplaceAt[EdgeList[hg], x_ :> (x -> tag), {1}],
      "EdgeSymmetry" -> First[hg["EdgeSymmetry"]]
   ]
];

(*
   Transforms the tagged edge into the root edge.
*)
convertTaggedEdgeToRootEdge[tag_] := KeyMap[ hg |->
   With[{ edges=EdgeList[hg], pos = First[First[Position[EdgeListTagged[hg], _ -> tag]]] },
      Hypergraph[
         VertexList[hg], 
         ReplacePart[edges,{1 -> edges[[pos]],pos -> edges[[1]]}],
         "EdgeSymmetry" -> First[hg["EdgeSymmetry"]]
      ]
  ]
];
