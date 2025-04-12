(* ::Package:: *)

Package["WolframInstitute`Hypergraph`"]

PackageExport["InsertionBracket"]

Options[InsertionBracket]={"IsomorphismClass"->"Ordered"}
InsertionBracket[x___,OptionsPattern[]]:=
	Switch[
		OptionValue["IsomorphismClass"],
		"Ordered",
		VertexOrderedInsertionBracket[x],
		"Rooted",
		RootedInsertionBracket[x]
		(* TODO: "Tagged" *)
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

KoszulSign[permutation_, OptionsPattern[]] := Module[
   {deg = OptionValue["Degree"],
    par = OptionValue["Parity"],
    n = Length[permutation],
    res = 1,
    i, j},
   For[i = 1, i < n, i++,
      For[j = i + 1, j <= n, j++,
         If[
            permutation[[i]] > permutation[[j]],
            res = res * (-1)^(par + deg[i] * deg[j])
         ]
      ]
   ];
   Return[res];
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
   Graded vertex-insertion bracket.
*)

VertexOrderedInsertionBracket =
   MultilinearExtension[
      Symmetrization[
         Composition[
            collectHypergraphsBy[CanonicalHypergraphVertexOrdered],
            VertexOrderedInsertion
         ],
         "Degree" -> ShiftedVertexDegree,
         "Parity" -> 1
      ]
   ];
   

(* 
   Merges values whose keys agree after applying `f`.
*)

collectHypergraphsBy[f_] :=
   Module[
      {F},
      F[hg_Hypergraph] := <|f[hg] -> 1|>;
      F[rl_Rule] := <|f[rl[[1]]] -> rl[[2]]|>;
      F[lst_List] := (F /@ lst) // DeleteCases[Merge[#, Total], 0] &;
      F[as_Association] := F[Normal[as]];
      F
   ];


(* 
   Suspended vertex degree. 
*)

ShiftedVertexDegree[hg_] := VertexCount[hg] - 1;


(* 
   Inserts `hg_n` into `hg_n-1`  ... into `hg_1` in all possible ways keeping track
   of the internal Koszul sign. Returns a list `{hg -> sgn,...}` of the results of the
   insertions with sign. 
*)

VertexOrderedInsertion[input__, OptionsPattern[]] :=
   With[
      {inputList = List[input]},
      Fold[
         {acc, next} |->
            Map[
               prev |-> With[
                  {vc = VertexCount[prev[[1]]]},
                  MapAt[
                     ((-1)^vc*# &),
                     insertVertexOrderedHypergraphInner[prev[[1]], #, next] & /@ 
                        Range[vc],
                     {All, 2}
                  ]
               ],
               acc
            ] // Catenate,
         {First[inputList] -> 1},
         Rest[inputList]
      ]
   ];


(*  
   Inserts `hg2` into `hg1` at a chosen vertex of index `i` and computes the 
   Koszul sign of this insertion. Returns `hg->sgn`. 
*)

insertVertexOrderedHypergraphInner[hg1_Hypergraph, i_, hg2_Hypergraph] :=
   Module[
      {
         vertices1 = VertexList[hg1],
         vertices2 = VertexList[hg2],
         vc1, vc2,
         edges1 = EdgeList[hg1],
         edges2 = EdgeList[hg2],
         newVertices2
      },
      vc1 = Length[vertices1];
      vc2 = Length[vertices2];
      newVertices2 = Table[Unique["ins"], Length[vertices2]];
      Hypergraph[
         Catenate[{vertices1[[1 ;; i - 1]], newVertices2, vertices1[[i + 1 ;; -1]]}],
         Catenate[{Replace[edges1, vertices1[[i]] -> newVertices2[[1]], 2],
            Replace[edges2, Thread[vertices2 -> newVertices2], 2]}]
      ] -> (-1)^(i - 1 + (vc1 - i)*vc2)
   ];


(* 
   Transforms `hg` into its canonical form as a vertex-ordered hypergraph.
   That is, it renames vertices of `hg` to {1,2,...} in the given order.
*)

CanonicalHypergraphVertexOrdered[hg_Hypergraph] :=
   With[
      {ren = Thread[VertexList[hg] -> Range[VertexCount[hg]]]},
      Hypergraph[
         ren[[All, 2]],
         Map[
            Sort[Replace[#, ren, 2]] &,
            EdgeList[hg]
         ] // Sort
      ]
   ];


(* 
   Insertion Lie bracket on isomorphism classes of rooted hypergraphs.
*)

RootedInsertionBracket =
   With[
      {rootTag = Unique["root"]},
      MultilinearExtension[
         Symmetrization[
            Composition[
               collectHypergraphsBy[CanonicalHypergraphRooted],
               MapAt[Abs, #, {All, 2}] &,
               VertexOrderedInsertion
            ],
            "Parity" -> 1
         ]
      ]
   ];


(* 
   Returns the canonical form of a rooted hypergraph. That is, the
   canonical form of a hypergraph, which has always vertices `{1,2,...}`,
   with the leading `1` swapped with the root vertex `i`.
*)

CanonicalHypergraphRooted = With[
   {tag = Unique["root"]},
   Composition[
      (convertTaggedUnaryEdgeToRoot[#, tag] &),
      CanonicalHypergraph,
      (convertRootToTaggedUnaryEdge[#, tag] &)
   ]
];


(*
	Transforms a root vertex into a tagged unary edge.
	Two rooted hypergraphs are isomorphic if and only if the
	transformed hypergraphs are isomorphic as hypergraphs.
*)

convertRootToTaggedUnaryEdge[hg_, tag_] :=
   Hypergraph[
      VertexList[hg], 
      Join[EdgeList[hg], {{getRoot[hg]} -> tag}]
   ];
  
    
(*
	Transforms a tagged unary edge into a root vertex.
*)

convertTaggedUnaryEdgeToRoot[hg_, tag_] :=
   makeRoot[
      Hypergraph[
         VertexList[hg], 
         DeleteCases[EdgeListTagged[hg], _ -> tag, {1}]
      ],
      First[FirstCase[EdgeListTagged[hg], (x_ -> tag) :> x]]
   ];


(* 
   Makes a given vertex `v` into the root of `hg` by swapping it with
   the first vertex in the list of vertices.
*)

makeRoot[hg_Hypergraph, v_] :=
   Hypergraph[
      Replace[#, {First[#] -> v, v -> First[#]}, {1}] &@VertexList[hg],
      EdgeList[hg]
   ];


(* 
   Returns the root of a hypergraph.
*)

getRoot[hg_Hypergraph] := First[VertexList[hg]];
