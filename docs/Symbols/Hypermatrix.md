---
Template: Symbol
Name: Hypermatrix
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/Hypermatrix
Keywords: [hypermatrix, adjacency arrays, graded tensor, edge symmetry, sparse array]
SeeAlso: [HypermatrixGraph, AdjacencyTensor, HypergraphIncidenceMatrix, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[Hypermatrix]()[*hg*]</code> gives the hypermatrix of the hypergraph *hg*, a graded collection of adjacency arrays with one rank-$k$ array per group of arity-$k$ hyperedges.

<code>[Hypermatrix]()[*vertices*, *edges*, *symmetries*]</code> gives the hypermatrix of the hypergraph with the given *vertices*, *edges* and per-edge *symmetries*.

## Details & Options

- [Hypermatrix]() returns a `Hypermatrix` object, which displays as a summary box showing the dimensions of its component arrays; `HypermatrixQ` tests for a valid `Hypermatrix` object.
- Hyperedges are grouped by their arity and edge symmetry: each group of arity-$k$ hyperedges becomes a [SparseArray]() of rank $k$ whose dimensions all equal the number of vertices $n$, keyed by the pair of $k$ and the symmetry, given as a list of [Cycles]() permutations.
- Array entries count hyperedge multiplicities, with indices referring to positions in [VertexList]()[*hg*].
- Arity-0 hyperedges are stored as their count instead of an array.
- Unlike [AdjacencyTensor](), which embeds all hyperedges in a single padded tensor of the maximal rank, a hypermatrix keeps one unpadded array per arity and symmetry.
- In <code>[Hypermatrix]()[*vertices*, *edges*, *symmetries*]</code>, *symmetries* is a list of the same length as *edges* whose elements are lists of [Cycles]() generators, one per edge; tagged edges *edge* -> *tag* are accepted, with the tag ignored.
- For a hypermatrix *hm*, <code>*hm*["Arrays"]</code> gives the list of component arrays, <code>*hm*["Dimensions"]</code> their dimensions, and <code>*hm*["Association"]</code> an association from the arity-symmetry keys to the arrays.
- In [TraditionalForm](), a hypermatrix displays its component arrays explicitly.
- [HypermatrixGraph]() reconstructs a hypergraph from its hypermatrix.

## Basic Examples

Compute the hypermatrix of a hypergraph with two binary hyperedges and one ternary hyperedge:

```wl
hm = Hypermatrix[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => Hypermatrix summary box, Dimensions: {3, 3}; {3, 3, 3} -->

There is one component array per hyperedge arity:

```wl
hm["Dimensions"]
```

<!-- => {{3, 3}, {3, 3, 3}} -->

The binary hyperedges are collected in a matrix, displayed with [MatrixForm]():

```wl
MatrixForm @ First @ hm["Arrays"]
```

<!-- => MatrixForm of {{0, 1, 0}, {0, 0, 1}, {0, 0, 0}} -->

## Scope

Component arrays are keyed by arity together with the edge symmetry; for an ordered hypergraph each symmetry is the trivial group:

```wl
Keys @ Hypermatrix[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}, "EdgeSymmetry" -> "Ordered"]]["Association"]
```

<!-- => {{2, {Cycles[{}]}}, {3, {Cycles[{}]}}} -->

---

Hyperedges of the same arity but different symmetry are kept in separate component arrays:

```wl
Hypermatrix[Hypergraph[{{1, 2}, {2, 3}}, "EdgeSymmetry" -> {{1, 2} -> "Ordered"}]]["Dimensions"]
```

<!-- => {{3, 3}, {3, 3}} -->

---

Arity-0 hyperedges are stored as their count:

```wl
Normal /@ Hypermatrix[Hypergraph[{{}, {}, {1, 2}}]]["Arrays"]
```

<!-- => {2, {{0, 1}, {0, 0}}} -->

---

Construct a hypermatrix directly from vertices, edges and per-edge symmetry generators:

```wl
Hypermatrix[{1, 2, 3}, {{1, 2}, {2, 3}, {1, 2, 3}}, Table[{Cycles[{}]}, 3]]["Dimensions"]
```

<!-- => {{3, 3}, {3, 3, 3}} -->

## Properties and Relations

Repeated hyperedges are counted in the array entries:

```wl
Normal /@ Hypermatrix[Hypergraph[{{1, 2}, {1, 2}, {2, 3}}]]["Arrays"]
```

<!-- => {{{0, 2, 0}, {0, 0, 1}, {0, 0, 0}}} -->

---

[HypermatrixGraph]() reconstructs the hypergraph from its hypermatrix:

```wl
EdgeList @ HypermatrixGraph[Hypermatrix[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]]
```

<!-- => {{1, 2}, {2, 3}, {1, 2, 3}} -->
