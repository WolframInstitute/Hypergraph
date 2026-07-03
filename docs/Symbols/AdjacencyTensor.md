---
Template: Symbol
Name: AdjacencyTensor
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/AdjacencyTensor
Keywords: [adjacency tensor, hypergraph adjacency, sparse array, tensor representation, hyperedge]
SeeAlso: [AdjacencyHypergraph, HypergraphIncidenceMatrix, HypergraphTransitionMatrix, Hypermatrix, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[AdjacencyTensor]()[*hg*]</code> gives the adjacency tensor of the hypergraph *hg*.

<code>[AdjacencyTensor]()[*vertices*, *edges*]</code> gives the adjacency tensor of the hypergraph with the given *vertices* and *edges*.

## Details & Options

- For a hypergraph with $n$ vertices and maximal hyperedge arity $r$, [AdjacencyTensor]() returns a [SparseArray]() of rank $r$ with every dimension equal to $n+1$.
- A hyperedge listing the vertices at positions $i_1, \ldots, i_k$ of [VertexList]()[*hg*] contributes 1 to the tensor entry at index $\{i_1, \ldots, i_k, n+1, \ldots, n+1\}$: the extra index value $n+1$ pads hyperedges of arity less than $r$.
- Entries count multiplicities: a hyperedge occurring $m$ times gives an entry $m$.
- Vertices within each hyperedge are taken in the order written, so the tensor is not symmetrized, regardless of the hypergraph's `"EdgeSymmetry"`.
- Hyperedge tags are ignored.
- In <code>[AdjacencyTensor]()[*vertices*, *edges*]</code>, tensor indices refer to positions in the list *vertices*, which may include vertices absent from *edges*.
- [AdjacencyHypergraph]() reconstructs a hypergraph from its adjacency tensor.

## Basic Examples

Compute the adjacency tensor of a hypergraph:

```wl
tensor = AdjacencyTensor[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => SparseArray summary box: rank-3, dimensions {4, 4, 4}, 3 stored elements -->

Each hyperedge yields one nonzero entry, padded with the extra index 4 up to the maximal arity 3:

```wl
Most @ ArrayRules[tensor]
```

<!-- => {{1, 2, 4} -> 1, {1, 2, 3} -> 1, {2, 3, 4} -> 1} -->

---

For a hypergraph with only binary hyperedges the adjacency tensor is a matrix, displayed here with [MatrixForm]():

```wl
MatrixForm @ AdjacencyTensor[Hypergraph[{{1, 2}, {2, 3}, {3, 1}}]]
```

<!-- => 4x4 matrix {{0, 1, 0, 0}, {0, 0, 1, 0}, {1, 0, 0, 0}, {0, 0, 0, 0}} -->

## Scope

Vertices can be arbitrary expressions; tensor indices refer to positions in the vertex list:

```wl
Most @ ArrayRules @ AdjacencyTensor[Hypergraph[{{"a", "b"}, {"b", "c", "a"}}]]
```

<!-- => {{1, 2, 4} -> 1, {2, 3, 1} -> 1} -->

---

An explicit vertex list determines the tensor dimensions, so vertices occurring in no hyperedge still contribute:

```wl
Dimensions @ AdjacencyTensor[{1, 2, 3, 4}, {{1, 2}, {2, 3}}]
```

<!-- => {5, 5} -->

---

Repeated hyperedges are counted:

```wl
Most @ ArrayRules @ AdjacencyTensor[Hypergraph[{{1, 2}, {1, 2}, {2, 3}}]]
```

<!-- => {{1, 2} -> 2, {2, 3} -> 1} -->

## Properties and Relations

[AdjacencyHypergraph]() reconstructs the hypergraph from its adjacency tensor, up to the order of hyperedges:

```wl
EdgeList @ AdjacencyHypergraph[{1, 2, 3}, AdjacencyTensor[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]]
```

<!-- => {{1, 2}, {1, 2, 3}, {2, 3}} -->

---

For a hypergraph with only binary hyperedges, dropping the padding row and column gives the adjacency matrix of the corresponding directed graph:

```wl
Normal[AdjacencyTensor[Hypergraph[{{1, 2}, {2, 3}, {3, 1}}]]][[;; 3, ;; 3]] == Normal[AdjacencyMatrix[Graph[{1 -> 2, 2 -> 3, 3 -> 1}]]]
```

<!-- => True -->

## Possible Issues

A hypergraph with no hyperedges gives an empty list rather than a [SparseArray]():

```wl
AdjacencyTensor[Hypergraph[{1, 2}, {}]]
```

<!-- => {} -->
