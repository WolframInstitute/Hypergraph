---
Template: Symbol
Name: AdjacencyHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/AdjacencyHypergraph
Keywords: [adjacency tensor, hypergraph construction, sparse array, tensor representation]
SeeAlso: [AdjacencyTensor, IncidenceHypergraph, HypermatrixGraph, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[AdjacencyHypergraph]()[*tensor*]</code> constructs the hypergraph whose adjacency tensor is *tensor*.

<code>[AdjacencyHypergraph]()[*vertices*, *tensor*]</code> uses the given list of *vertices* for the tensor indices.

## Details & Options

- *tensor* must be a cubical array of some rank $r$ whose common dimension is one more than the number of vertices; for other dimensions [AdjacencyHypergraph]() stays unevaluated.
- An index value $i$ with $1 \le i \le n$ refers to the $i$-th element of *vertices*; the extra index value $n+1$ is a padding index marking hyperedges of arity less than $r$.
- A nonzero entry $m$ at index $\{i_1, \ldots, i_k, n+1, \ldots, n+1\}$ yields $m$ copies of the hyperedge listing the vertices at positions $i_1, \ldots, i_k$.
- <code>[AdjacencyHypergraph]()[*tensor*]</code> uses <code>[Range]()[*n*]</code> as the vertices, where $n+1$ is the dimension of *tensor*.
- [AdjacencyHypergraph]() inverts [AdjacencyTensor]().

## Basic Examples

Construct a hypergraph from a rank-3 adjacency tensor:

```wl
hg = AdjacencyHypergraph[SparseArray[{{1, 2, 4} -> 1, {2, 3, 4} -> 1, {1, 2, 3} -> 1}, {4, 4, 4}]]
```

<!-- => Hypergraph rendering with hyperedges {1, 2}, {1, 2, 3}, {2, 3} -->

The padding index 4 terminates the two binary hyperedges:

```wl
EdgeList[hg]
```

<!-- => {{1, 2}, {1, 2, 3}, {2, 3}} -->

## Scope

An explicit vertex list names the tensor indices:

```wl
EdgeList @ AdjacencyHypergraph[{"a", "b", "c"}, SparseArray[{{1, 2} -> 1, {2, 3} -> 1}, {4, 4}]]
```

<!-- => {{"a", "b"}, {"b", "c"}} -->

---

An entry $m$ produces $m$ copies of the corresponding hyperedge:

```wl
EdgeList @ AdjacencyHypergraph[SparseArray[{{1, 2} -> 2}, {3, 3}]]
```

<!-- => {{1, 2}, {1, 2}} -->

## Properties and Relations

Take the adjacency tensor of a hypergraph:

```wl
tensor = AdjacencyTensor[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => SparseArray summary box: rank-3, dimensions {4, 4, 4}, 3 stored elements -->

[AdjacencyHypergraph]() recovers the original hypergraph, up to the order of hyperedges:

```wl
Sort @ EdgeList @ AdjacencyHypergraph[{1, 2, 3}, tensor] == Sort @ EdgeList @ Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]
```

<!-- => True -->

## Possible Issues

The tensor must be cubical with each dimension one more than the number of vertices; otherwise the expression stays unevaluated:

```wl
AdjacencyHypergraph[{1, 2, 3}, SparseArray[{{1, 2} -> 1}, {3, 3}]]
```

<!-- => AdjacencyHypergraph[{1, 2, 3}, SparseArray[...]] (unevaluated) -->

---

Vertices occurring in no hyperedge are not recovered in the round trip through [AdjacencyTensor]():

```wl
VertexList @ AdjacencyHypergraph[{1, 2, 3, 4}, AdjacencyTensor[Hypergraph[{1, 2, 3, 4}, {{1, 2}, {2, 3}}]]]
```

<!-- => {1, 2, 3} -->
