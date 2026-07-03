---
Template: Symbol
Name: HypermatrixGraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypermatrixGraph
Keywords: [hypermatrix, hypergraph construction, adjacency arrays, edge symmetry]
SeeAlso: [Hypermatrix, AdjacencyHypergraph, IncidenceHypergraph, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypermatrixGraph]()[*hm*]</code> constructs the hypergraph corresponding to the hypermatrix *hm*.

<code>[HypermatrixGraph]()[*assoc*]</code> constructs the hypergraph from an association whose keys are arity-symmetry pairs and whose values are adjacency arrays.

## Details & Options

- The resulting hypergraph has vertices <code>[Range]()[*n*]</code>, where $n$ is the common dimension of the component arrays.
- A positive integer entry $m$ at index $\{i_1, \ldots, i_k\}$ of a rank-$k$ component array yields $m$ copies of the hyperedge $\{i_1, \ldots, i_k\}$.
- The symmetry part of each key, a list of [Cycles]() permutations, is applied to the resulting hyperedges through the `"EdgeSymmetry"` option of [Hypergraph]().
- In <code>[HypermatrixGraph]()[*assoc*]</code>, each key is a pair of an integer arity and a list of [Cycles]() generators, as in the `"Association"` property of a `Hypermatrix` object.
- [HypermatrixGraph]() inverts [Hypermatrix]() up to relabeling the vertices as $1, \ldots, n$ in the order of the original vertex list.

## Basic Examples

Construct a hypergraph from the hypermatrix of another hypergraph:

```wl
hg = HypermatrixGraph[Hypermatrix[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]]
```

<!-- => Hypergraph rendering with hyperedges {1, 2}, {2, 3}, {1, 2, 3} -->

The hyperedges are read off the component arrays:

```wl
EdgeList[hg]
```

<!-- => {{1, 2}, {2, 3}, {1, 2, 3}} -->

## Scope

Construct a hypergraph directly from an association of arity-symmetry keys and arrays; an entry 2 yields two copies of the hyperedge:

```wl
EdgeList @ HypermatrixGraph[<|{2, {Cycles[{}]}} -> {{0, 2}, {0, 0}}|>]
```

<!-- => {{1, 2}, {1, 2}} -->

## Properties and Relations

[HypermatrixGraph]() inverts [Hypermatrix]() up to vertex relabeling; for a hypergraph with vertices other than $1, \ldots, n$ the round trip gives an isomorphic hypergraph:

```wl
IsomorphicHypergraphQ[Hypergraph[{{"a", "b"}, {"b", "c", "a"}}], HypermatrixGraph[Hypermatrix[Hypergraph[{{"a", "b"}, {"b", "c", "a"}}]]]]
```

<!-- => True -->

---

The edge symmetry of the original hypergraph is preserved through the round trip:

```wl
EdgeSymmetry @ HypermatrixGraph[Hypermatrix[Hypergraph[{{1, 2}, {2, 3}}, "EdgeSymmetry" -> "Unordered"]]]
```

<!-- => {{Cycles[{}], Cycles[{{1, 2}}]}, {Cycles[{}], Cycles[{{1, 2}}]}} -->

---

Arity-0 hyperedges, stored in a hypermatrix as a count, are reconstructed:

```wl
EdgeList @ HypermatrixGraph[Hypermatrix[Hypergraph[{{}, {1, 2}}]]]
```

<!-- => {{}, {1, 2}} -->
