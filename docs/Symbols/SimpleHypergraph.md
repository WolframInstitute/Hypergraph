---
Template: Symbol
Name: SimpleHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/SimpleHypergraph
Keywords: [simple hypergraph, duplicate edges, self loops, edge symmetry]
SeeAlso: [SimpleHypergraphQ, EdgeSymmetry, EdgeMultiplicity, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[SimpleHypergraph]()[*hg*]</code> gives the hypergraph obtained from *hg* by removing hyperedges with a repeated vertex and merging hyperedges that agree up to their edge symmetry.

<code>[SimpleHypergraph]()[*args*]</code> gives <code>[SimpleHypergraph]()[[Hypergraph]()[*args*]]</code>.

## Details & Options

- Two hyperedges that are permutations of one another under their shared [EdgeSymmetry]() are merged into one; for example two `"Unordered"` copies of the same vertex set collapse to a single hyperedge.
- A hyperedge with a repeated vertex, such as a self-loop `{v, v}`, is dropped entirely rather than merged.
- The vertex list of *hg*, including any isolated vertices, is unchanged.
- [SimpleHypergraphQ]() tests whether a hypergraph already equals its own [SimpleHypergraph]().

## Basic Examples

Duplicate copies of the same hyperedge are merged into one:

```wl
EdgeList[SimpleHypergraph[Hypergraph[{{1, 2}, {1, 2}, {2, 3}}]]]
```

<!-- => {{1, 2}, {2, 3}} -->

---

An edge with a repeated vertex is removed:

```wl
EdgeList[SimpleHypergraph[Hypergraph[{{1, 1}, {1, 2}}]]]
```

<!-- => {{1, 2}} -->

## Scope

Since edges are `"Unordered"` by default, a pair listed in either order is the same hyperedge:

```wl
EdgeList[SimpleHypergraph[Hypergraph[{{1, 2}, {2, 1}}]]]
```

<!-- => {{1, 2}} -->

---

Two cyclic rotations of the same ternary hyperedge are merged, since they belong to the same `"Cyclic"` symmetry orbit:

```wl
EdgeList[SimpleHypergraph[Hypergraph[{CyclicEdge[{1, 2, 3}], CyclicEdge[{2, 3, 1}]}]]]
```

<!-- => {{1, 2, 3}} -->

---

<code>[SimpleHypergraph]()[*args*]</code> accepts a raw edge list directly, constructing the hypergraph first:

```wl
EdgeList[SimpleHypergraph[{{1, 2}, {2, 1}}]]
```

<!-- => {{1, 2}} -->

---

Isolated vertices are kept, even ones left isolated only after edges are merged away:

```wl
VertexList[SimpleHypergraph[Hypergraph[{1, 2, 3, 4}, {{1, 2}, {2, 1}}]]]
```

<!-- => {1, 2, 3, 4} -->

## Properties and Relations

[SimpleHypergraphQ]() is true exactly when [SimpleHypergraph]() changes nothing:

```wl
SimpleHypergraphQ[Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => True -->

---

A hypergraph with a duplicate hyperedge is not simple:

```wl
SimpleHypergraphQ[Hypergraph[{{1, 2}, {1, 2}}]]
```

<!-- => False -->

---

A hypergraph with a self-loop is not simple either:

```wl
SimpleHypergraphQ[Hypergraph[{{1, 1}, {1, 2}}]]
```

<!-- => False -->
