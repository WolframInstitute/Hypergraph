---
Template: Symbol
Name: ToOrderedHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/ToOrderedHypergraph
Keywords: [edge symmetry, ordered hypergraph, symmetry expansion, hypergraph conversion]
SeeAlso: [OrderedHypergraphToGraph, EdgeSymmetry, CanonicalHypergraph, HypergraphToGraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[ToOrderedHypergraph]()[*hg*]</code> gives a hypergraph with `"Ordered"` edge symmetry, obtained by expanding every hyperedge of *hg* over its full edge symmetry group.

## Details & Options

- Each hyperedge of *hg* is replaced by one copy per element of its [EdgeSymmetry]() group, with the vertices permuted accordingly; the result's `"EdgeSymmetry"` is `"Ordered"` throughout, so no further symmetrization applies to it.
- A hyperedge already given with `"Ordered"` symmetry contributes only its single, unpermuted copy.
- A tag on a hyperedge is copied onto every one of its expanded copies.
- Vertex and edge annotations (styles, labels, and other options of *hg*) are preserved.
- [ToOrderedHypergraph]() requires an actual [Hypergraph]() object; it does not accept a raw edge-spec list.

## Basic Examples

Expand the default `"Unordered"` symmetry of a pair into both of its orderings:

```wl
EdgeList[ToOrderedHypergraph[Hypergraph[{{1, 2}}]]]
```

<!-- => {{1, 2}, {2, 1}} -->

---

The result's `"EdgeSymmetry"` option is `"Ordered"` for every edge:

```wl
Union[Lookup[Options[ToOrderedHypergraph[Hypergraph[{{1, 2}}]]], "EdgeSymmetry"][[All, 2]]]
```

<!-- => {"Ordered"} -->

## Scope

An `"Unordered"` ternary hyperedge expands into all 6 permutations of its vertices:

```wl
EdgeList[ToOrderedHypergraph[Hypergraph[{{1, 2, 3}}]]]
```

<!-- => {{1, 2, 3}, {1, 3, 2}, {2, 1, 3}, {3, 1, 2}, {2, 3, 1}, {3, 2, 1}} -->

---

A [CyclicEdge]() expands into only its 3 rotations, not all 6 permutations:

```wl
EdgeList[ToOrderedHypergraph[Hypergraph[{CyclicEdge[{1, 2, 3}]}]]]
```

<!-- => {{1, 2, 3}, {3, 1, 2}, {2, 3, 1}} -->

---

A hyperedge already `"Ordered"` is left with a single, unpermuted copy:

```wl
EdgeList[ToOrderedHypergraph[Hypergraph[{DirectedEdge[1, 2], DirectedEdge[2, 3]}]]] === {{1, 2}, {2, 3}}
```

<!-- => True -->

---

Vertex styling carried by *hg* is preserved:

```wl
Lookup[Options[ToOrderedHypergraph[Hypergraph[{{a, b}}, VertexStyle -> {a -> Red}]]], VertexStyle]
```

<!-- => {a -> RGBColor[1, 0, 0]} -->

## Properties and Relations

Every hyperedge of the result has trivial, `"Ordered"` symmetry, so applying [ToOrderedHypergraph]() again changes nothing:

```wl
hg = ToOrderedHypergraph[Hypergraph[{{1, 2, 3}}]];
EdgeList[ToOrderedHypergraph[hg]] === EdgeList[hg]
```

<!-- => True -->

---

[ToOrderedHypergraph]() and [OrderedHypergraphToGraph]() play complementary roles in [CanonicalHypergraph](): expanding over the full symmetry group before building the order-sensitive graph encoding is what makes the default `Method -> "Graph"` independent of which symmetric representative a hyperedge was written with:

```wl
IsomorphicHypergraphQ[CanonicalHypergraph[Hypergraph[{{1, 2, 3}}]], CanonicalHypergraph[Hypergraph[{{1, 3, 2}}]]]
```

<!-- => True -->
