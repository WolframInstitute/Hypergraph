---
Template: Symbol
Name: IsomorphicHypergraphQ
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/IsomorphicHypergraphQ
Keywords: [hypergraph isomorphism, isomorphism test, vertex relabeling, canonical form]
SeeAlso: [CanonicalHypergraph, Hypergraph, EnumerateHypergraphs, RandomHypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[IsomorphicHypergraphQ]()[*hg1*, *hg2*]</code> gives True if the hypergraphs *hg1* and *hg2* are isomorphic, and False otherwise.

## Details & Options

- Two hypergraphs are isomorphic when [CanonicalHypergraph]() gives the same vertex list, edge list, and edge symmetry for both, computed with the default canonicalization method.
- [IsomorphicHypergraphQ]() takes no options; it always uses the default `Method` of [CanonicalHypergraph]().
- Vertex and edge annotations, including styles, do not affect the isomorphism test.
- The number of vertices must agree, so isolated (edge-free) vertices are counted.
- Repeated hyperedges are counted with multiplicity.

## Basic Examples

Two hypergraphs related by a relabeling of vertices are isomorphic:

```wl
IsomorphicHypergraphQ[Hypergraph[{{1, 2}, {2, 3}}], Hypergraph[{{a, b}, {b, c}}]]
```

<!-- => True -->

---

Hypergraphs with different numbers of hyperedges are not isomorphic:

```wl
IsomorphicHypergraphQ[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => False -->

## Scope

For the default `"Unordered"` edge symmetry, reversing the vertices within a hyperedge does not change the isomorphism class:

```wl
IsomorphicHypergraphQ[Hypergraph[{{1, 2}}], Hypergraph[{{2, 1}}]]
```

<!-- => True -->

---

With `"EdgeSymmetry" -> "Ordered"`, the order of vertices within each hyperedge matters, so an "out-star" and an "in-star" on the same number of vertices are not isomorphic:

```wl
IsomorphicHypergraphQ[Hypergraph[{{1, 2}, {1, 3}}, "EdgeSymmetry" -> "Ordered"], Hypergraph[{{2, 1}, {3, 1}}, "EdgeSymmetry" -> "Ordered"]]
```

<!-- => False -->

---

The same two edge sets are isomorphic once the ordering constraint is dropped:

```wl
IsomorphicHypergraphQ[Hypergraph[{{1, 2}, {1, 3}}], Hypergraph[{{2, 1}, {3, 1}}]]
```

<!-- => True -->

---

Isolated vertices are counted, so a hypergraph with an extra edge-free vertex is not isomorphic to one without it:

```wl
IsomorphicHypergraphQ[Hypergraph[{1, 2, 3}, {{1, 2}}], Hypergraph[{a, b}, {{a, b}}]]
```

<!-- => False -->

## Properties and Relations

[IsomorphicHypergraphQ]() agrees with directly comparing canonical forms:

```wl
With[{hg1 = Hypergraph[{{1, 2}, {2, 3}, {3, 1}}], hg2 = Hypergraph[{{x, y}, {y, z}, {z, x}}]},
    IsomorphicHypergraphQ[hg1, hg2] === (CanonicalHypergraph[hg1] === CanonicalHypergraph[hg2])
]
```

<!-- => True -->

---

Vertex styling does not affect the isomorphism test:

```wl
IsomorphicHypergraphQ[Hypergraph[{{1, 2}}, VertexStyle -> {1 -> Red}], Hypergraph[{{a, b}}]]
```

<!-- => True -->

## Possible Issues

[IsomorphicHypergraphQ]() does not accept a `Method` option the way [CanonicalHypergraph]() does; passing one leaves the call unevaluated, since no such definition exists:

```wl
IsomorphicHypergraphQ[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}}], Method -> "Combinatorial"]
```

<!-- => unevaluated, since IsomorphicHypergraphQ has no three-argument definition -->
