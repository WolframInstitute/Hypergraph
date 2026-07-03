---
Template: Symbol
Name: SimpleHypergraphQ
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/SimpleHypergraphQ
Keywords: [simple hypergraph, duplicate edges, predicate]
SeeAlso: [SimpleHypergraph, Hypergraph, ConnectedHypergraphQ, EdgeMultiplicity]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[SimpleHypergraphQ]()[*hg*]</code> gives `True` if the hypergraph *hg* has no repeated hyperedges, and `False` otherwise.

## Details & Options

- [SimpleHypergraphQ]() compares the edge count of *hg* to the edge count of <code>[SimpleHypergraph]()[*hg*]</code>, which removes duplicate and non-duplicate-free hyperedges.
- Two hyperedges that differ only by a permutation allowed by their `"EdgeSymmetry"` count as the same edge, so the test depends on *hg*'s edge symmetry.
- A hyperedge that repeats a vertex, such as `{1, 1, 2}`, is never simple.

## Basic Examples

A hypergraph with no repeated hyperedges is simple:

```wl
SimpleHypergraphQ[Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => True -->

---

A hypergraph with a literally repeated hyperedge is not simple:

```wl
SimpleHypergraphQ[Hypergraph[{{1, 2}, {1, 2}}]]
```

<!-- => False -->

## Scope

With the default `"Unordered"` symmetry, `{1, 2}` and `{2, 1}` are the same hyperedge, so listing both is not simple:

```wl
SimpleHypergraphQ[Hypergraph[{{1, 2}, {2, 1}}]]
```

<!-- => False -->

---

With `"Ordered"` symmetry the same two hyperedges are distinct, so the hypergraph is simple:

```wl
SimpleHypergraphQ[Hypergraph[{{1, 2}, {2, 1}}, "EdgeSymmetry" -> "Ordered"]]
```

<!-- => True -->

---

A hyperedge that repeats a vertex is never simple:

```wl
SimpleHypergraphQ[Hypergraph[{{1, 1, 2}}]]
```

<!-- => False -->

## Properties and Relations

[SimpleHypergraph]() removes exactly the edges that make [SimpleHypergraphQ]() `False`, so its result is always simple:

```wl
SimpleHypergraphQ[SimpleHypergraph[Hypergraph[{{1, 2}, {1, 2}, {2, 3}}]]]
```

<!-- => True -->
