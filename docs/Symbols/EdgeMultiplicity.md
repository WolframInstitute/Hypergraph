---
Template: Symbol
Name: EdgeMultiplicity
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/EdgeMultiplicity
Keywords: [multi-hypergraph, repeated edges, edge counts, multiplicity]
SeeAlso: [Hypergraph, SimpleHypergraphQ, EdgeSymmetry, EdgeListTagged]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[EdgeMultiplicity]()[*hg*]</code> gives an [Association]() from each distinct hyperedge of the hypergraph *hg* to the number of times it occurs.

## Details & Options

- Two hyperedges that differ only by a permutation allowed by their `"EdgeSymmetry"` are counted as the same hyperedge, keyed by the canonical representative among the equivalent orderings.
- The sum of the values equals <code>[EdgeCount]()[*hg*]</code>.
- A hypergraph is simple, in the sense of [SimpleHypergraphQ](), exactly when every value in <code>[EdgeMultiplicity]()[*hg*]</code> is 1.

## Basic Examples

Count the occurrences of each distinct hyperedge:

```wl
Normal @ EdgeMultiplicity[Hypergraph[{{1, 2}, {1, 2}, {2, 3}}]]
```

<!-- => {{1, 2} -> 2, {2, 3} -> 1} -->

## Scope

With the default `"Unordered"` symmetry, `{1, 2}` and `{2, 1}` are the same hyperedge, so they are counted together:

```wl
Normal @ EdgeMultiplicity[Hypergraph[{{1, 2}, {2, 1}}]]
```

<!-- => {{1, 2} -> 2} -->

---

With `"Ordered"` symmetry the two vertex orders are distinct hyperedges, each occurring once:

```wl
Normal @ EdgeMultiplicity[Hypergraph[{{1, 2}, {2, 1}}, "EdgeSymmetry" -> "Ordered"]]
```

<!-- => {{1, 2} -> 1, {2, 1} -> 1} -->

## Properties and Relations

The total multiplicity equals the number of hyperedges:

```wl
hg = Hypergraph[{{1, 2}, {1, 2}, {2, 3}}];
Total[Values[EdgeMultiplicity[hg]]] == EdgeCount[hg]
```

<!-- => True -->

---

[SimpleHypergraphQ]() is `True` exactly when every multiplicity is 1:

```wl
hg = Hypergraph[{{1, 2}, {1, 2}, {2, 3}}];
SimpleHypergraphQ[hg] === AllTrue[Values[EdgeMultiplicity[hg]], # === 1 &]
```

<!-- => True -->
