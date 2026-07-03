---
Template: Symbol
Name: HypergraphInsertionBracket
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphInsertionBracket
Keywords: [insertion bracket, graded Lie bracket, hypergraph operad, vertex insertion, Koszul sign]
SeeAlso: [HypergraphInsertionBracketDegree, CanonicalHypergraph, IsomorphicHypergraphQ, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphInsertionBracket]()[*hg1*, *hg2*, …]</code> gives the graded antisymmetric insertion bracket of the hypergraphs *hg1*, *hg2*, …, as an association from resulting hypergraphs to integer coefficients.

<code>[HypergraphInsertionBracket]()[*assoc1*, *hg2*, …]</code> extends the bracket linearly whenever an argument is given as an association <|*hg* -> *c*, …|>, representing a formal sum of hypergraphs.

## Details & Options

- Inserting a hypergraph *hg'* at a vertex *v* of a hypergraph *hg* identifies *v* with the root of *hg'*, which by convention is the first vertex in [VertexList]()[*hg'*].
- For two hypergraphs, the bracket sums the insertions of *hg2* into *hg1* at every vertex of *hg1*, minus the insertions of *hg1* into *hg2* at every vertex of *hg2*, each term carrying a sign from the Koszul convention applied to the (graded) vertex being replaced.
- For *n* hypergraphs, the bracket sums consecutive insertions of all *n* arguments over every permutation of their order, each term signed by the permutation's parity together with the Koszul sign of the insertion.
- Terms are collected up to isomorphism: results that agree after canonicalization are merged by adding their coefficients, and terms whose coefficients cancel to 0 are dropped from the resulting association.
- Insertion at a vertex only happens when that vertex's "Degree" annotation (see [HypergraphInsertionBracketDegree]()) matches the degree of the root of the inserted hypergraph; every vertex defaults to degree 0, so plain hypergraphs (with no degree annotations) insert at every vertex.
- For *n* = 2, the bracket satisfies the antisymmetry of a graded Lie bracket: swapping the two arguments negates the result.
- The following option can be given:

| option | default | effect |
|---|---|---|
| <code>"Kind"</code> | <code>"Ordered"</code> | which isomorphism classes of hypergraphs the bracket is computed on |

- Possible settings for `"Kind"` include:

| <code>"Ordered"</code> | hypergraphs up to relabeling of (possibly graded) ordered vertices |
| <code>"Rooted"</code> | hypergraphs with a distinguished root vertex |
| <code>"Tagged"</code> | hypergraphs with a distinguished (tagged) hyperedge |

## Basic Examples

Compute the insertion bracket of a triangle and a single edge:

```wl
Dataset[KeyMap[EdgeList, HypergraphInsertionBracket[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}]]]]
```

<!-- => Dataset with 5 rows: {{1,2},{1,3,4}}->1, {{2,3},{1,2,4}}->1, {{3,4},{1,2,3}}->1, {{1,4},{1,2,3}}->-1, {{1,2},{2,3,4}}->-1 -->

---

The bracket of a hypergraph with itself vanishes:

```wl
HypergraphInsertionBracket[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}}]]
```

<!-- => <||> -->

## Scope

The bracket accepts any number of hypergraph arguments, summing consecutive insertions over every permutation of them:

```wl
Length[HypergraphInsertionBracket[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}], Hypergraph[{{1}}]]]
```

<!-- => 12 -->

---

An argument given as an association is a formal sum of hypergraphs, and the bracket distributes linearly over it:

```wl
Dataset[KeyMap[EdgeList, HypergraphInsertionBracket[<|Hypergraph[{{1, 2}, {2, 3}}] -> 2|>, Hypergraph[{{1, 2}}]]]]
```

<!-- => Dataset with 3 rows, each coefficient twice that of HypergraphInsertionBracket[Hypergraph[{{1, 2}, {2, 3}}], Hypergraph[{{1, 2}}]] -->

## Properties and Relations

Swapping the two arguments negates the bracket, so their sum cancels to zero:

```wl
With[{hg1 = Hypergraph[{{1, 2}, {2, 3}}], hg2 = Hypergraph[{{1, 2}}]},
    DeleteCases[Merge[{HypergraphInsertionBracket[hg1, hg2], HypergraphInsertionBracket[hg2, hg1]}, Total], 0] === <||>
]
```

<!-- => True -->

---

The bracket satisfies the graded Jacobi identity:

```wl
With[{hg1 = Hypergraph[{1, 2, 3}, {{1, 2, 3}}], hg2 = Hypergraph[{1, 2}, {{1, 2}}], hg3 = Hypergraph[{1}, {{1}}], b = HypergraphInsertionBracket[#1, #2] &},
    DeleteCases[Merge[{-b[hg1, b[hg2, hg3]], b[b[hg1, hg2], hg3], b[hg2, b[hg1, hg3]]}, Total], 0] === <||>
]
```

<!-- => True -->

## Possible Issues

In the current implementation, only the default `"Kind" -> "Ordered"` is reliable: `"Kind" -> "Rooted"` and `"Kind" -> "Tagged"` raise errors on this build:

```wl
Quiet[Check[HypergraphInsertionBracket[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}], "Kind" -> "Rooted"], $Failed]] === $Failed
```

<!-- => True -->
