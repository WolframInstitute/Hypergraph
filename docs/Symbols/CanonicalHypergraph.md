---
Template: Symbol
Name: CanonicalHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/CanonicalHypergraph
Keywords: [canonical form, hypergraph isomorphism, canonicalization, vertex relabeling]
SeeAlso: [IsomorphicHypergraphQ, Hypergraph, EnumerateHypergraphs, RandomHypergraph, HypergraphInsertionBracket]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[CanonicalHypergraph]()[*hg*]</code> gives a canonical form of the hypergraph *hg*, with vertices relabeled by consecutive integers.

<code>[CanonicalHypergraph]()[*args*]</code> gives the canonical form of <code>[Hypergraph]()[*args*]</code>.

## Details & Options

- The canonical form is a [Hypergraph]() whose vertices are the consecutive integers 1, 2, ..., with hyperedges rewritten accordingly and sorted into a canonical order.
- Isomorphic hypergraphs, in the sense of [IsomorphicHypergraphQ](), have identical canonical forms.
- The edge symmetry of *hg* is preserved in the canonical form; by default, all other annotations and styling options are dropped.
- The following options can be given:

| option | default | effect |
|---|---|---|
| <code>Method</code> | <code>Automatic</code> | the method used to find the canonical vertex relabeling |
| <code>"Annotations"</code> | <code>False</code> | whether to transfer vertex and edge annotations to the canonical form |

- Possible settings for [Method]() include:

| <code>Automatic</code> | canonicalize a graph encoding of the hypergraph with [CanonicalGraph]() |
| <code>"Graph"</code> | same as <code>Automatic</code> |
| <code>"MultiGraph"</code> | reduce repeated edges of the graph encoding before canonicalizing |
| <code>"Combinatorial"</code> | exhaustive combinatorial search |

- The <code>Automatic</code> method falls back to the combinatorial search when [CanonicalGraph]() cannot handle the graph encoding; the <code>"Combinatorial"</code> method uses the resource function `FindCanonicalHypergraphIsomorphism` directly.
- Canonical forms computed with different [Method]() settings are always isomorphic, but need not be identical.
- With <code>"Annotations" -> True</code>, vertex and edge annotations, including styles, are transferred to the relabeled vertices and hyperedges of the canonical form.

## Basic Examples

Find the canonical form of a hypergraph:

```wl
chg = CanonicalHypergraph[Hypergraph[{{a, b}, {b, c}, {c, a, d}}]]
```

<!-- => Hypergraph object, rendered as a hypergraph plot with 4 vertices -->

Its vertices are relabeled by consecutive integers:

```wl
EdgeList[chg]
```

<!-- => {{1, 3}, {1, 4}, {2, 3, 4}} -->

---

Isomorphic hypergraphs have identical canonical forms:

```wl
CanonicalHypergraph[Hypergraph[{{a, b}, {b, c}}]] === CanonicalHypergraph[Hypergraph[{{2, 3}, {1, 2}}]]
```

<!-- => True -->

## Scope

[CanonicalHypergraph]() can be applied directly to a list of hyperedges:

```wl
CanonicalHypergraph[{{x, y}, {y, z}}]
```

<!-- => Hypergraph object with EdgeList {{1, 3}, {2, 3}}, rendered as a plot -->

---

Edge symmetry is preserved by canonicalization:

```wl
CanonicalHypergraph[Hypergraph[{{b, a}, {c, b}}, "EdgeSymmetry" -> "Ordered"]]["EdgeSymmetry"]
```

<!-- => {"Ordered", "Ordered"} -->

---

Repeated hyperedges are kept:

```wl
EdgeList @ CanonicalHypergraph[Hypergraph[{{1, 2}, {1, 2}, {2, 3}}]]
```

<!-- => {{1, 3}, {2, 3}, {2, 3}} -->

---

Isolated vertices are relabeled along with the rest:

```wl
VertexList @ CanonicalHypergraph[Hypergraph[{a, b, c, d}, {{a, b}}]]
```

<!-- => {1, 2, 3, 4} -->

## Options

### Method

The default method canonicalizes a graph encoding of the hypergraph:

```wl
EdgeList @ CanonicalHypergraph[Hypergraph[{{1, 2, 3}, {3, 4}}]]
```

<!-- => {{1, 4}, {2, 3, 4}} -->

The `"Combinatorial"` method can return a different canonical representative:

```wl
EdgeList @ CanonicalHypergraph[Hypergraph[{{1, 2, 3}, {3, 4}}], Method -> "Combinatorial"]
```

<!-- => {{1, 4}, {1, 2, 3}} -->

Representatives obtained with different methods are always isomorphic:

```wl
IsomorphicHypergraphQ[CanonicalHypergraph[Hypergraph[{{1, 2, 3}, {3, 4}}]], CanonicalHypergraph[Hypergraph[{{1, 2, 3}, {3, 4}}], Method -> "Combinatorial"]]
```

<!-- => True -->

### "Annotations"

By default, styles and annotations are not transferred to the canonical form:

```wl
CanonicalHypergraph[Hypergraph[{{a, b}, {b, c}}, VertexStyle -> {a -> Red}]]
```

<!-- => hypergraph plot with default vertex styling -->

With `"Annotations" -> True`, they are carried through the vertex relabeling:

```wl
CanonicalHypergraph[Hypergraph[{{a, b}, {b, c}}, VertexStyle -> {a -> Red}], "Annotations" -> True]
```

<!-- => hypergraph plot in which the vertex relabeled from a is red -->

## Properties and Relations

Two hypergraphs are isomorphic exactly when their canonical forms coincide:

```wl
With[{hg1 = Hypergraph[{{1, 2}, {2, 3}, {3, 1}}], hg2 = Hypergraph[{{x, y}, {y, z}, {z, x}}]},
    IsomorphicHypergraphQ[hg1, hg2] === (CanonicalHypergraph[hg1] === CanonicalHypergraph[hg2])
]
```

<!-- => True -->

---

[CanonicalHypergraph]() is idempotent, so a canonical form is its own canonical form:

```wl
With[{chg = CanonicalHypergraph[Hypergraph[{{a, b}, {b, c}, {c, a, d}}]]},
    CanonicalHypergraph[chg] === chg
]
```

<!-- => True -->

---

Hypergraphs enumerated by [EnumerateHypergraphs]() are pairwise non-isomorphic, so their canonical forms are all distinct:

```wl
DuplicateFreeQ[CanonicalHypergraph /@ EnumerateHypergraphs[{{2, 2}}]]
```

<!-- => True -->

## Possible Issues

Canonical representatives depend on the [Method]() setting, so only compare canonical forms computed with the same method:

```wl
CanonicalHypergraph[Hypergraph[{{1, 2, 3}, {3, 4}}]] === CanonicalHypergraph[Hypergraph[{{1, 2, 3}, {3, 4}}], Method -> "Combinatorial"]
```

<!-- => False -->
