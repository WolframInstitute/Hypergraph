---
Template: Symbol
Name: EnumerateHypergraphs
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/EnumerateHypergraphs
Keywords: [enumerate hypergraphs, hypergraph signature, canonical enumeration, connected hypergraphs, simple hypergraphs]
SeeAlso: [EnumerateOrderedHypergraphs, EnumerateHypergraphRules, RandomHypergraph, CanonicalHypergraph, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[EnumerateHypergraphs]()[{{$m_1$, $a_1$}, {$m_2$, $a_2$}, …}]</code> enumerates simple connected hypergraphs with $m_1$ hyperedges of arity $a_1$, $m_2$ hyperedges of arity $a_2$, etc.

<code>[EnumerateHypergraphs]()[*n*, *sig*]</code> enumerates hypergraphs of signature *sig* with up to *n* vertices.

<code>[EnumerateHypergraphs]()[{*n*}, *sig*]</code> enumerates hypergraphs with exactly *n* vertices.

## Details & Options

- [EnumerateHypergraphs]() returns a list of [Hypergraph]() objects with unordered hyperedges; use [EnumerateOrderedHypergraphs]() for ordered hyperedges.
- The signature `{{m1, a1}, {m2, a2}, ...}` specifies `m1` hyperedges of arity `a1`, `m2` hyperedges of arity `a2`, etc, where the arity of a hyperedge is the number of vertices it connects.
- By default, [EnumerateHypergraphs]() returns one representative per isomorphism class, keeping only simple, connected hypergraphs.
- With no explicit vertex count, hypergraphs with up to the maximum number of vertices that a connected hypergraph of the given signature can have are enumerated.
- <code>[EnumerateHypergraphs]()[*n*, *sig*]</code> joins the exactly-*k* enumerations for *k* from 1 to *n*; the vertex count `All` or `Automatic` is equivalent to the default.
- <code>[EnumerateHypergraphs]()[…]</code> is equivalent to <code>[EnumerateOrderedHypergraphs]()[…, "EdgeSymmetry" -> "Unordered"]</code>.
- Options of [Hypergraph](), such as [VertexLabels]() or `"EdgeSymmetry"`, are applied to every generated hypergraph.
- The following options can be given:

| option | default | effect |
|---|---|---|
| <code>"Simple"</code> | <code>True</code> | keep only simple hypergraphs (`True`), only non-simple ones (`False`), or both (`All`) |
| <code>"Connected"</code> | <code>True</code> | keep only connected hypergraphs (`True`), only disconnected ones (`False`), or both (`All`) |
| <code>"Canonical"</code> | <code>Automatic</code> | whether to keep a single representative per isomorphism class; `None` or `False` keeps all enumerated representatives, and any other setting is used as the `Method` of [CanonicalHypergraph]() |

## Basic Examples

Enumerate all simple connected hypergraphs with one unary and two binary hyperedges:

```wl
EnumerateHypergraphs[{{1, 1}, {2, 2}}]
```

<!-- => list of 2 hypergraphs -->

---

Enumerate hypergraphs with one hyperedge each of arity 1, 2 and 3:

```wl
EnumerateHypergraphs[{{1, 1}, {1, 2}, {1, 3}}]
```

<!-- => list of 5 hypergraphs -->

## Scope

Simple connected hypergraphs with three binary hyperedges have three or four vertices:

```wl
EnumerateHypergraphs[{{3, 2}}]
```

<!-- => list of 3 hypergraphs -->

---

Enumerate hypergraphs with exactly four vertices:

```wl
EnumerateHypergraphs[{4}, {{3, 2}}]
```

<!-- => list of 2 hypergraphs -->

---

Enumerate hypergraphs with at most three vertices:

```wl
EnumerateHypergraphs[3, {{3, 2}}]
```

<!-- => list of 1 hypergraph -->

---

Options of [Hypergraph]() are applied to every result:

```wl
EnumerateHypergraphs[{{3, 2}}, VertexLabels -> Automatic]
```

<!-- => list of 3 hypergraphs with labeled vertices -->

## Options

### "Simple"

By default, only simple hypergraphs are returned:

```wl
EnumerateHypergraphs[{{2, 2}}]
```

<!-- => list of 1 hypergraph -->

---

Enumerate only non-simple hypergraphs:

```wl
EnumerateHypergraphs[{{2, 2}}, "Simple" -> False]
```

<!-- => list of 3 hypergraphs -->

---

Include both simple and non-simple hypergraphs:

```wl
EnumerateHypergraphs[{{2, 2}}, "Simple" -> All]
```

<!-- => list of 4 hypergraphs -->

### "Connected"

Enumerate only disconnected hypergraphs:

```wl
EnumerateHypergraphs[{{2, 2}}, "Connected" -> False]
```

<!-- => list of 1 hypergraph -->

---

Include both connected and disconnected hypergraphs:

```wl
EnumerateHypergraphs[{{2, 2}}, "Connected" -> All]
```

<!-- => list of 2 hypergraphs -->

### "Canonical"

Without canonicalization, several representatives of the same isomorphism class are kept:

```wl
EnumerateHypergraphs[{{2, 2}}, "Canonical" -> None]
```

<!-- => list of 3 hypergraphs -->

### "EdgeSymmetry"

Enumerate with ordered hyperedges, like [EnumerateOrderedHypergraphs]():

```wl
EnumerateHypergraphs[{{2, 2}}, "EdgeSymmetry" -> "Ordered"]
```

<!-- => list of 4 hypergraphs -->

## Properties and Relations

The default enumeration accumulates the exact vertex-count slices:

```wl
Table[Length[EnumerateHypergraphs[{s}, {{3, 2}}]], {s, 4}]
```

<!-- => {0, 0, 1, 2} -->

---

Its length is the total over all slices:

```wl
Length[EnumerateHypergraphs[{{3, 2}}]] == Total[Table[Length[EnumerateHypergraphs[{s}, {{3, 2}}]], {s, 4}]]
```

<!-- => True -->

---

[EnumerateOrderedHypergraphs]() distinguishes hyperedge orientations that [EnumerateHypergraphs]() identifies:

```wl
EnumerateOrderedHypergraphs[{{2, 2}}]
```

<!-- => list of 4 hypergraphs -->

## Possible Issues

The number of hypergraphs and the enumeration time grow combinatorially with the signature:

```wl
Length[EnumerateHypergraphs[{{4, 2}}]]
```

<!-- => 5 (about 2 seconds) -->
