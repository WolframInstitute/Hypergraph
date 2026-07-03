---
Template: Symbol
Name: EnumerateOrderedHypergraphs
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/EnumerateOrderedHypergraphs
Keywords: [enumerate hypergraphs, ordered hyperedges, hypergraph signature, canonical enumeration]
SeeAlso: [EnumerateHypergraphs, EnumerateHypergraphRules, RandomHypergraph, CanonicalHypergraph, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[EnumerateOrderedHypergraphs]()[{{$m_1$, $a_1$}, {$m_2$, $a_2$}, …}]</code> enumerates simple connected hypergraphs with ordered hyperedges, $m_1$ of arity $a_1$, $m_2$ of arity $a_2$, etc.

<code>[EnumerateOrderedHypergraphs]()[*n*, *sig*]</code> enumerates hypergraphs of signature *sig* with up to *n* vertices.

<code>[EnumerateOrderedHypergraphs]()[{*n*}, *sig*]</code> enumerates hypergraphs with exactly *n* vertices.

## Details & Options

- [EnumerateOrderedHypergraphs]() returns a list of [Hypergraph]() objects whose hyperedges are ordered, so two hyperedges connecting the same vertices in different orders are distinct.
- The signature `{{m1, a1}, {m2, a2}, ...}` specifies `m1` hyperedges of arity `a1`, `m2` hyperedges of arity `a2`, etc, where the arity of a hyperedge is the number of vertices it connects.
- By default, [EnumerateOrderedHypergraphs]() returns one representative per isomorphism class, keeping only simple, connected hypergraphs.
- With no explicit vertex count, hypergraphs with up to the maximum number of vertices that a connected hypergraph of the given signature can have are enumerated.
- <code>[EnumerateOrderedHypergraphs]()[*n*, *sig*]</code> joins the exactly-*k* enumerations for *k* from 1 to *n*; the vertex count `All` or `Automatic` is equivalent to the default.
- <code>[EnumerateHypergraphs]()[…]</code> is equivalent to <code>[EnumerateOrderedHypergraphs]()[…, "EdgeSymmetry" -> "Unordered"]</code>.
- Options of [Hypergraph](), such as [VertexLabels]() or `"EdgeSymmetry"`, are applied to every generated hypergraph.
- The following options can be given:

| option | default | effect |
|---|---|---|
| <code>"Simple"</code> | <code>True</code> | keep only simple hypergraphs (`True`), only non-simple ones (`False`), or both (`All`) |
| <code>"Connected"</code> | <code>True</code> | keep only connected hypergraphs (`True`), only disconnected ones (`False`), or both (`All`) |
| <code>"Canonical"</code> | <code>Automatic</code> | whether to keep a single representative per isomorphism class; `None` or `False` keeps all enumerated representatives, and any other setting is used as the `Method` of [CanonicalHypergraph]() |

## Basic Examples

Enumerate all simple connected hypergraphs with two ordered binary hyperedges:

```wl
EnumerateOrderedHypergraphs[{{2, 2}}]
```

<!-- => list of 4 hypergraphs -->

---

Enumerate hypergraphs with three ordered binary hyperedges:

```wl
EnumerateOrderedHypergraphs[{{3, 2}}]
```

<!-- => list of 12 hypergraphs -->

## Scope

Enumerate hypergraphs with exactly three vertices:

```wl
EnumerateOrderedHypergraphs[{3}, {{2, 2}}]
```

<!-- => list of 3 hypergraphs -->

---

Enumerate hypergraphs with at most two vertices:

```wl
EnumerateOrderedHypergraphs[2, {{2, 2}}]
```

<!-- => list of 1 hypergraph -->

---

Options of [Hypergraph]() are applied to every result:

```wl
EnumerateOrderedHypergraphs[{{2, 2}}, VertexLabels -> Automatic]
```

<!-- => list of 4 hypergraphs with labeled vertices -->

## Options

### "Simple"

Enumerate only non-simple hypergraphs:

```wl
EnumerateOrderedHypergraphs[{{2, 2}}, "Simple" -> False]
```

<!-- => list of 4 hypergraphs -->

---

Include both simple and non-simple hypergraphs:

```wl
EnumerateOrderedHypergraphs[{{2, 2}}, "Simple" -> All]
```

<!-- => list of 8 hypergraphs -->

### "Connected"

Enumerate only disconnected hypergraphs:

```wl
EnumerateOrderedHypergraphs[{{2, 2}}, "Connected" -> False]
```

<!-- => list of 1 hypergraph -->

---

Include both connected and disconnected hypergraphs:

```wl
EnumerateOrderedHypergraphs[{{2, 2}}, "Connected" -> All]
```

<!-- => list of 5 hypergraphs -->

### "EdgeSymmetry"

With unordered hyperedges, the enumeration reduces to [EnumerateHypergraphs]():

```wl
EnumerateOrderedHypergraphs[{{2, 2}}, "EdgeSymmetry" -> "Unordered"]
```

<!-- => list of 1 hypergraph -->

## Properties and Relations

The default enumeration accumulates the exact vertex-count slices:

```wl
Table[Length[EnumerateOrderedHypergraphs[{s}, {{2, 2}}]], {s, 3}]
```

<!-- => {0, 1, 3} -->

---

[EnumerateHypergraphs]() identifies hypergraphs that differ only by hyperedge orientation:

```wl
EnumerateHypergraphs[{{2, 2}}]
```

<!-- => list of 1 hypergraph -->

## Possible Issues

The number of hypergraphs and the enumeration time grow combinatorially with the signature:

```wl
Length[EnumerateOrderedHypergraphs[{{4, 2}}]]
```

<!-- => filled in after verification -->
