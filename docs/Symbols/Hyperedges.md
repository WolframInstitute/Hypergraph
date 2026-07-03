---
Template: Symbol
Name: Hyperedges
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/Hyperedges
Keywords: [hyperedge collection, edge list, edge tags, hypergraph internals]
SeeAlso: [Hyperedge, HyperedgeList, EdgeListTagged, HypergraphQ, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[Hyperedges]()[*e1*, *e2*, …]</code> represents a collection of hyperedges *e1*, *e2*, …, each given in the same form as an edge in the list passed to [Hypergraph]().

## Details & Options

- [Hyperedges]() is the internal representation [Hypergraph]() uses for its edges; [HyperedgesQ]() tests whether an expression is a valid [Hyperedges]() object.
- Each *ei* is a list of vertices, optionally tagged as *edge* `->` *tag*, or wrapped in [Labeled](), [Style](), or [Annotation]() to carry per-edge styling recognized by [Hypergraph]().
- <code>*he*["EdgeList"]</code> gives the hyperedges with any tags stripped; <code>*he*["EdgeListTagged"]</code> keeps the `->` *tag* on tagged edges; <code>*he*["VertexList"]</code> gives the deduplicated vertices appearing in any hyperedge.
- [Join]() concatenates [Hyperedges]() collections in argument order; [Inverse]() reverses both the order of the hyperedges and the vertex order within each hyperedge.
- `Hyperedges[0]` gives `Hyperedges[]` (no hyperedges) and `Hyperedges[1]` gives `Hyperedges[{}]` (one empty hyperedge); any other atom *v* becomes the singleton hyperedge `{v}`.

## Basic Examples

Build a collection of hyperedges directly:

```wl
he = Hyperedges[{1, 2}, {2, 3}, {1, 2, 3}]
```

<!-- => Hyperedges box notation: (1 2) [CirclePlus] (2 3) [CirclePlus] (1 2 3) -->

Its edge list matches the vertex lists it was given:

```wl
he["EdgeList"]
```

<!-- => {{1, 2}, {2, 3}, {1, 2, 3}} -->

---

A hyperedge can carry a tag, kept by `"EdgeListTagged"`:

```wl
Hyperedges[{1, 2} -> "a"]["EdgeListTagged"]
```

<!-- => {{1, 2} -> "a"} -->

## Scope

Feeding a [Hyperedges]() collection to [Hypergraph]() reproduces the same hypergraph as a plain list of edges:

```wl
Hypergraph[Hyperedges[{1, 2}, {2, 3}]] === Hypergraph[{{1, 2}, {2, 3}}]
```

<!-- => True -->

---

[Join]() concatenates two hyperedge collections in order:

```wl
Join[Hyperedges[{1, 2}, {2, 3}], Hyperedges[{3, 4}]]["EdgeList"]
```

<!-- => {{1, 2}, {2, 3}, {3, 4}} -->

---

[Inverse]() reverses both the order of the hyperedges and the vertices within each:

```wl
Inverse[Hyperedges[{1, 2}, {3, 4, 5}]]["EdgeList"]
```

<!-- => {{5, 4, 3}, {2, 1}} -->

---

`Hyperedges[0]` and `Hyperedges[1]` are special-cased, matching the empty hypergraph [Hypergraph]() builds by default:

```wl
{Hyperedges[0], Hyperedges[1]}
```

<!-- => {Hyperedges[], Hyperedges[{}]} -->

## Properties and Relations

[HyperedgesQ]() recognizes a validly constructed collection:

```wl
HyperedgesQ[Hyperedges[{1, 2}]]
```

<!-- => True -->

---

A non-[Hyperedges]() expression fails the test:

```wl
HyperedgesQ[{1, 2}]
```

<!-- => False -->

## Possible Issues

A hyperedge's tag does not survive being wrapped in a [Hypergraph]():

```wl
EdgeListTagged[Hypergraph[Hyperedges[{1, 2} -> "a"]]]
```

<!-- => {{1, 2}} -->
