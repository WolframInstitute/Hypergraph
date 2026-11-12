---
Template: Symbol
Name: EdgeListTagged
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/EdgeListTagged
Keywords: [hyperedges, edge tags, edge list]
SeeAlso: [Hypergraph, HyperedgeList, EdgeMultiplicity, Hyperedge]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[EdgeListTagged]()[*hg*]</code> gives the hyperedges of the hypergraph *hg*, with tagged edges given as <code>*edge* -> *tag*</code>.

<code>[EdgeListTagged]()[*hg*, *patt*]</code> gives only the tagged hyperedges that match the pattern *patt*.

## Details & Options

- A hyperedge is tagged by giving it as <code>*edge* -> *tag*</code> in the edge list of a [Hypergraph](), or as the third argument of [DirectedEdge](), [UndirectedEdge]() or [CyclicEdge]().
- An untagged hyperedge appears in <code>[EdgeListTagged]()[*hg*]</code> exactly as it does in <code>[EdgeList]()[*hg*]</code>, as a plain list of vertices; a tagged one appears as <code>*edge* -> *tag*</code>.
- <code>*hg*["EdgeTags"]</code> gives just the tags, in the same order, with `None` for an untagged edge.
- With <code>[EdgeLabels]() -> "EdgeTag"</code>, a hypergraph labels each hyperedge with its tag.

## Basic Examples

Get the hyperedges of a hypergraph together with their tags:

```wl
EdgeListTagged[Hypergraph[{{1, 2} -> "a", {2, 3}, {1, 4} -> "b"}]]
```

<!-- => {{1, 2} -> "a", {2, 3}, {1, 4} -> "b"} -->

---

Keep only the tagged hyperedges:

```wl
EdgeListTagged[Hypergraph[{{1, 2} -> "a", {2, 3}, {1, 4} -> "b"}], _Rule]
```

<!-- => {{1, 2} -> "a", {1, 4} -> "b"} -->

## Scope

Select the hyperedges with a given tag:

```wl
EdgeListTagged[Hypergraph[{{1, 2} -> "a", {2, 3}, {1, 4} -> "b"}], HoldPattern[_ -> "a"]]
```

<!-- => {{1, 2} -> "a"} -->

---

Give just the tags, with `None` for an untagged hyperedge:

```wl
Hypergraph[{{1, 2} -> "a", {2, 3}, {1, 4} -> "b"}]["EdgeTags"]
```

<!-- => {"a", None, "b"} -->

---

Label each hyperedge with its tag:

```wl
Hypergraph[{{1, 2} -> "a", {1, 2} -> "b", {2, 3} -> "c"}, EdgeLabels -> "EdgeTag", VertexLabels -> "Name"]
```

<!-- => Hypergraph with each edge labeled by its tag -->

## Properties and Relations

[EdgeList]() gives the same hyperedges without their tags:

```wl
EdgeList[Hypergraph[{{1, 2} -> "a", {2, 3}, {1, 4} -> "b"}]]
```

<!-- => {{1, 2}, {2, 3}, {1, 4}} -->

---

For a hypergraph with no tagged edges, `EdgeListTagged` agrees with `EdgeList`:

```wl
hg = Hypergraph[{{1, 2}, {2, 3}}];
EdgeList[hg] === EdgeListTagged[hg]
```

<!-- => True -->

## Possible Issues

A pattern of the form <code>*lhs* -> *rhs*</code> is read as a replacement rule, as in [Cases](), so it replaces every hyperedge instead of selecting the tagged ones:

```wl
EdgeListTagged[Hypergraph[{{1, 2} -> "a", {2, 3}, {1, 4} -> "b"}], _ -> "a"]
```

<!-- => {"a", "a", "a"} -->

---

Wrap such a pattern in [HoldPattern]() to select by tag:

```wl
EdgeListTagged[Hypergraph[{{1, 2} -> "a", {2, 3}, {1, 4} -> "b"}], HoldPattern[_ -> "a"]]
```

<!-- => {{1, 2} -> "a"} -->
