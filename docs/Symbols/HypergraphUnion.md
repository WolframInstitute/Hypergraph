---
Template: Symbol
Name: HypergraphUnion
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphUnion
Keywords: [union, hyperedges, Plus, combine hypergraphs]
SeeAlso: [HypergraphHadamardProduct, Hypergraph, HyperedgeList, EdgeSymmetry]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphUnion]()[*hg1*, *hg2*, …]</code> gives the hypergraph whose vertices are the union of the vertices of *hg1*, *hg2*, … and whose hyperedges are the concatenation of their hyperedges.

## Details & Options

- [HypergraphUnion]() backs `Plus` on [Hypergraph]() objects, so `hg1 + hg2` and <code>[HypergraphUnion]()[*hg1*, *hg2*]</code> are the same operation.
- Hyperedges are concatenated, not merged: a hyperedge present in more than one argument appears once for each occurrence, with multiplicity preserved.
- Vertex and edge annotations (styles, labels, coordinates, and other per-vertex or per-edge options) are merged across the arguments; when several arguments annotate the same vertex or edge, the first non-`None` annotation encountered wins.

## Basic Examples

Take the union of two hypergraphs that share a vertex:

```wl
EdgeList[HypergraphUnion[Hypergraph[{{1, 2}}], Hypergraph[{{2, 3}}]]]
```

<!-- => {{1, 2}, {2, 3}} -->

---

Its vertex list is the union of the two vertex lists:

```wl
VertexList[HypergraphUnion[Hypergraph[{{1, 2}}], Hypergraph[{{2, 3}}]]]
```

<!-- => {1, 2, 3} -->

---

[HypergraphUnion]() backs `Plus`, so adding two hypergraphs gives the same result:

```wl
EdgeList[Hypergraph[{{1, 2}}] + Hypergraph[{{2, 3}}]]
```

<!-- => {{1, 2}, {2, 3}} -->

## Scope

[HypergraphUnion]() accepts any number of hypergraphs, concatenating all of their hyperedges:

```wl
EdgeList[HypergraphUnion[Hypergraph[{{1, 2}}], Hypergraph[{{2, 3}}], Hypergraph[{{3, 4}}]]]
```

<!-- => {{1, 2}, {2, 3}, {3, 4}} -->

---

A vertex style set on only one argument is transferred to the union:

```wl
Lookup[Options[HypergraphUnion[Hypergraph[{{a, b}}, VertexStyle -> {a -> Red}], Hypergraph[{{b, c}}]]], VertexStyle][[1]]
```

<!-- => Verbatim[a] -> RGBColor[1, 0, 0] -->

