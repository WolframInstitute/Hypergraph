---
Template: Symbol
Name: HypergraphEmbedding
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphEmbedding
Keywords: [hypergraph, embedding, vertex coordinates, layout]
SeeAlso: [SimpleHypergraphPlot, SimpleHypergraphPlot3D, Hypergraph, Hypergraph3D]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphEmbedding]()[*hg*]</code> gives the coordinates at which the vertices of the hypergraph *hg* are drawn.

<code>[HypergraphEmbedding]()[*hg*, *dim*]</code> gives the coordinates of a *dim*-dimensional layout, where *dim* is 2 or 3.

## Details & Options

- [HypergraphEmbedding]() returns one coordinate per vertex, in the order of <code>[VertexList]()[*hg*]</code>.
- The coordinates are computed by the same layout that [SimpleHypergraphPlot]() uses to draw the hypergraph: an edge-weighted spring embedding, unless explicit positions are given.
- With *dim* omitted or [Automatic](), the dimension is taken from the hypergraph's `"LayoutDimension"` option, so a hypergraph constructed with [Hypergraph3D]() gets a 3D embedding.
- Explicit positions given as [VertexCoordinates]() rules <code>*v* -> {*x*, *y*}</code> are returned as specified.
- Additional arguments after *dim* are passed as options to [SimpleHypergraphPlot]().

## Basic Examples

Coordinates at which the vertices of a hypergraph are drawn:

```wl
HypergraphEmbedding[Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => {{0., 3.67394*10^-16}, {1., 2.44929*10^-16}, {2., 0.}} -->

---

Associate each vertex with its coordinate:

```wl
Thread[VertexList[#] -> HypergraphEmbedding[#]] & @ Hypergraph[{{1, 2}, {2, 3}}]
```

<!-- => {1 -> {0., ...}, 2 -> {1., ...}, 3 -> {2., 0.}} -->

## Scope

Get a 3-dimensional embedding of a hypergraph:

```wl
HypergraphEmbedding[Hypergraph[{{1, 2}, {2, 3}}], 3]
```

<!-- => list of three {x, y, z} coordinates -->

---

A hypergraph constructed with [Hypergraph3D]() gets a 3D embedding by default:

```wl
HypergraphEmbedding[Hypergraph3D[{{1, 2}, {2, 3}}]]
```

<!-- => list of three {x, y, z} coordinates -->

---

Explicit vertex coordinates are returned as specified:

```wl
HypergraphEmbedding[Hypergraph[{{1, 2}}, VertexCoordinates -> {1 -> {0, 0}, 2 -> {3, 1}}]]
```

<!-- => {{0., 0.}, {3., 1.}} -->

## Properties and Relations

The embedding has one coordinate per vertex:

```wl
Length[HypergraphEmbedding[Hypergraph[{{1, 2, 3}, {3, 4, 5}}]]] == VertexCount[Hypergraph[{{1, 2, 3}, {3, 4, 5}}]]
```

<!-- => True -->

---

Feeding the embedding back as [VertexCoordinates]() reproduces the automatic layout:

```wl
With[{hg = Hypergraph[{{1, 2}, {2, 3}}]},
    SimpleHypergraphPlot[hg, VertexCoordinates -> Thread[VertexList[hg] -> HypergraphEmbedding[hg]]]
]
```

<!-- => Graphics -->
