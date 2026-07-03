---
Template: Symbol
Name: Hypergraph3D
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/Hypergraph3D
Keywords: [hypergraph, 3D layout, hyperedge, construction]
SeeAlso: [Hypergraph, SimpleHypergraphPlot3D, SimpleHypergraphPlot, HypergraphEmbedding]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[Hypergraph3D]()[{$e_1$, $e_2$, …}]</code> yields a hypergraph with hyperedges $e_i$, laid out in three dimensions.

<code>[Hypergraph3D]()[{$v_1$, $v_2$, …}, {$e_1$, $e_2$, …}]</code> yields a hypergraph with vertices $v_i$ and hyperedges $e_j$.

<code>[Hypergraph3D]()[*hg*]</code> converts the hypergraph *hg* to a 3-dimensional layout.

## Details & Options

- [Hypergraph3D]() returns a [Hypergraph]() object with the option `"LayoutDimension"` set to 3; it accepts the same arguments and options as [Hypergraph]().
- A hypergraph with `"LayoutDimension" -> 3` displays as a [Graphics3D]() rendered by [SimpleHypergraphPlot3D](), and its [HypergraphEmbedding]() consists of 3D coordinates.
- <code>[Hypergraph3D]()[*hg*]</code> keeps the vertices, hyperedges and styling options of *hg* but drops its 2D [VertexCoordinates](), so that positions are recomputed in three dimensions.
- Conversely, <code>[Hypergraph]()[*hg*]</code> converts a 3D hypergraph back to a 2-dimensional layout.

## Basic Examples

Make a hypergraph laid out in 3D:

```wl
Hypergraph3D[{{1, 2, 3}, {2, 3, 4}, {4, 5, 1}}]
```

<!-- => Hypergraph -->

---

Convert an existing hypergraph to a 3D layout:

```wl
Hypergraph3D[Hypergraph[{{1, 2}, {2, 3, 4}}]]
```

<!-- => Hypergraph -->

## Scope

Specify isolated vertices in addition to hyperedges:

```wl
Hypergraph3D[{1, 2, 3, 4, 5}, {{1, 2, 3}}]
```

<!-- => Hypergraph -->

---

Styling options work the same way as for [Hypergraph]():

```wl
Hypergraph3D[{{1, 2, 3}, {3, 4, 5}}, EdgeStyle -> {{1, 2, 3} -> Directive[Opacity[0.3], Red]}, VertexLabels -> Automatic]
```

<!-- => Hypergraph -->

## Properties and Relations

Plotting a 3D hypergraph gives a [Graphics3D]() object:

```wl
SimpleHypergraphPlot[Hypergraph3D[{{1, 2, 3}, {3, 4, 5}}]]
```

<!-- => Graphics3D -->

---

The embedding of a 3D hypergraph consists of 3D coordinates:

```wl
HypergraphEmbedding[Hypergraph3D[{{1, 2}, {2, 3}}]]
```

<!-- => list of three {x, y, z} coordinates -->
