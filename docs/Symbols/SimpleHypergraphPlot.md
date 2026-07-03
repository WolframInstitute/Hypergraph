---
Template: Symbol
Name: SimpleHypergraphPlot
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/SimpleHypergraphPlot
Keywords: [hypergraph, visualization, plot, hyperedge, graph drawing]
SeeAlso: [SimpleHypergraphPlot3D, Hypergraph, Hypergraph3D, HypergraphEmbedding, HypergraphDraw, HighlightRule]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[SimpleHypergraphPlot]()[*hg*]</code> generates a plot of the hypergraph *hg*.

<code>[SimpleHypergraphPlot]()[{$e_1$, $e_2$, …}]</code> plots the hypergraph with hyperedges $e_i$.

## Details & Options

- [SimpleHypergraphPlot]() returns a [Graphics]() object, or a [Graphics3D]() object when the option `"LayoutDimension"` is set to 3.
- Hyperedges are drawn according to their arity: an empty hyperedge `{}` as a circle, a unary hyperedge as a disk around its vertex, a binary hyperedge as a line, and a hyperedge with three or more vertices as a filled polygon spanning its vertices.
- Vertex positions are computed with an edge-weighted spring embedding unless explicit positions are given with [VertexCoordinates]() as a list of rules <code>*v* -> {*x*, *y*}</code>.
- Hyperedges whose `"EdgeSymmetry"` is `"Ordered"`, `"Directed"` or `"Cyclic"` are drawn with arrows along their boundary; `"EdgeArrows" -> True` draws arrows for all hyperedges.
- In the list form, every $e_i$ must be a plain list of vertices; to plot tagged or annotated hyperedges, construct a [Hypergraph]() object first.
- [SimpleHypergraphPlot]() takes the same options as [Hypergraph](), together with all [Graphics]() and [Graphics3D]() options, which are passed to the resulting graphic.

| option | default | effect |
|---|---|---|
| <code>PlotTheme</code> | <code>Automatic</code> | overall theme, such as <code>Automatic</code>, <code>"Colored"</code> or <code>"Dark"</code> |
| <code>ColorFunction</code> | <code>ColorData[97]</code> | function applied to the hyperedge index to determine its color |
| <code>"EdgeArrows"</code> | <code>False</code> | whether to draw arrows along every hyperedge |
| <code>"EdgeSymmetry"</code> | <code>Automatic</code> | hyperedge symmetry: <code>"Unordered"</code>, <code>"Cyclic"</code> or <code>"Ordered"</code> |
| <code>"EdgeType"</code> | <code>"Cyclic"</code> | whether the boundary of a hyperedge closes back to its first vertex |
| <code>"EdgeMethod"</code> | <code>"ConcavePolygon"</code> | method used to construct the polygon of a hyperedge |
| <code>"EdgeSize"</code> | <code>Automatic</code> | radius used to draw nullary and unary hyperedges |
| <code>"EdgeLineStyle"</code> | <code>Automatic</code> | style of hyperedge boundary lines and arrows |
| <code>"LayoutDimension"</code> | <code>2</code> | dimension of the layout, 2 or 3 |

- Styling and labeling options such as [VertexStyle](), [VertexLabels](), [VertexSize](), [VertexShapeFunction](), [EdgeStyle](), [EdgeLabels](), [VertexLabelStyle]() and [EdgeLabelStyle]() accept a single value applied to all elements, or lists of rules assigning values to individual vertices or hyperedges.

## Basic Examples

Plot a hypergraph with hyperedges of different arities, including an empty hyperedge:

```wl
SimpleHypergraphPlot[{{}, {1}, {1, 2}, {4}, {2, 3, 4}}]
```

<!-- => Graphics -->

---

Plot a [Hypergraph]() object:

```wl
SimpleHypergraphPlot[Hypergraph[{{1, 2, 3}, {3, 4, 5}, {5, 6, 1}}]]
```

<!-- => Graphics -->

## Scope

Show vertex labels:

```wl
SimpleHypergraphPlot[{{1, 2, 3}, {3, 4}}, VertexLabels -> Automatic]
```

<!-- => Graphics -->

---

Plot a hypergraph with tagged hyperedges, labeling each hyperedge by its tag:

```wl
SimpleHypergraphPlot[Hypergraph[{{1, 2, 3} -> "a", {3, 4, 5} -> "b"}], EdgeLabels -> "EdgeTag"]
```

<!-- => Graphics -->

---

Lay the hypergraph out in three dimensions:

```wl
SimpleHypergraphPlot[{{1, 2, 3}, {3, 4, 5}}, "LayoutDimension" -> 3]
```

<!-- => Graphics3D -->

## Options

### PlotTheme

Use the dark theme:

```wl
SimpleHypergraphPlot[{{1}, {1, 2}, {4}, {2, 3, 4}, {4, 3, 6, 1}}, PlotTheme -> "Dark"]
```

<!-- => Graphics -->

---

Use the colored theme:

```wl
SimpleHypergraphPlot[{{1, 2}, {2, 3, 4}, {4, 5, 6, 7}}, PlotTheme -> "Colored"]
```

<!-- => Graphics -->

### ColorFunction

Color hyperedges by their index:

```wl
SimpleHypergraphPlot[{{1, 2, 3}, {3, 4, 5}, {5, 6, 1}}, ColorFunction -> ColorData["RustTones"]]
```

<!-- => Graphics -->

### EdgeArrows

Draw arrows along every hyperedge:

```wl
SimpleHypergraphPlot[{{1, 2, 3}, {3, 4}}, "EdgeArrows" -> True, VertexLabels -> Automatic]
```

<!-- => Graphics -->

### EdgeSymmetry

Cyclic hyperedges are drawn with arrows indicating the cyclic order of their vertices:

```wl
SimpleHypergraphPlot[{{1, 2, 3, 4}}, "EdgeSymmetry" -> "Cyclic", VertexLabels -> Automatic]
```

<!-- => Graphics -->

### VertexCoordinates

Place vertices at explicit positions:

```wl
SimpleHypergraphPlot[{{1, 2, 3}, {3, 4}}, VertexCoordinates -> {1 -> {0, 0}, 2 -> {1, 0}, 3 -> {1, 1}, 4 -> {2, 1}}, VertexLabels -> Automatic]
```

<!-- => Graphics -->

### EdgeStyle

Style individual hyperedges:

```wl
SimpleHypergraphPlot[{{1, 2, 3}, {3, 4, 5}}, EdgeStyle -> {{1, 2, 3} -> Directive[Opacity[0.3], Red]}, VertexLabels -> Automatic]
```

<!-- => Graphics -->

## Properties and Relations

A [Hypergraph]() object displays using [SimpleHypergraphPlot]():

```wl
Hypergraph[{{1, 2, 3}, {3, 4, 5}, {5, 6, 1}}]
```

<!-- => Hypergraph -->

---

[SimpleHypergraphPlot3D]() plots a hypergraph with a 3-dimensional layout:

```wl
SimpleHypergraphPlot3D[{{1, 2, 3}, {3, 4, 5}}]
```

<!-- => Graphics3D -->

## Neat Examples

Plot a random hypergraph:

```wl
BlockRandom[SimpleHypergraphPlot[RandomHypergraph[8, {3, 3}], PlotTheme -> "Colored"], RandomSeeding -> 5]
```

<!-- => Graphics -->
