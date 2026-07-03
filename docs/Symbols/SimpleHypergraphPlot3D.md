---
Template: Symbol
Name: SimpleHypergraphPlot3D
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/SimpleHypergraphPlot3D
Keywords: [hypergraph, visualization, 3D plot, hyperedge]
SeeAlso: [SimpleHypergraphPlot, Hypergraph3D, Hypergraph, HypergraphEmbedding]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[SimpleHypergraphPlot3D]()[*hg*]</code> generates a 3D plot of the hypergraph *hg*.

<code>[SimpleHypergraphPlot3D]()[{$e_1$, $e_2$, …}]</code> generates a 3D plot of the hypergraph with hyperedges $e_i$.

## Details & Options

- [SimpleHypergraphPlot3D]() returns a [Graphics3D]() object.
- <code>[SimpleHypergraphPlot3D]()[*hg*, *opts*]</code> is equivalent to <code>[SimpleHypergraphPlot]()[*hg*, "LayoutDimension" -> 3, *opts*]</code>.
- Vertices are drawn as spheres and hyperedges with three or more vertices as surfaces spanning their vertices; a binary hyperedge is drawn as a line, a unary hyperedge as a sphere around its vertex and an empty hyperedge `{}` as a free-standing sphere.
- [SimpleHypergraphPlot3D]() accepts the same options as [SimpleHypergraphPlot](), including all [Hypergraph]() and [Graphics3D]() options.
- Explicit vertex positions can be given with [VertexCoordinates]() as a list of rules <code>*v* -> {*x*, *y*, *z*}</code>.

## Basic Examples

Plot a hypergraph in 3D:

```wl
SimpleHypergraphPlot3D[{{1, 2, 3}, {3, 4, 5}, {5, 6, 1}}]
```

<!-- => Graphics3D -->

---

Plot a [Hypergraph]() object in 3D:

```wl
SimpleHypergraphPlot3D[Hypergraph[{{1, 2, 3}, {2, 3, 4}}]]
```

<!-- => Graphics3D -->

## Scope

Show vertex labels:

```wl
SimpleHypergraphPlot3D[{{1, 2, 3}, {3, 4}}, VertexLabels -> Automatic]
```

<!-- => Graphics3D -->

---

Draw arrows along every hyperedge:

```wl
SimpleHypergraphPlot3D[{{1, 2, 3}, {3, 4, 5}}, "EdgeArrows" -> True]
```

<!-- => Graphics3D -->

## Properties and Relations

[SimpleHypergraphPlot3D]() is equivalent to [SimpleHypergraphPlot]() with `"LayoutDimension"` set to 3:

```wl
SimpleHypergraphPlot[{{1, 2, 3}, {3, 4, 5}}, "LayoutDimension" -> 3]
```

<!-- => Graphics3D -->

---

A hypergraph constructed with [Hypergraph3D]() displays as a 3D plot automatically:

```wl
Hypergraph3D[{{1, 2, 3}, {3, 4, 5}}]
```

<!-- => Hypergraph -->

## Neat Examples

Plot a random hypergraph in 3D:

```wl
BlockRandom[SimpleHypergraphPlot3D[RandomHypergraph[8, {4, 3}]], RandomSeeding -> 5]
```

<!-- => Graphics3D -->
