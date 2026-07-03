---
Template: Symbol
Name: Hypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/Hypergraph
Keywords: [hypergraph, hyperedge, edge symmetry, graph generalization, Wolfram model]
SeeAlso: [HypergraphRule, SimpleHypergraphPlot, Hypergraph3D, HypergraphDraw, CanonicalHypergraph, RandomHypergraph, EnumerateHypergraphs]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[Hypergraph]()[{$e_1$, $e_2$, …}]</code> yields a hypergraph with hyperedges $e_i$.

<code>[Hypergraph]()[{$v_1$, $v_2$, …}, {$e_1$, $e_2$, …}]</code> yields a hypergraph with vertices $v_i$ and hyperedges $e_j$.

<code>[Hypergraph]()[*g*]</code> yields a hypergraph with the same vertices and edges as the graph *g*.

<code>[Hypergraph]()[*hg*, *opts*]</code> yields a copy of the hypergraph *hg* with the options *opts* applied.

## Details & Options

- A [Hypergraph]() generalizes [Graph]() by allowing hyperedges that connect any number of vertices.
- Each hyperedge $e_i$ is a list of vertices <code>{$v_1$, $v_2$, …}</code>. Hyperedges can be empty, contain a single vertex, and repeat; vertices can be arbitrary expressions.
- [Hypergraph]() objects display as a graphical plot, drawn like <code>[SimpleHypergraphPlot]()[*hg*]</code>; hypergraphs with many vertices or edges display as a summary box instead.
- `HypergraphQ` tests whether an expression is a valid [Hypergraph]() object.
- In <code>[Hypergraph]()[{$e_1$, $e_2$, …}]</code>, each hyperedge can be given in the following forms:

| edge specification | interpretation |
|---|---|
| <code>{*v*<sub>1</sub>, *v*<sub>2</sub>, …}</code> | hyperedge with the default edge symmetry |
| <code>*u* -> *w*</code> | binary hyperedge with `"Ordered"` symmetry |
| <code>[DirectedEdge]()[*u*, *w*]</code> | hyperedge with `"Ordered"` symmetry joining *u* and *w* (vertices or vertex lists) |
| <code>[UndirectedEdge]()[*u*, *w*]</code> | hyperedge with `"Unordered"` symmetry |
| <code>CyclicEdge[{*v*<sub>1</sub>, *v*<sub>2</sub>, …}]</code> | hyperedge with `"Cyclic"` symmetry |
| <code>[Labeled]()[*e*, *lbl*]</code> | hyperedge *e* labeled *lbl* |
| <code>[Style]()[*e*, *style*]</code> | hyperedge *e* with the given style |
| <code>[Annotation]()[*e*, *opt* -> *value*]</code> | hyperedge *e* with an arbitrary annotation |

- Vertices in the explicit vertex list can likewise be wrapped in [Labeled](), [Style]() or [Annotation]().
- Every hyperedge carries an edge symmetry, a group of vertex permutations under which the hyperedge is considered equivalent. Possible settings for the `"EdgeSymmetry"` option include:

| setting | interpretation |
|---|---|
| `"Unordered"` | all orderings of the vertices are equivalent (default) |
| `"Ordered"` or `"Directed"` | the hyperedge is taken exactly as written |
| `"Cyclic"` | orderings equivalent up to cyclic rotation |
| list of [Cycles]() | explicit list of equivalent vertex permutations |
| <code>{*e*<sub>1</sub> -> *sym*<sub>1</sub>, …}</code> | different symmetries for different hyperedges |

- Properties of a hypergraph *hg* can be accessed as <code>*hg*["*prop*"]</code>; available properties include `"VertexList"`, `"EdgeList"`, `"EdgeListTagged"`, `"EdgeTags"`, `"EdgeSymmetry"`, `"FullEdgeSymmetry"`, `"Arity"`, `"Edges"`, `"Options"` and `"AbsoluteOptions"`.
- Standard graph functions such as [VertexList](), [EdgeList](), [VertexCount](), [EdgeCount](), [VertexDegree](), [Options](), [AbsoluteOptions]() and [AnnotationValue]() work directly on [Hypergraph]() objects.
- Annotation options such as [VertexStyle](), [VertexLabels](), [EdgeStyle]() and [EdgeLabels]() accept a single value applied to all elements, or lists of rules <code>{*elem* -> *value*, …}</code> whose left-hand sides can be patterns.
- [Hypergraph]() takes the following options, in addition to the standard [Graph]() options such as [VertexStyle](), [VertexLabels](), [VertexLabelStyle](), [VertexSize](), [VertexCoordinates](), [VertexShapeFunction](), [EdgeStyle](), [EdgeLabels]() and [EdgeLabelStyle]():

| option | default | effect |
|---|---|---|
| <code>"EdgeSymmetry"</code> | <code>Automatic</code> | symmetry of the hyperedges |
| <code>"EdgeArrows"</code> | <code>False</code> | whether to draw arrows showing the vertex order within each hyperedge |
| <code>"EdgeType"</code> | <code>"Cyclic"</code> | whether the drawn boundary of a hyperedge closes back on itself (`"Cyclic"`) or stays open (`"Bag"`) |
| <code>"EdgeMethod"</code> | <code>"ConcavePolygon"</code> | method used to draw hyperedge regions |
| <code>"EdgeLineStyle"</code> | <code>Automatic</code> | style of hyperedge boundary lines |
| <code>"EdgeSize"</code> | <code>Automatic</code> | size of drawn hyperedge regions |
| <code>"LayoutDimension"</code> | <code>2</code> | dimension of the layout, 2 or 3 |
| <code>PlotTheme</code> | <code>Automatic</code> | overall theme, such as `"Colored"` or `"Dark"` |
| <code>ColorFunction</code> | <code>ColorData[97]</code> | function assigning colors to hyperedges |

## Basic Examples

Create a hypergraph from a list of hyperedges:

```wl
Hypergraph[{{1, 2}, {1, 2, 3}, {3, 4, 5}}]
```

<!-- => Hypergraph object with 5 vertices and 3 edges, displayed graphically -->

---

Give an explicit vertex list to include isolated vertices:

```wl
Hypergraph[{1, 2, 3, 4, 5}, {{1, 2, 3}, {3, 4}}, VertexLabels -> "Name"]
```

<!-- => Hypergraph object with 5 vertices (5 isolated) and 2 edges -->

---

Define a hypergraph:

```wl
hg = Hypergraph[{{1, 2}, {2, 3, 4}}]
```

<!-- => Hypergraph object with 4 vertices and 2 edges -->

Get its hyperedges:

```wl
EdgeList[hg]
```

<!-- => {{1, 2}, {2, 3, 4}} -->

Get its vertices:

```wl
VertexList[hg]
```

<!-- => {1, 2, 3, 4} -->

## Scope

### Construction

Vertices can be arbitrary expressions:

```wl
Hypergraph[{{"a", "b"}, {"b", "c", "d"}}, VertexLabels -> "Name"]
```

<!-- => Hypergraph object on string vertices -->

---

Hyperedges can be empty, contain a single vertex or repeat:

```wl
hg = Hypergraph[{{}, {1}, {1, 2}, {1, 2}}]
```

<!-- => Hypergraph object with 2 vertices and 4 edges -->

The arity of each hyperedge is the number of vertices it contains:

```wl
hg["Arity"]
```

<!-- => {0, 1, 2, 2} -->

---

Convert a [Graph]() to a hypergraph with the same vertices and binary edges:

```wl
Hypergraph[CycleGraph[5], VertexLabels -> "Name"]
```

<!-- => Hypergraph object with 5 vertices and 5 binary edges -->

---

Construct a hypergraph from a `Hyperedges` object:

```wl
Hypergraph[Hyperedges[{1, 2}, {2, 3, 4}]]
```

<!-- => Hypergraph object with 4 vertices and 2 edges -->

---

Make a restyled copy of an existing hypergraph:

```wl
Hypergraph[Hypergraph[{{1, 2, 3}, {3, 4}}], VertexLabels -> "Name", "EdgeArrows" -> True]
```

<!-- => Hypergraph object with labels and edge arrows -->

### Properties

Define a hypergraph:

```wl
hg = Hypergraph[{{1, 2, 3}, {3, 4}, {4}}]
```

<!-- => Hypergraph object with 4 vertices and 3 edges -->

Properties can be accessed by name:

```wl
hg["VertexList"]
```

<!-- => {1, 2, 3, 4} -->

The list of hyperedges:

```wl
hg["EdgeList"]
```

<!-- => {{1, 2, 3}, {3, 4}, {4}} -->

The arity of each hyperedge:

```wl
hg["Arity"]
```

<!-- => {3, 2, 1} -->

The symmetry of each hyperedge:

```wl
hg["EdgeSymmetry"]
```

<!-- => {"Unordered", "Unordered", "Unordered"} -->

Standard graph functions work directly on hypergraphs:

```wl
VertexCount[hg]
```

<!-- => 4 -->

The number of hyperedges:

```wl
EdgeCount[hg]
```

<!-- => 3 -->

The number of hyperedges each vertex belongs to:

```wl
VertexDegree[hg]
```

<!-- => {1, 1, 2, 2} -->

### Edge symmetry

By default hyperedges are unordered, connecting their vertices symmetrically:

```wl
Hypergraph[{{1, 2, 3}}]["EdgeSymmetry"]
```

<!-- => {"Unordered"} -->

---

[DirectedEdge](), [UndirectedEdge]() and `CyclicEdge` wrappers set the symmetry of individual hyperedges:

```wl
hg = Hypergraph[{DirectedEdge[1, 2], UndirectedEdge[2, 3], CyclicEdge[{1, 3, 4}]}, "EdgeArrows" -> True]
```

<!-- => Hypergraph object with 4 vertices and 3 edges -->

Each hyperedge keeps its own symmetry:

```wl
hg["EdgeSymmetry"]
```

<!-- => {"Ordered", "Unordered", "Cyclic"} -->

---

Set a uniform symmetry for all hyperedges with the `"EdgeSymmetry"` option:

```wl
Hypergraph[{{1, 2, 3}, {3, 4, 5}}, "EdgeSymmetry" -> "Ordered"]["EdgeSymmetry"]
```

<!-- => {"Ordered", "Ordered"} -->

---

Give per-edge symmetry rules:

```wl
Hypergraph[{{1, 2, 3}, {3, 4, 5}}, "EdgeSymmetry" -> {{1, 2, 3} -> "Cyclic"}]["EdgeSymmetry"]
```

<!-- => {"Cyclic", "Unordered"} -->

---

The `EdgeSymmetry` function returns the explicit vertex permutations of each hyperedge:

```wl
EdgeSymmetry[Hypergraph[{{1, 2, 3}}, "EdgeSymmetry" -> "Cyclic"]]
```

<!-- => {{Cycles[{}], Cycles[{{1, 2, 3}}], Cycles[{{1, 3, 2}}]}} -->

### Annotations

Wrap a hyperedge in [Labeled]() or [Style]() to annotate it:

```wl
Hypergraph[{Labeled[{1, 2, 3}, "e"], Style[{3, 4}, Red]}]
```

<!-- => Hypergraph object with a labeled and a styled edge -->

---

Vertices in the vertex list can be annotated the same way:

```wl
Hypergraph[{Style[1, Red], Labeled[2, "two"], 3}, {{1, 2}, {2, 3}}]
```

<!-- => Hypergraph object with a styled and a labeled vertex -->

---

Annotation options accept per-element rules whose left-hand sides can be patterns:

```wl
Hypergraph[{{1, 2}, {2, 3}}, VertexStyle -> {1 -> Red, _ -> Blue}, VertexLabels -> "Name"]
```

<!-- => Hypergraph object with vertex 1 red and the rest blue -->

## Options

### EdgeArrows

Show the order of vertices within each hyperedge with arrows:

```wl
Hypergraph[{{3, 1}, {3, 2, 1}, {1, 2, 4}}, "EdgeArrows" -> True, VertexLabels -> "Name"]
```

<!-- => Hypergraph object drawn with arrows along each edge -->

### VertexLabels

Label vertices by name:

```wl
Hypergraph[{{1, 2, 3}, {3, 4}}, VertexLabels -> "Name"]
```

<!-- => Hypergraph object with vertex name labels -->

---

Use rules to label individual vertices:

```wl
Hypergraph[{{1, 2, 3}, {3, 4}}, VertexLabels -> {4 -> "end", _ -> Automatic}]
```

<!-- => Hypergraph object with vertex 4 labeled "end" -->

### EdgeStyle

Style individual hyperedges:

```wl
Hypergraph[{{1, 2, 3}, {3, 4}}, EdgeStyle -> {{1, 2, 3} -> Directive[Red, Opacity[0.3]]}]
```

<!-- => Hypergraph object with the ternary edge drawn red -->

### PlotTheme

Use a dark theme:

```wl
Hypergraph[{{1}, {1, 2}, {2, 3, 4}}, PlotTheme -> "Dark"]
```

<!-- => Hypergraph object drawn white on black -->

### LayoutDimension

Lay out a hypergraph in three dimensions:

```wl
Hypergraph[{{1, 2, 3}, {3, 4, 5}}, "LayoutDimension" -> 3]
```

<!-- => Hypergraph object displayed as 3D graphics -->

## Properties and Relations

[CanonicalHypergraph]() relabels vertices into a canonical form:

```wl
EdgeList[CanonicalHypergraph[Hypergraph[{{x, y}, {y, z}}]]]
```

<!-- => {{1, 3}, {2, 3}} -->

---

[IsomorphicHypergraphQ]() tests whether two hypergraphs are the same up to relabeling of vertices:

```wl
IsomorphicHypergraphQ[Hypergraph[{{x, y}, {y, z}}], Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => True -->

---

Rewrite a hypergraph with a [HypergraphRule]():

```wl
Lookup[HypergraphRule[{{1, 2}}, {{1, 2, 3}}][Hypergraph[{{1, 2}}], "CanonicalizeMethod" -> Full], "Hypergraph"]
```

<!-- => {Hypergraph object with one ternary edge} -->

---

Hypergraphs with many vertices or edges display as a summary box:

```wl
Hypergraph[Table[{i, i + 1, i + 2}, {i, 50}]]
```

<!-- => Hypergraph summary box: 52 vertices, 50 edges -->

## Neat Examples

Hypergraphs of arity 3 render as translucent membranes in 3D:

```wl
Hypergraph3D[{{1, 2, 3}, {3, 4, 5}, {5, 6, 1}, {2, 4, 6}}]
```

<!-- => 3D Hypergraph object -->
