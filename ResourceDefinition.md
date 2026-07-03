---
Template: Paclet
ResourceType: Paclet
Name: WolframInstitute/Hypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
Description: Hypergraph tools
ContributedBy: Nikolay Murzin and Carlos Zapata Carratala and Pavel Hajek
Keywords: [hypergraph, graph, hypermatrix, edge symmetry, isomorphism, rule, rewriting]
MainGuide: Documentation/English/Guides/HypergraphFunctionality.nb
License: MIT
WolframVersion: 13.2+
Categories: [Graphs & Networks]
Disclosures: [WLSystemSymbols]
SourceControlURL: https://github.com/WolframInstitute/Hypergraph
RelatedResources: [WolframModel, WolframModelPlot, EnumerateHypergraphs]
Links: ["[Wolfram Institute](https://wolframinstitute.org)"]
---

## Usage

The Hypergraph paclet processes hypergraphs as symbolic mathematical structures and produces effective visualizations. Hyperedges are encoded as vertex lists with a per-edge symmetry type ("Ordered", "Unordered", "Cyclic" or "Directed"), and on top of the core `Hypergraph` object the paclet provides interactive drawing, 2D and 3D rendering, canonicalization and isomorphism testing, matrix and tensor representations, exhaustive and random generation, and hypergraph rewriting rules in the style of Wolfram model systems.

- <code>[Hypergraph]()</code>, <code>[Hypergraph3D]()</code>, <code>[HypergraphRule]()</code> construct hypergraphs and rewriting rules
- <code>[SimpleHypergraphPlot]()</code>, <code>[SimpleHypergraphPlot3D]()</code>, <code>[HypergraphDraw]()</code>, <code>[HighlightRule]()</code>, <code>[HypergraphEmbedding]()</code> visualize and edit
- <code>[CanonicalHypergraph]()</code>, <code>[IsomorphicHypergraphQ]()</code> canonicalize and compare
- <code>[EnumerateHypergraphs]()</code>, <code>[EnumerateOrderedHypergraphs]()</code>, <code>[EnumerateHypergraphRules]()</code>, <code>[RandomHypergraph]()</code>, <code>[RandomHypergraphRule]()</code> generate hypergraphs and rules
- <code>[AdjacencyTensor]()</code>, <code>[AdjacencyHypergraph]()</code>, <code>[HypergraphIncidenceMatrix]()</code>, <code>[IncidenceHypergraph]()</code>, <code>[HypergraphTransitionMatrix]()</code>, <code>[Hypermatrix]()</code>, <code>[HypermatrixGraph]()</code> convert to and from arrays
- <code>[HypergraphInsertionBracket]()</code>, <code>[HypergraphInsertionBracketDegree]()</code> algebraic operations

## Details & Options

- A [Hypergraph]() displays as an interactive plot; its vertices, edges, symmetries and styling are accessed through properties such as `"VertexList"`, `"EdgeList"` and `"EdgeSymmetry"`.
- [HypergraphRule]() objects apply directly to hypergraphs, returning one rewrite record per match.
- Enumeration functions generate canonical representatives: every hypergraph or rule with the requested signature appears exactly once up to isomorphism.


## Basic Examples

A hypergraph with a unary, a binary and a ternary edge:

```wl
h = Hypergraph[{{1}, {1, 2}, {1, 3, 4}}]
```

<!-- => Hypergraph object rendered as a plot -->

Every edge carries a symmetry type:

```wl
EdgeSymmetry[h]
```

<!-- => list of per-edge symmetry rules -->

---

A random hypergraph with a mixed signature:

```wl
SeedRandom[42];
h1 = RandomHypergraph[10, {{3, 1}, {5, 2}, {4, 3}}]
```

<!-- => Hypergraph object -->

Adding hypergraphs takes their union:

```wl
SeedRandom[43];
h1 + RandomHypergraph[10, {{3, 2}}]
```

<!-- => Hypergraph object -->

---

The adjacency tensor of a hypergraph round-trips back to a hypergraph:

```wl
SeedRandom[44];
hg = RandomHypergraph[6, {{4, 2}, {2, 3}}];
AdjacencyHypergraph[AdjacencyTensor[hg]]
```

<!-- => Hypergraph object -->

---

Enumerate all connected hypergraphs made of two binary edges:

```wl
EnumerateHypergraphs[{{2, 2}}]
```

<!-- => list of Hypergraph objects -->

---

Apply a rewriting rule to a hypergraph:

```wl
HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]][Hypergraph[{{a, b}}]] // Map[#["Hypergraph"] &]
```

<!-- => list of rewritten Hypergraph objects -->

## Hero Image

```wl
SeedRandom[1234];
SimpleHypergraphPlot3D @ RandomHypergraph[25, {{3, 2}, {3, 3}, {3, 4}, {3, 5}}, VertexLabels -> Automatic, PlotTheme -> "Dark"]
```

## Author Notes

The literate-markdown documentation sources under `docs/` were drafted with the assistance of Claude (Anthropic), supervised and reviewed by Nikolay Murzin; the paclet code is human-written.
