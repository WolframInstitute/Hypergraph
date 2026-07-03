---
Template: Symbol
Name: OrderedHypergraphToGraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/OrderedHypergraphToGraph
Keywords: [graph encoding, hypergraph canonicalization, incidence structure, ordered edges]
SeeAlso: [HypergraphToGraph, CanonicalHypergraph, ToOrderedHypergraph, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[OrderedHypergraphToGraph]()[*hg*]</code> gives a [Graph]() encoding of the hypergraph *hg* that distinguishes each hyperedge and preserves the order of vertices within it.

## Details & Options

- The resulting graph's vertices are tagged `{"Vertex",` *v*`}` for each vertex *v* of *hg*, and `{"Hyperedge",` *i*`,` *v*`}` for each vertex *v* of the *i*-th hyperedge; edges chain a hyperedge's vertices, in the order given, from one `"Hyperedge"` node to the next and the last one to the corresponding `"Vertex"` node.
- Because each hyperedge gets its own `"Hyperedge"` nodes, two identical hyperedges remain distinguishable in the graph, unlike in [HypergraphToGraph]().
- [OrderedHypergraphToGraph]() uses the vertex order exactly as stored in [EdgeList]()[*hg*], regardless of *hg*'s [EdgeSymmetry](); it does not enumerate the symmetry group the way [HypergraphToGraph]() does.
- The graph is reduced with [TransitiveReductionGraph](), so only the minimal chain of edges needed to encode each hyperedge's position order is kept.
- This encoding underlies the default `Method -> "Graph"` used by [CanonicalHypergraph]().
- Any options given are passed directly to [Graph]().
- <code>[OrderedHypergraphToGraph]()[*args*]</code> also accepts anything <code>[Hypergraph]()[*args*]</code> accepts, such as a raw list of hyperedges.

## Basic Examples

Convert a hypergraph to its ordered graph encoding:

```wl
hg = Hypergraph[{{1, 2, 3}, {3, 4}}];
OrderedHypergraphToGraph[hg]
```

<!-- => Graph object with "Vertex" and "Hyperedge" nodes -->

Its vertices distinguish the hypergraph's own vertices from per-hyperedge markers, one per vertex of each hyperedge:

```wl
VertexList[OrderedHypergraphToGraph[hg]]
```

<!-- => {{"Vertex", 1}, {"Vertex", 2}, {"Vertex", 3}, {"Vertex", 4}, {"Hyperedge", 1, 1}, {"Hyperedge", 1, 2}, {"Hyperedge", 1, 3}, {"Hyperedge", 2, 3}, {"Hyperedge", 2, 4}} -->

## Scope

A raw list of hyperedges is accepted directly, without an explicit [Hypergraph]() wrapper:

```wl
VertexCount[OrderedHypergraphToGraph[{{1, 2}, {2, 3}}]]
```

<!-- => 7 -->

---

The encoding follows the stored vertex order of a hyperedge even when its declared symmetry is `"Unordered"`:

```wl
EdgeList[OrderedHypergraphToGraph[Hypergraph[{{1, 2, 3}}]]]
```

<!-- => {DirectedEdge[{"Hyperedge", 1, 1}, {"Vertex", 1}], DirectedEdge[{"Hyperedge", 1, 1}, {"Hyperedge", 1, 2}], DirectedEdge[{"Hyperedge", 1, 2}, {"Vertex", 2}], DirectedEdge[{"Hyperedge", 1, 2}, {"Hyperedge", 1, 3}], DirectedEdge[{"Hyperedge", 1, 3}, {"Vertex", 3}]} -->

---

Repeated hyperedges each keep their own `"Hyperedge"` nodes rather than collapsing into one:

```wl
VertexList[OrderedHypergraphToGraph[Hypergraph[{{1, 2}, {1, 2}}]]]
```

<!-- => {{"Vertex", 1}, {"Vertex", 2}, {"Hyperedge", 1, 1}, {"Hyperedge", 1, 2}, {"Hyperedge", 2, 1}, {"Hyperedge", 2, 2}} -->

## Properties and Relations

[OrderedHypergraphToGraph]() is the encoding [CanonicalHypergraph]() uses internally with the default `Method -> "Graph"`; canonical forms computed with this and with `Method -> "Combinatorial"` are always isomorphic, even though the two methods search for the canonical vertex relabeling differently:

```wl
hg = Hypergraph[{{1, 2, 3}, {3, 4}}];
IsomorphicHypergraphQ[CanonicalHypergraph[hg], CanonicalHypergraph[hg, Method -> "Combinatorial"]]
```

<!-- => True -->

---

Restricting the graph to only `"Vertex"` nodes gives no edges directly between them; connectivity always passes through `"Hyperedge"` nodes:

```wl
og = OrderedHypergraphToGraph[Hypergraph[{{1, 2}, {2, 3}}]];
EdgeList[VertexDelete[og, Cases[VertexList[og], {"Hyperedge", __}]]]
```

<!-- => {} -->
