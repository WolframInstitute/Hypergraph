---
Template: Symbol
Name: HypergraphToGraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphToGraph
Keywords: [graph conversion, directed graph, edge symmetry, hypergraph to graph]
SeeAlso: [OrderedHypergraphToGraph, EdgeSymmetry, AdjacencyTensor, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphToGraph]()[*hg*]</code> gives a directed [Graph]() on the vertices of the hypergraph *hg*, with an edge for every pair of vertices adjacent in some symmetric ordering of a hyperedge.

## Details & Options

- For each hyperedge, [HypergraphToGraph]() enumerates its full edge symmetry group (see [EdgeSymmetry]()), permutes the hyperedge's vertex list by every group element, and adds a [DirectedEdge]() between each pair of vertices adjacent in the permuted list.
- A hyperedge with `"Ordered"` symmetry (for instance a hyperedge given as a [DirectedEdge]()) contributes edges only for its vertices exactly as listed; the default `"Unordered"` symmetry contributes an edge for every ordered pair of vertices in the hyperedge.
- Since a hyperedge can contribute the same directed edge more than once (through different symmetry-group elements), the resulting [Graph]() can carry multi-edges.
- Hyperedges of arity 1 contribute no edges; vertices not covered by a hyperedge remain isolated in the graph.
- Any options given are passed directly to [Graph]().
- [HypergraphToGraph]() requires an actual [Hypergraph]() object; it does not accept a raw edge-spec list.

## Basic Examples

Convert a hypergraph of ordinary directed edges to a graph:

```wl
hg = Hypergraph[{DirectedEdge[1, 2], DirectedEdge[2, 3], DirectedEdge[3, 1]}];
HypergraphToGraph[hg]
```

<!-- => Graph object with 3 vertices and edges 1->2, 2->3, 3->1 -->

---

By default hyperedges are `"Unordered"`, so a pair contributes both directions:

```wl
EdgeList[HypergraphToGraph[Hypergraph[{{1, 2}}]]]
```

<!-- => {DirectedEdge[1, 2], DirectedEdge[2, 1]} -->

## Scope

A ternary hyperedge with the default `"Unordered"` symmetry contributes an edge for every ordered pair of its vertices, so its full symmetry group produces repeated edges:

```wl
EdgeList[HypergraphToGraph[Hypergraph[{{1, 2, 3}}]]]
```

<!-- => {DirectedEdge[1, 2], DirectedEdge[2, 1], DirectedEdge[1, 3], DirectedEdge[3, 1], DirectedEdge[2, 3], DirectedEdge[3, 2], DirectedEdge[1, 3], DirectedEdge[3, 1], DirectedEdge[1, 2], DirectedEdge[2, 1], DirectedEdge[2, 3], DirectedEdge[3, 2]} -->

---

A [CyclicEdge]() gives only the directed triangle, not every ordered pair:

```wl
Union[EdgeList[HypergraphToGraph[Hypergraph[{CyclicEdge[{1, 2, 3}]}]]]]
```

<!-- => {DirectedEdge[1, 2], DirectedEdge[2, 3], DirectedEdge[3, 1]} -->

---

Options are forwarded to [Graph]():

```wl
GraphQ[HypergraphToGraph[Hypergraph[{{1, 2}, {2, 3}}], GraphLayout -> "CircularEmbedding"]]
```

<!-- => True -->

## Properties and Relations

Converting the hypergraph built from a directed graph gives back the same graph:

```wl
g = Graph[{1 -> 2, 2 -> 3, 3 -> 1}];
EdgeList[HypergraphToGraph[Hypergraph[g]]] === EdgeList[g]
```

<!-- => True -->

---

For a binary hypergraph specified with `"Ordered"` symmetry, [HypergraphToGraph]()'s adjacency matrix agrees with [AdjacencyTensor]() once its padding row and column are dropped:

```wl
hgOrd = Hypergraph[{DirectedEdge[1, 2], DirectedEdge[2, 3], DirectedEdge[3, 1]}];
Normal[AdjacencyTensor[hgOrd]][[;; 3, ;; 3]] == Normal[AdjacencyMatrix[HypergraphToGraph[hgOrd]]]
```

<!-- => True -->

## Possible Issues

Unlike [OrderedHypergraphToGraph](), [HypergraphToGraph]() does not accept a raw edge list, only an actual [Hypergraph]() object; it is left unevaluated otherwise:

```wl
HypergraphToGraph[{{1, 2}, {2, 3}}]
```

<!-- => HypergraphToGraph[{{1, 2}, {2, 3}}] (unevaluated) -->
