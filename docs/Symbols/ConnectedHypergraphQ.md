---
Template: Symbol
Name: ConnectedHypergraphQ
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/ConnectedHypergraphQ
Keywords: [connectivity, connected hypergraph, predicate]
SeeAlso: [Hypergraph, SimpleHypergraphQ, HypergraphToGraph, EnumerateHypergraphs]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[ConnectedHypergraphQ]()[*hg*]</code> gives `True` if the hypergraph *hg* is connected, and `False` otherwise.

## Details & Options

- [ConnectedHypergraphQ]() replaces each hyperedge with a path linking consecutive vertices (a hyperedge of arity *k* contributes $k-1$ ordinary edges), builds a [Graph]() on the same vertices, and tests it with [ConnectedGraphQ]().
- A single vertex, whether isolated or from an arity-1 hyperedge, is trivially connected.
- An empty hyperedge contributes no edges, so it neither connects nor disconnects the hypergraph.

## Basic Examples

A hypergraph whose hyperedges share vertices is connected:

```wl
ConnectedHypergraphQ[Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => True -->

---

A hypergraph split into two disjoint hyperedges is not connected:

```wl
ConnectedHypergraphQ[Hypergraph[{{1, 2}, {3, 4}}]]
```

<!-- => False -->

## Scope

A single ternary hyperedge already connects all three of its vertices:

```wl
ConnectedHypergraphQ[Hypergraph[{{1, 2, 3}}]]
```

<!-- => True -->

---

An isolated vertex with no hyperedges disconnects an otherwise connected hypergraph:

```wl
ConnectedHypergraphQ[Hypergraph[{1, 2, 3}, {{1, 2}}]]
```

<!-- => False -->

## Properties and Relations

[ConnectedHypergraphQ]() agrees with [ConnectedGraphQ]() applied to [HypergraphToGraph]() of the same hypergraph:

```wl
ConnectedHypergraphQ[Hypergraph[{{1, 2}, {2, 3}}]] === ConnectedGraphQ[HypergraphToGraph[Hypergraph[{{1, 2}, {2, 3}}]]]
```

<!-- => True -->
