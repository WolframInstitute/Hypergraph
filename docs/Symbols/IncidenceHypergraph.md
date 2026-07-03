---
Template: Symbol
Name: IncidenceHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/IncidenceHypergraph
Keywords: [incidence matrix, hypergraph construction, vertex-edge incidence, binary encoding]
SeeAlso: [HypergraphIncidenceMatrix, AdjacencyHypergraph, HypermatrixGraph, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[IncidenceHypergraph]()[*matrix*]</code> constructs the hypergraph whose incidence matrix is *matrix*.

<code>[IncidenceHypergraph]()[*vertices*, *matrix*]</code> uses the given list of *vertices* for the matrix rows.

## Details & Options

- Rows of *matrix* correspond to vertices and columns to hyperedges.
- Each entry is interpreted as a binary encoding of positions: bit $p-1$ of the entry for a vertex and a hyperedge (the value $2^{p-1}$) places that vertex at position $p$ of the hyperedge.
- Every position of a hyperedge, from 1 up to its arity, should be claimed by exactly one row; the encoding is the one produced by [HypergraphIncidenceMatrix]().
- <code>[IncidenceHypergraph]()[*matrix*]</code> uses <code>[Range]()[*n*]</code> as the vertices, where $n$ is the number of rows.
- [IncidenceHypergraph]() inverts [HypergraphIncidenceMatrix]() exactly, reproducing the order of hyperedges and the order of vertices within each hyperedge.

## Basic Examples

Construct a hypergraph from an incidence matrix:

```wl
hg = IncidenceHypergraph[{{1, 0, 1}, {2, 1, 2}, {0, 2, 4}}]
```

<!-- => Hypergraph rendering with hyperedges {1, 2}, {2, 3}, {1, 2, 3} -->

Each column becomes a hyperedge, with the entries placing vertices at positions:

```wl
EdgeList[hg]
```

<!-- => {{1, 2}, {2, 3}, {1, 2, 3}} -->

## Scope

An explicit vertex list names the rows:

```wl
EdgeList @ IncidenceHypergraph[{"x", "y", "z"}, {{1, 0}, {2, 1}, {0, 2}}]
```

<!-- => {{"x", "y"}, {"y", "z"}} -->

## Properties and Relations

Start with a hypergraph whose hyperedges are ordered tuples:

```wl
hg = Hypergraph[{{2, 1}, {3, 1, 2}}]
```

<!-- => Hypergraph rendering -->

The round trip through [HypergraphIncidenceMatrix]() reproduces the hypergraph exactly:

```wl
IncidenceHypergraph[VertexList[hg], HypergraphIncidenceMatrix[hg]] === hg
```

<!-- => True -->

## Possible Issues

The entries are positional binary codes, not membership counts, so a plain 0-1 incidence matrix is misread: every 1 claims position 1, and only one vertex per position survives:

```wl
EdgeList @ IncidenceHypergraph[{{1, 0}, {1, 1}, {0, 1}}]
```

<!-- => {{1}, {2}} -->

Encoding the intended positions recovers both binary hyperedges:

```wl
EdgeList @ IncidenceHypergraph[{{1, 0}, {2, 1}, {0, 2}}]
```

<!-- => {{1, 2}, {2, 3}} -->
