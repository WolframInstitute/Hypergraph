---
Template: Symbol
Name: HypergraphIncidenceMatrix
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphIncidenceMatrix
Keywords: [incidence matrix, hypergraph incidence, sparse array, vertex-edge incidence]
SeeAlso: [IncidenceHypergraph, AdjacencyTensor, HypergraphTransitionMatrix, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphIncidenceMatrix]()[*hg*]</code> gives the incidence matrix of the hypergraph *hg*.

<code>[HypergraphIncidenceMatrix]()[*vertices*, *edges*]</code> gives the incidence matrix of the hypergraph with the given *vertices* and *edges*.

## Details & Options

- The result is a [SparseArray]() matrix with one row per vertex, in the order of [VertexList]()[*hg*], and one column per hyperedge, in the order of [EdgeList]()[*hg*].
- The entry for a vertex and a hyperedge is $\sum_p 2^{p-1}$, summed over the positions $p$ at which the vertex occurs in the hyperedge: bit $p-1$ of the entry records occupancy of position $p$.
- This positional binary encoding makes the matrix a faithful representation of ordered hyperedges: an entry 1 means the vertex is first in the hyperedge, 2 means second, 4 means third, and a vertex occurring at several positions has several bits set.
- An entry 0 means the vertex and the hyperedge are not incident; <code>[Sign]()[*matrix*]</code> gives the ordinary 0-1 incidence matrix.
- [IncidenceHypergraph]() reconstructs the hypergraph exactly from its incidence matrix.
- For a hypergraph with no vertices the result is `{{}}`.

## Basic Examples

Compute the incidence matrix of a hypergraph, displayed with [MatrixForm]():

```wl
MatrixForm @ HypergraphIncidenceMatrix[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => MatrixForm of {{1, 0, 1}, {2, 1, 2}, {0, 2, 4}} -->

---

The entries encode the position of each vertex within each hyperedge, so the two orderings of a binary hyperedge get different columns:

```wl
MatrixForm @ HypergraphIncidenceMatrix[Hypergraph[{{1, 2}, {2, 1}}]]
```

<!-- => MatrixForm of {{1, 2}, {2, 1}} -->

## Scope

An explicit vertex list determines the rows, so vertices occurring in no hyperedge get zero rows:

```wl
MatrixForm @ HypergraphIncidenceMatrix[{1, 2, 3}, {{1, 2}}]
```

<!-- => MatrixForm of {{1}, {2}, {0}} -->

---

A vertex repeated within a hyperedge has one bit set per occupied position:

```wl
MatrixForm @ HypergraphIncidenceMatrix[Hypergraph[{{1, 1, 2}}]]
```

<!-- => MatrixForm of {{3}, {4}}: vertex 1 occupies positions 1 and 2, so 1 + 2 = 3 -->

## Properties and Relations

[Sign]() reduces the positional encoding to plain vertex-edge membership:

```wl
MatrixForm @ Sign @ HypergraphIncidenceMatrix[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => MatrixForm of {{1, 0, 1}, {1, 1, 1}, {0, 1, 1}} -->

---

Start with a hypergraph whose hyperedges are ordered tuples:

```wl
hg = Hypergraph[{{2, 1}, {3, 1, 2}}]
```

<!-- => Hypergraph rendering -->

[IncidenceHypergraph]() inverts [HypergraphIncidenceMatrix]() exactly, including the order of vertices within each hyperedge:

```wl
IncidenceHypergraph[VertexList[hg], HypergraphIncidenceMatrix[hg]] === hg
```

<!-- => True -->
