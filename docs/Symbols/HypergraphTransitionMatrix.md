---
Template: Symbol
Name: HypergraphTransitionMatrix
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphTransitionMatrix
Keywords: [transition matrix, random walk, stochastic matrix, hypergraph dynamics]
SeeAlso: [HypergraphIncidenceMatrix, AdjacencyTensor, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphTransitionMatrix]()[*hg*]</code> gives the transition matrix of a random walk on the vertices of the hypergraph *hg*.

## Details & Options

- The result is a row-stochastic $n \times n$ [SparseArray](), with rows and columns following [VertexList]()[*hg*].
- The entry for vertices $v$ and $w$ is proportional to $\sum_e (|e| - 1)$, summed over the hyperedges $e$ containing both $v$ and $w$, where $|e|$ is the number of distinct vertices of $e$; each row is normalized to total 1.
- Equivalently, at each step the walker at $v$ picks an incident hyperedge $e$ with probability proportional to $|e| \, (|e| - 1)$, then moves to a vertex of $e$ chosen uniformly at random, possibly staying at $v$; diagonal entries are therefore generally nonzero.
- Vertex membership is determined from the 0-1 incidence pattern, so repeated occurrences of a vertex within a hyperedge do not increase its weight, while repeated hyperedges count separately.
- The order and symmetry of vertices within hyperedges do not affect the result.

## Basic Examples

Compute the transition matrix of a hypergraph:

```wl
tm = HypergraphTransitionMatrix[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => SparseArray summary box: 3x3, 9 stored elements -->

Display it as a matrix:

```wl
MatrixForm[tm]
```

<!-- => MatrixForm of {{3/8, 3/8, 1/4}, {3/10, 2/5, 3/10}, {1/4, 3/8, 3/8}} -->

Every row sums to 1:

```wl
Total[tm, {2}]
```

<!-- => {1, 1, 1} -->

## Scope

For a hypergraph with only binary hyperedges, the result is a lazy random walk on the corresponding graph, staying in place with probability 1/2:

```wl
MatrixForm @ HypergraphTransitionMatrix[Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => MatrixForm of {{1/2, 1/2, 0}, {1/4, 1/2, 1/4}, {0, 1/2, 1/2}} -->

---

A 3-uniform hypergraph spreads each step uniformly over the vertices of the chosen hyperedge:

```wl
MatrixForm @ HypergraphTransitionMatrix[Hypergraph[{{1, 2, 3}, {2, 3, 4}}]]
```

<!-- => MatrixForm of {{1/3, 1/3, 1/3, 0}, {1/6, 1/3, 1/3, 1/6}, {1/6, 1/3, 1/3, 1/6}, {0, 1/3, 1/3, 1/3}} -->

## Applications

Compute the transition matrix of a hypergraph random walk:

```wl
tm = HypergraphTransitionMatrix[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => SparseArray summary box: 3x3 -->

Iterating the walk from a distribution concentrated at vertex 1 converges to the stationary distribution:

```wl
N @ Nest[# . tm &, {1, 0, 0}, 20]
```

<!-- => {0.307692, 0.384615, 0.307692} -->

## Possible Issues

A vertex belonging to no hyperedge with at least two distinct vertices has no transitions, producing a zero row (and [Power]() messages during the normalization):

```wl
Quiet @ MatrixForm @ HypergraphTransitionMatrix[Hypergraph[{1, 2, 3, 4}, {{1, 2}, {2, 3}}]]
```

<!-- => MatrixForm of {{1/2, 1/2, 0, 0}, {1/4, 1/2, 1/4, 0}, {0, 1/2, 1/2, 0}, {0, 0, 0, 0}} -->
