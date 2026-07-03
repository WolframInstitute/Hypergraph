---
Template: Symbol
Name: HypergraphIncidence
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphIncidence
Keywords: [incidence, vertex degree, hyperedge membership, hypergraph]
SeeAlso: [HypergraphIncidenceMatrix, EdgeSymmetry, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphIncidence]()[*hg*]</code> gives an association from each vertex of *hg* to the list of hyperedges it belongs to.

## Details & Options

- The keys of the result are the vertices of *hg*, in the order given by [VertexList](); the value at each vertex is the list of hyperedges of *hg* containing it, in [EdgeList]() order.
- A vertex belonging to no hyperedge gets the empty list `{}`.
- [VertexDegree]() of *hg* is the length of each vertex's incidence list.

## Basic Examples

Find, for each vertex, the hyperedges it belongs to:

```wl
Normal @ HypergraphIncidence[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => {1 -> {{1, 2}, {1, 2, 3}}, 2 -> {{1, 2}, {2, 3}, {1, 2, 3}}, 3 -> {{2, 3}, {1, 2, 3}}} -->

## Scope

A vertex that belongs to no hyperedge gets an empty incidence list:

```wl
Normal @ HypergraphIncidence[Hypergraph[{1, 2, 3, 4}, {{1, 2}, {2, 3}}]]
```

<!-- => {1 -> {{1, 2}}, 2 -> {{1, 2}, {2, 3}}, 3 -> {{2, 3}}, 4 -> {}} -->

---

The keys follow the hypergraph's own vertex order:

```wl
Keys[HypergraphIncidence[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]] === VertexList[Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}]]
```

<!-- => True -->

## Properties and Relations

The length of each vertex's incidence list is its [VertexDegree]():

```wl
hg = Hypergraph[{{1, 2}, {2, 3}, {1, 2, 3}}];
Values[Length /@ HypergraphIncidence[hg]] === VertexDegree[hg]
```

<!-- => True -->

## Possible Issues

[HypergraphIncidence]() requires an actual [Hypergraph]() object; it is left unevaluated on a raw edge list:

```wl
HypergraphIncidence[{{1, 2}, {2, 3}}]
```

<!-- => HypergraphIncidence[{{1, 2}, {2, 3}}] (unevaluated) -->
