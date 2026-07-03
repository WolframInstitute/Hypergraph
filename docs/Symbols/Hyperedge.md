---
Template: Symbol
Name: Hyperedge
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/Hyperedge
Keywords: [hyperedge, edge symmetry, edge specification]
SeeAlso: [HyperedgeList, EdgeSymmetry, Hypergraph, CyclicEdge]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[Hyperedge]()[*edge*]</code> represents a hyperedge on the vertex list *edge*, with `"Unordered"` symmetry, for use inside a [Hypergraph]() edge specification.

<code>[Hyperedge]()[*edge*, *sym*]</code> represents a hyperedge with the given edge symmetry *sym*.

## Details & Options

- Inside a [Hypergraph]() edge list, `Hyperedge[…]` is equivalent to giving the same edge annotated with [EdgeSymmetry](); it is one of several interchangeable ways to set a hyperedge's symmetry, alongside `CyclicEdge`, [DirectedEdge]() and [UndirectedEdge]().
- [HyperedgeList]() returns the hyperedges of a hypergraph as a list of `Hyperedge[…]` objects, so a `Hyperedge` is both an edge specification and a display form for an existing hyperedge.
- A `Hyperedge` displays with its vertices enclosed in parentheses for `"Unordered"` symmetry, braces for `"Ordered"`, or square brackets for `"Cyclic"`, with the symmetry shown in a tooltip.
- *edge* can itself be tagged as <code>*vertices* -> *tag*</code>.

## Basic Examples

Use `Hyperedge` to give one hyperedge a symmetry different from the rest:

```wl
Hypergraph[{Hyperedge[{1, 2, 3}, "Cyclic"], {3, 4}}]["EdgeSymmetry"]
```

<!-- => {"Cyclic", Automatic} -->

---

Without a second argument, a `Hyperedge` has the default `"Unordered"` symmetry:

```wl
Hypergraph[{Hyperedge[{1, 2, 3}], {3, 4}}]["EdgeSymmetry"]
```

<!-- => {"Unordered", "Unordered"} -->

## Scope

`HyperedgeList` returns each hyperedge of a hypergraph as a `Hyperedge` object carrying its own symmetry:

```wl
HyperedgeList[Hypergraph[{{1, 2, 3}, {3, 4}}, "EdgeSymmetry" -> {{1, 2, 3} -> "Cyclic"}]]
```

<!-- => {Hyperedge[{1, 2, 3}, "Cyclic"], Hyperedge[{3, 4}]} -->

## Properties and Relations

`CyclicEdge` gives the same edge symmetry as an explicit `"Cyclic"` `Hyperedge`:

```wl
Hypergraph[{CyclicEdge[{1, 2, 3}]}]["EdgeSymmetry"] === Hypergraph[{Hyperedge[{1, 2, 3}, "Cyclic"]}]["EdgeSymmetry"]
```

<!-- => True -->
