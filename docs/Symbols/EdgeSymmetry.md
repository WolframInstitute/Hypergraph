---
Template: Symbol
Name: EdgeSymmetry
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/EdgeSymmetry
Keywords: [edge symmetry, permutation group, hyperedge annotation, cycles]
SeeAlso: [Hypergraph, Hyperedge, HyperedgeList, CyclicEdge, EdgeListTagged]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[EdgeSymmetry]()[*hg*]</code> gives the explicit vertex permutations each hyperedge of the hypergraph *hg* is symmetric under.

<code>[EdgeSymmetry]()[*edge*, *sym*]</code> annotates *edge* with the edge symmetry *sym*, for use inside a [Hypergraph]() edge specification.

## Details & Options

- For a hyperedge *e* of arity $k$, <code>[EdgeSymmetry]()[*hg*]</code> gives a list of [Cycles]() objects on $\{1, \ldots, k\}$, one for each vertex permutation of *e* that leaves it equivalent to itself.
- The list corresponds to *hg*'s `"EdgeSymmetry"` option: `"Unordered"` gives every permutation of the vertices, `"Ordered"`/`"Directed"` gives only the identity, and `"Cyclic"` gives the cyclic rotations.
- <code>[EdgeSymmetry]()[*edge*, *sym*]</code> evaluates immediately to <code>[Annotation]()[*edge*, "EdgeSymmetry" -> *sym*]</code>, so it can be spliced directly into a [Hypergraph]() edge list to set the symmetry of one hyperedge, the same way `CyclicEdge` or [DirectedEdge]() do.

## Basic Examples

The symmetry group of a ternary hyperedge with the default `"Unordered"` symmetry is all six permutations of its three vertices:

```wl
EdgeSymmetry[Hypergraph[{{1, 2, 3}}]]
```

<!-- => {{Cycles[{}], Cycles[{{2, 3}}], Cycles[{{1, 2}}], Cycles[{{1, 2, 3}}], Cycles[{{1, 3, 2}}], Cycles[{{1, 3}}]}} -->

---

With `"Ordered"` symmetry, only the identity permutation leaves a hyperedge equivalent to itself:

```wl
EdgeSymmetry[Hypergraph[{{1, 2}}, "EdgeSymmetry" -> "Ordered"]]
```

<!-- => {{Cycles[{}]}} -->

## Scope

With `"Cyclic"` symmetry, a ternary hyperedge is equivalent to its two cyclic rotations:

```wl
EdgeSymmetry[Hypergraph[{{1, 2, 3}}, "EdgeSymmetry" -> "Cyclic"]]
```

<!-- => {{Cycles[{}], Cycles[{{1, 2, 3}}], Cycles[{{1, 3, 2}}]}} -->

---

Annotate a single hyperedge inside a [Hypergraph]() edge list to give it its own symmetry:

```wl
Hypergraph[{EdgeSymmetry[{1, 2, 3}, "Cyclic"], {3, 4}}]["EdgeSymmetry"]
```

<!-- => {"Cyclic", Automatic} -->

## Properties and Relations

Each hyperedge's group of permutations has size equal to the group order implied by its symmetry setting; for `"Unordered"` on an arity-3 edge that is $3! = 6$:

```wl
Length[First[EdgeSymmetry[Hypergraph[{{1, 2, 3}}]]]]
```

<!-- => 6 -->

---

`EdgeSymmetry[edge, sym]` is one of several equivalent ways to set a hyperedge's symmetry; `CyclicEdge` gives the same result as an explicit `"Cyclic"` annotation:

```wl
Hypergraph[{CyclicEdge[{1, 2, 3}]}]["EdgeSymmetry"] === Hypergraph[{EdgeSymmetry[{1, 2, 3}, "Cyclic"]}]["EdgeSymmetry"]
```

<!-- => True -->
