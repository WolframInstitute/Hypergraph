---
Template: Symbol
Name: HypergraphArityReduce
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphArityReduce
Keywords: [arity reduction, subsets, hyperedge decomposition, graph reduction]
SeeAlso: [Hypergraph, HyperedgeList, EdgeSymmetry, SimpleHypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphArityReduce]()[*hg*, *k*]</code> gives the hypergraph obtained by replacing every hyperedge of *hg* with all of its size-*k* vertex subsets.

## Details & Options

- Each hyperedge of arity *n* is replaced by <code>[Subsets]()[*edge*, {*k*}]</code>, so it contributes $\binom{n}{k}$ hyperedges of arity *k*; a tag on the original hyperedge is copied onto each subset.
- A hyperedge of arity less than *k* contributes no subsets and disappears from the result.
- A hyperedge already of arity *k* contributes only itself, unchanged.
- The vertex list, including isolated vertices, is unchanged.
- Vertex-level options such as [VertexStyle]() are unchanged; the edge-level options [EdgeStyle](), [EdgeLabels](), [EdgeLabelStyle](), and `"EdgeSymmetry"` are remapped from each original hyperedge onto its resulting subsets.
- [HypergraphArityReduce]() requires an actual [Hypergraph]() object; it does not accept a raw edge-spec list.

## Basic Examples

Reduce a ternary hyperedge to all of its pairs, alongside a pair that is already at the target arity:

```wl
hg = Hypergraph[{{1, 2, 3}, {2, 3}}];
EdgeList[HypergraphArityReduce[hg, 2]]
```

<!-- => {{1, 2}, {1, 3}, {2, 3}, {2, 3}} -->

The resulting pairs inherit the `"Unordered"` symmetry of the original ternary edge:

```wl
EdgeSymmetry[HypergraphArityReduce[hg, 2]]
```

<!-- => {{Cycles[{}], Cycles[{{1, 2}}]}, {Cycles[{}], Cycles[{{1, 2}}]}, {Cycles[{}], Cycles[{{1, 2}}]}, {Cycles[{}], Cycles[{{1, 2}}]}} -->

## Scope

Reducing to an arity larger than a hyperedge's own drops that hyperedge:

```wl
EdgeList[HypergraphArityReduce[Hypergraph[{{1, 2, 3}, {2, 3}}], 3]]
```

<!-- => {{1, 2, 3}} -->

---

Reducing to arity 1 gives one singleton-vertex hyperedge per vertex occurrence:

```wl
EdgeList[HypergraphArityReduce[Hypergraph[{{1, 2, 3}, {2, 3}}], 1]]
```

<!-- => {{1}, {2}, {3}, {2}, {3}} -->

---

Isolated vertices are kept even after their only hyperedge is reduced away:

```wl
VertexList[HypergraphArityReduce[Hypergraph[{1, 2, 3, 4}, {{1, 2, 3}}], 2]]
```

<!-- => {1, 2, 3, 4} -->

---

Vertex styling is preserved:

```wl
Lookup[Options[HypergraphArityReduce[Hypergraph[{{a, b, c}}, VertexStyle -> {a -> Red}], 2]], VertexStyle]
```

<!-- => {a -> RGBColor[1, 0, 0]} -->

## Properties and Relations

The number of hyperedges after reducing a single *n*-ary hyperedge to arity *k* is the binomial coefficient $\binom{n}{k}$, here $\binom{4}{2} = 6$:

```wl
EdgeCount[HypergraphArityReduce[Hypergraph[{{1, 2, 3, 4}}], 2]]
```

<!-- => 6 -->

## Possible Issues

[HypergraphArityReduce]() does not accept a raw edge-spec list, only an actual [Hypergraph]() object; it is left unevaluated otherwise:

```wl
HypergraphArityReduce[{{1, 2, 3}}, 2]
```

<!-- => HypergraphArityReduce[{{1, 2, 3}}, 2] (unevaluated) -->
