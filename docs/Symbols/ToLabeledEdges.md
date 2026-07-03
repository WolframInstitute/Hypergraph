---
Template: Symbol
Name: ToLabeledEdges
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/ToLabeledEdges
Keywords: [pattern matching, hyperedge labels, MultiReplace, hypergraph rewriting]
SeeAlso: [ToLabeledPatternEdges, ToPatternRules, HypergraphRule, HighlightRule]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[ToLabeledEdges]()[*vertexLabels*, *edges*]</code> pairs every vertex occurring in the list of hyperedges *edges* with its label from the association *vertexLabels*, giving <code>Labeled[*vertex*, *label*]</code> in its place.

<code>[ToLabeledEdges]()[*vertexLabels*, *edges*, True]</code> instead turns *edges* into a pattern: every vertex becomes a fresh pattern variable wrapped as <code>Labeled[_, *label*_]</code>, so *label* captures whatever value occupies that vertex's position when the pattern is matched.

<code>[ToLabeledEdges]()[*hg*]</code> and <code>[ToLabeledEdges]()[*hg*, True]</code> give the same two forms directly from a [Hypergraph]() *hg*, using its `VertexLabels` annotation as *vertexLabels*.

## Details & Options

- Two vertices given the *same* label symbol become the *same* pattern variable in the result, so ordinary Wolfram Language pattern matching already forces them to bind to equal values; two vertices given *different* label symbols instead get an explicit condition requiring their captured values to differ. This is the mechanism behind [HypergraphRule]()'s `"DistinctVertexLabels"` option.
- The optional third argument (the pattern form's *vertexCondition*, default `True`) can be set to `False` to omit that distinctness condition and allow differently labeled vertices to capture the same value.
- <code>[ToLabeledEdges]()[*hg*, …]</code> additionally accepts an *edgeCondition* argument (default `False`) analogous to *vertexCondition*, but for distinctness among the hyperedges' own labels.
- [ToLabeledEdges]() is the internal representation [HypergraphRuleApply]() (used by <code>*rule*[*hg*]</code>, see [HypergraphRule]()) builds from a hypergraph before handing it, and a pattern built the same way, to `ResourceFunction["MultiReplace"]`; [ToLabeledPatternEdges]() is a convenience wrapper around the pattern form that additionally accounts for a hyperedge's own symmetry.

## Basic Examples

Pair each vertex of a hyperedge with its label:

```wl
ToLabeledEdges[<|1 -> "p", 2 -> "q"|>, {{1, 2}}]
```

<!-- => {{Labeled[1, "p"], Labeled[2, "q"]}} -->

---

Building the pattern form instead, two vertices with different label symbols require their captured values to differ:

```wl
{
    MatchQ[{{Labeled[10, "p"], Labeled[20, "q"]}}, ToLabeledEdges[<|1 -> x, 2 -> y|>, {{1, 2}}, True]],
    MatchQ[{{Labeled[10, "p"], Labeled[20, "p"]}}, ToLabeledEdges[<|1 -> x, 2 -> y|>, {{1, 2}}, True]]
}
```

<!-- => {True, False} -->

---

Giving both vertices the *same* label symbol instead requires their captured values to coincide:

```wl
{
    MatchQ[{{Labeled[10, "p"], Labeled[20, "p"]}}, ToLabeledEdges[<|1 -> x, 2 -> x|>, {{1, 2}}, True]],
    MatchQ[{{Labeled[10, "p"], Labeled[20, "q"]}}, ToLabeledEdges[<|1 -> x, 2 -> x|>, {{1, 2}}, True]]
}
```

<!-- => {True, False} -->

## Scope

Applied directly to a [Hypergraph](), [ToLabeledEdges]() reads vertex labels from its `VertexLabels` annotation:

```wl
ToLabeledEdges[Hypergraph[{{1, 2}}, VertexLabels -> {1 -> "p", 2 -> "q"}]]
```

<!-- => {Labeled[{Labeled[Labeled[1, "p"], Automatic], Labeled[Labeled[2, "q"], Automatic]}, {{Cycles[{}], Cycles[{{1, 2}}]}, None}]} -->

---

With `True` as the second argument, the hypergraph's edges are instead turned into a pattern (a `Condition` wrapping every vertex's fresh pattern variable):

```wl
Head[ToLabeledEdges[Hypergraph[{{1, 2}}, VertexLabels -> {1 -> "p", 2 -> "q"}], True]]
```

<!-- => Condition -->
