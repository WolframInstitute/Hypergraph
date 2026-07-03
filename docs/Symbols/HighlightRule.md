---
Template: Symbol
Name: HighlightRule
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HighlightRule
Keywords: [hypergraph rewriting, rule matching, highlighting, visualization]
SeeAlso: [HypergraphRule, Hypergraph, SimpleHypergraphPlot, RandomHypergraphRule]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HighlightRule]()[*rule*, *hg*]</code> gives a list of graphics highlighting every way the [HypergraphRule]() *rule* matches the hypergraph *hg*, together with the result of each rewrite.

<code>[HighlightRule]()[*matches*, *hg*]</code> gives graphics for an explicit list of match associations, as returned by <code>*rule*[*hg*]</code>.

## Details & Options

- [HighlightRule]() returns one [Graphics]() per match, showing *hg* with the matched vertices and hyperedges highlighted, an arrow, and the rewritten hypergraph with the newly created vertices and hyperedges highlighted.
- Applying a [HypergraphRule]() to a hypergraph as <code>*rule*[*hg*]</code> returns a list of [Association]() objects, one per match, whose keys include `"Hypergraph"` (the rewrite result), `"MatchVertices"`, `"MatchEdges"`, `"MatchEdgePositions"`, `"NewVertices"`, `"NewEdges"` and `"DeletedVertices"`; [HighlightRule]() accepts such a list in place of the rule.
- Rule-application options such as `"CanonicalizeMethod"` and `"BindingsMethod"` are forwarded to <code>*rule*[*hg*]</code>, and plot options are forwarded to [SimpleHypergraphPlot]().

| option | default | effect |
|---|---|---|
| <code>"HighlightLeftVertexStyle"</code> | <code>Directive[PointSize[0.02], Red]</code> | style of the matched vertices in the input hypergraph |
| <code>"HighlightLeftEdgeStyle"</code> | <code>Directive[Thick, Red, EdgeForm[Directive[Red, Thick]]]</code> | style of the matched hyperedges in the input hypergraph |
| <code>"HighlightRightVertexStyle"</code> | <code>Directive[PointSize[0.02], Red]</code> | style of the newly created vertices in the rewritten hypergraph |
| <code>"HighlightRightEdgeStyle"</code> | <code>Directive[Thick, Red, EdgeForm[Directive[Red, Thick]]]</code> | style of the newly created hyperedges in the rewritten hypergraph |

## Basic Examples

Highlight the matches of a rewriting rule that attaches a new hyperedge, one graphic per match:

```wl
HighlightRule[HypergraphRule[{{1, 2}}, {{1, 2}, {2, 3}}], Hypergraph[{{1, 2}}]]
```

<!-- => {Graphics, Graphics} (the unordered edge matches in both orientations) -->

## Scope

Applying a rule to a hypergraph gives an association for each match:

```wl
matches = HypergraphRule[{{1, 2}}, {{1, 2}, {2, 3}}][Hypergraph[{{1, 2}}]];
Keys[First[matches]]
```

<!-- => {"Hypergraph", "MatchVertices", "MatchEdges", "MatchEdgePositions", "NewVertices", "NewEdges", "DeletedVertices", "RuleVertexMap", "Bindings", "EdgeArities"} -->

Highlight these precomputed matches:

```wl
HighlightRule[matches, Hypergraph[{{1, 2}}]]
```

<!-- => {Graphics, Graphics} -->

## Options

### Highlight styles

Change the highlight color:

```wl
HighlightRule[HypergraphRule[{{1, 2}}, {{1, 2}, {2, 3}}], Hypergraph[{{1, 2}}],
    "HighlightLeftEdgeStyle" -> Directive[Thick, Blue, EdgeForm[Directive[Blue, Thick]]],
    "HighlightRightEdgeStyle" -> Directive[Thick, Blue, EdgeForm[Directive[Blue, Thick]]],
    "HighlightRightVertexStyle" -> Directive[PointSize[0.02], Blue]
]
```

<!-- => {Graphics, Graphics} -->

### CanonicalizeMethod

By default, symmetric matches are all listed; a rule replacing an unordered ternary hyperedge by a triangle matches it in all six vertex orderings:

```wl
Length[HighlightRule[HypergraphRule[{{1, 2, 3}}, {{1, 2}, {2, 3}, {3, 1}}], Hypergraph[{{1, 2, 3}}]]]
```

<!-- => 6 -->

Use `"CanonicalizeMethod" -> Full` to keep only inequivalent matches:

```wl
HighlightRule[HypergraphRule[{{1, 2, 3}}, {{1, 2}, {2, 3}, {3, 1}}], Hypergraph[{{1, 2, 3}}], "CanonicalizeMethod" -> Full]
```

<!-- => {Graphics} -->

## Properties and Relations

[HighlightRule]() gives one graphic per match of the rule:

```wl
Length[HighlightRule[HypergraphRule[{{1, 2}}, {{1, 2}, {2, 3}}], Hypergraph[{{1, 2}, {2, 3}}]]] == Length[HypergraphRule[{{1, 2}}, {{1, 2}, {2, 3}}][Hypergraph[{{1, 2}, {2, 3}}]]]
```

<!-- => True -->
