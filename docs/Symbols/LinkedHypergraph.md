---
Template: Symbol
Name: LinkedHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/LinkedHypergraph
Keywords: [linked list visualization, pointer diagram, directed hypergraph, HoldForm]
SeeAlso: [SimpleHypergraphPlot, Hypergraph, HypergraphDraw]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[LinkedHypergraph]()[*edges*]</code> gives a [Hypergraph]() from the edge list *edges*, drawn as a pointer-style diagram in which the second vertex of every edge is shown as a framed box holding its own value.

## Details & Options

- Every edge in *edges* is treated as a link from its first vertex to its second; [LinkedHypergraph]() wraps each edge's second vertex in [HoldForm]() so it becomes its own distinct vertex, displayed as a framed box showing that value, with its ordinary vertex label switched off.
- The result's `"EdgeSymmetry"` defaults to `"Ordered"`, so links are drawn as directed arrows rather than symmetrized; an explicit `"EdgeSymmetry"` given as an option overrides this default.
- Options are otherwise the same as [Hypergraph]()'s.

| option | default | effect |
|---|---|---|
| <code>"NodeStyle"</code> | <code>{FontSize -> 16, FontColor -> Black}</code> | style of the text inside each framed node |
| <code>"NodeFrameStyle"</code> | <code>{Background -> LightBlue, FrameMargins -> {4 {1, 1}, {1, 1}}, FrameStyle -> RGBColor[0.34, 0.39, 0.55, .5], RoundingRadius -> {5, 10}}</code> | [Framed]() options for each node's box |

## Basic Examples

Draw a chain of links as a hypergraph with framed target nodes:

```wl
LinkedHypergraph[{{1, 2}, {2, 3}, {3, 4}}]
```

<!-- => Hypergraph object, rendered as a directed chain with each linked-to node shown as a framed box -->

## Scope

The second vertex of every edge is wrapped in [HoldForm](), becoming a distinct vertex even when the same value also appears as a plain, unwrapped vertex elsewhere:

```wl
lhg = LinkedHypergraph[{{1, 2}, {2, 3}}];
VertexList[lhg]
```

<!-- => {1, HoldForm[2], 2, HoldForm[3]} -->

---

The default `"EdgeSymmetry"` is `"Ordered"`:

```wl
Union[Lookup[Options[LinkedHypergraph[{{1, 2}}]], "EdgeSymmetry"][[All, 2]]]
```

<!-- => {"Ordered"} -->

---

An explicit `"EdgeSymmetry"` option overrides that default:

```wl
Union[Lookup[Options[LinkedHypergraph[{{1, 2}}, "EdgeSymmetry" -> "Unordered"]], "EdgeSymmetry"][[All, 2]]]
```

<!-- => {"Unordered"} -->

---

`"NodeStyle"` changes the style of the text drawn inside each node:

```wl
! FreeQ[Options[LinkedHypergraph[{{1, 2}}, "NodeStyle" -> {FontSize -> 24}]], FontSize -> 24]
```

<!-- => True -->

## Properties and Relations

A vertex reached only as a link target is a different vertex from the same value appearing unwrapped as a link source:

```wl
lhg = LinkedHypergraph[{{1, 2}, {2, 3}}];
MemberQ[VertexList[lhg], 2] && MemberQ[VertexList[lhg], HoldForm[2]]
```

<!-- => True -->
