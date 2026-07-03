---
Template: Symbol
Name: HypergraphLargeQ
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphLargeQ
Keywords: [summary box, display threshold, predicate, large hypergraph]
SeeAlso: [Hypergraph, SetHypergraphSummaryThresholds, VertexCount, EdgeCount]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphLargeQ]()[*hg*]</code> gives `True` if the hypergraph *hg* is large enough to display as a summary box instead of a graphical plot, and `False` otherwise.

## Details & Options

- [HypergraphLargeQ]() gives `True` when [VertexCount]()[*hg*] exceeds `"MaxVertices"`, [EdgeCount]()[*hg*] exceeds `"MaxEdges"`, or their sum exceeds `"MaxTotalElements"`, using the thresholds stored by [SetHypergraphSummaryThresholds]().
- The default thresholds are `"MaxVertices" -> 32`, `"MaxEdges" -> 128` and `"MaxTotalElements" -> 196`.
- A [Hypergraph]() displays as a summary box in [StandardForm]() exactly when [HypergraphLargeQ]() gives `True` for it; otherwise it displays as a graphical plot.
- [HypergraphLargeQ]() gives `False` for any expression that is not a valid [Hypergraph](), rather than raising an error.

## Basic Examples

A small hypergraph is not large:

```wl
HypergraphLargeQ[Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => False -->

---

A hypergraph with more than the default edge threshold of hyperedges is large:

```wl
HypergraphLargeQ[Hypergraph[Table[{i, i + 1}, {i, 150}]]]
```

<!-- => True -->

## Properties and Relations

Lowering `"MaxVertices"` with [SetHypergraphSummaryThresholds]() makes a previously small hypergraph large:

```wl
SetHypergraphSummaryThresholds["MaxVertices" -> 2];
HypergraphLargeQ[Hypergraph[{{1, 2, 3}}]]
```

<!-- => True -->

## Possible Issues

An expression that is not a hypergraph is never considered large:

```wl
HypergraphLargeQ["not a hypergraph"]
```

<!-- => False -->
