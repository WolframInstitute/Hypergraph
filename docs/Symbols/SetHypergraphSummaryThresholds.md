---
Template: Symbol
Name: SetHypergraphSummaryThresholds
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/SetHypergraphSummaryThresholds
Keywords: [summary box, display threshold, large hypergraph, formatting]
SeeAlso: [Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[SetHypergraphSummaryThresholds]()[*"key"* -> *value*]</code> sets the named threshold used to decide when a [Hypergraph]() displays as a compact summary box instead of a full plot.

## Details & Options

- The recognized keys are `"MaxVertices"`, `"MaxEdges"`, and `"MaxTotalElements"`. A hypergraph switches to a summary box once its vertex count exceeds `"MaxVertices"`, its edge count exceeds `"MaxEdges"`, or their sum exceeds `"MaxTotalElements"`; the predicate [HypergraphLargeQ]() implements this test.
- The defaults are `"MaxVertices" -> 32`, `"MaxEdges" -> 128`, `"MaxTotalElements" -> 196`.
- [SetHypergraphSummaryThresholds]() returns the whole updated association of thresholds, keyed by name.
- Passing more than one rule in a single call raises `AssociateTo::argrx` and leaves the thresholds unchanged, because the rules are spliced directly into a two-argument `AssociateTo` call; set several thresholds with separate one-rule calls.
- An unrecognized key is stored without validation; it has no effect on the display test, which only reads the three keys above.
- The change is global and persists for the rest of the session; there is no option to scope it locally or reset it back to the defaults.

## Basic Examples

Lower `"MaxVertices"` so a twelve-vertex hypergraph switches to a summary box:

```wl
SetHypergraphSummaryThresholds["MaxVertices" -> 10];
HypergraphLargeQ[Hypergraph[Range[12], Hyperedges[{1, 2}]]]
```

<!-- => True -->

## Scope

The return value carries every threshold, not only the one just set:

```wl
SetHypergraphSummaryThresholds["MaxEdges" -> 50]["MaxEdges"]
```

<!-- => 50 -->

## Properties and Relations

An unrecognized key is accepted and stored, but ignored by the display test:

```wl
Keys[SetHypergraphSummaryThresholds["Bogus" -> 1]]
```

<!-- => {MaxVertices, MaxEdges, MaxTotalElements, Bogus} -->

## Possible Issues

Passing more than one rule at once raises `AssociateTo::argrx` and returns the call unevaluated, with no thresholds changed:

```wl
SetHypergraphSummaryThresholds["MaxEdges" -> 5, "MaxVertices" -> 5]
```

<!-- => AssociateTo::argrx message; call returns unevaluated -->
