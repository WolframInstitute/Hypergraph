---
Template: Symbol
Name: HypergraphInsertionBracketDegree
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphInsertionBracketDegree
Keywords: [vertex degree, grading, Koszul sign, insertion bracket]
SeeAlso: [HypergraphInsertionBracket, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphInsertionBracketDegree]()[*hg*]</code> gives the grading degree of the hypergraph *hg* used by [HypergraphInsertionBracket]().

## Details & Options

- The degree is $|hg| = \deg(hg) - \deg(\text{root})$: the sum of the vertex degrees of *hg*, minus the degree of its root vertex, which by convention is the first vertex in [VertexList]()[*hg*].
- Vertex degrees are read from the `"Degree"` vertex annotation, set with the option <code>"VertexAnnotationRules" -> {"Degree" -> {*v* -> *d*, …}}</code>; a vertex with no explicit degree defaults to 0.
- [HypergraphInsertionBracket]() uses this degree, together with the `KoszulSign` convention, to sign the terms of its `"Ordered"` and `"Rooted"` insertion brackets, and to restrict insertion to vertices of matching degree.
- [HypergraphInsertionBracketDegree]() takes no options.

## Basic Examples

A hypergraph with no `"Degree"` annotation has degree 0:

```wl
HypergraphInsertionBracketDegree[Hypergraph[{{1, 2, 3}}]]
```

<!-- => 0 -->

---

A hypergraph made only of isolated vertices also has degree 0:

```wl
HypergraphInsertionBracketDegree[Hypergraph[{1, 2}, {}]]
```

<!-- => 0 -->

## Scope

A non-Hypergraph argument is left unevaluated:

```wl
HypergraphInsertionBracketDegree[{{1, 2}}]
```

<!-- => unevaluated, since the argument is not a Hypergraph -->

## Possible Issues

In the current implementation, the `"Degree"` annotation set through `"VertexAnnotationRules"` has no effect: [HypergraphInsertionBracketDegree]() always evaluates as if every vertex had degree 0, even when explicit nonzero degrees are given (here the formula $\deg(hg) - \deg(\text{root})$ with degrees $\{1 \to 0, 2 \to 0, 3 \to 1\}$ and root $1$ would give $1 - 0 = 1$):

```wl
HypergraphInsertionBracketDegree[Hypergraph[{1, 2, 3}, {{1, 2, 3}}, "VertexAnnotationRules" -> {"Degree" -> {1 -> 0, 2 -> 0, 3 -> 1}}]]
```

<!-- => 0 -->
