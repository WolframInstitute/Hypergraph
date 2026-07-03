---
Template: Symbol
Name: HypergraphRule
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphRule
Keywords: [rewriting rule, Wolfram model, hypergraph rewriting, pattern matching]
SeeAlso: [Hypergraph, HighlightRule, EnumerateHypergraphRules, RandomHypergraphRule, HypergraphDraw]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphRule]()[*input*, *output*]</code> gives a hypergraph rewriting rule that replaces the hypergraph *input* with the hypergraph *output*.

<code>[HypergraphRule]()[*input*, *output*, *cond*]</code> gives a rule that additionally requires the condition *cond* to hold for a match.

## Details & Options

- <code>[HypergraphRule]()[*input*, *output*]</code> coerces *input* and *output* to [Hypergraph]() and displays the rule as an *input* -> *output* diagram.
- Applying the rule to a hypergraph as <code>*rule*[*hg*]</code> returns a list of match records, one [Association]() per way the input side matches *hg*, with keys `"Hypergraph"`, `"MatchVertices"`, `"MatchEdges"`, `"MatchEdgePositions"`, `"NewVertices"`, `"NewEdges"`, `"DeletedVertices"`, `"RuleVertexMap"`, `"Bindings"` and `"EdgeArities"`.
- The `"Hypergraph"` key holds the rewritten hypergraph; vertices created by the rewrite get fresh names not used elsewhere in the hypergraph.
- <code>*rule*["Input"]</code> and <code>*rule*["Output"]</code> give the two sides as [Hypergraph]() objects; <code>*rule*["Condition"]</code> gives the held condition, `Hold[True]` when no condition was given.
- In <code>[HypergraphRule]()[*input*, *output*, *cond*]</code>, *cond* is only checked for vertices that the input hyperedges label with a shared symbol through [Hypergraph]()'s `VertexLabels` option; a plain vertex position with no such label is unconstrained.
- Rule application takes options from `HypergraphRuleApply` (see `Options[HypergraphRule]` for the full combined option list, which also includes every [Hypergraph]() construction option applied to both sides).

| option | default | effect |
|---|---|---|
| <code>"CanonicalizeMethod"</code> | <code>Automatic</code> | <code>Full</code> deduplicates matches that differ only by a symmetry of the input hyperedges |
| <code>"DistinctVertexLabels"</code> | <code>True</code> | require distinctly labeled input vertices to bind to distinct hypergraph vertices |
| <code>"DistinctEdgeLabels"</code> | <code>True</code> | require distinctly labeled input hyperedges to bind to distinct hypergraph hyperedges |
| <code>"SymmetryMethod"</code> | <code>Automatic</code> | any other value ignores a matched hyperedge's own `"EdgeSymmetry"` tag instead of requiring it to agree with the input |
| <code>"BindingsMethod"</code> | <code>Automatic</code> | <code>First</code> keeps only one assignment of unmatched input vertices instead of enumerating every permutation |
| <code>"MatchesMethod"</code> | <code>Automatic</code> | post-processing applied to the raw list of pattern matches before match records are built |

## Basic Examples

Make a rule that attaches a new hyperedge between two matched vertices:

```wl
rule = HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => HypergraphRule displayed as an input -> output diagram -->

Applying the rule to a hypergraph gives one match record for every way the input side matches:

```wl
matches = rule[Hypergraph[{{a, b}, {b, c}}]];
Length[matches]
```

<!-- => 4 -->

## Scope

Each match record is an association describing one rewrite:

```wl
rule = HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]];
Keys[First[rule[Hypergraph[{{a, b}, {b, c}}]]]]
```

<!-- => {"Hypergraph", "MatchVertices", "MatchEdges", "MatchEdgePositions", "NewVertices", "NewEdges", "DeletedVertices", "RuleVertexMap", "Bindings", "EdgeArities"} -->

---

The `"Hypergraph"` key holds the rewritten hypergraph, with the new vertex given a fresh name:

```wl
match = First[HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]][Hypergraph[{{a, b}, {b, c}}]]];
EdgeList[match["Hypergraph"]]
```

<!-- => {{a, b}, {N, b}, {b, c}} for some freshly generated vertex name N -->

---

Both sides of the rule and its held condition are accessible as properties:

```wl
rule = HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}], x > 0];
{HypergraphQ[rule["Input"]], HypergraphQ[rule["Output"]], rule["Condition"]}
```

<!-- => {True, True, Hold[x > 0]} -->

---

By default hyperedges are unordered, so a binary pattern edge matches an edge in both vertex orders:

```wl
Length[HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]][Hypergraph[{{a, b}, {b, c}}]]]
```

<!-- => 4 -->

---

With `"Ordered"` edge symmetry on both the rule and the target, each edge matches in only one direction:

```wl
orderedRule = HypergraphRule[Hypergraph[{{1, 2}}, "EdgeSymmetry" -> "Ordered"], Hypergraph[{{1, 2}, {2, 3}}, "EdgeSymmetry" -> "Ordered"]];
Length[orderedRule[Hypergraph[{{a, b}, {b, c}}, "EdgeSymmetry" -> "Ordered"]]]
```

<!-- => 2 -->

---

A third argument restricts matches with a condition; labeling the input vertices with their own names lets the condition refer to the bound vertices:

```wl
boundedRule = HypergraphRule[
    Hypergraph[{{x, y}}, "EdgeSymmetry" -> "Ordered", VertexLabels -> {x -> x, y -> y}],
    Hypergraph[{{x, y}, {y, x}}, "EdgeSymmetry" -> "Ordered"],
    x < y
];
target = Hypergraph[{{1, 2}, {2, 1}}, "EdgeSymmetry" -> "Ordered", VertexLabels -> {1 -> 1, 2 -> 2}];
#["Bindings"] & /@ boundedRule[target]
```

<!-- => {{HoldPattern[x] :> 1, HoldPattern[y] :> 2}} -->

## Options

### CanonicalizeMethod

By default, symmetric matches are all listed; a rule replacing an unordered ternary hyperedge by a triangle matches it in all six vertex orderings:

```wl
rule = HypergraphRule[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}, {2, 3}, {3, 1}}]];
target = Hypergraph[{{1, 2, 3}}];
Length[rule[target]]
```

<!-- => 6 -->

Use `"CanonicalizeMethod" -> Full` to keep only one match per equivalence class:

```wl
Length[rule[target, "CanonicalizeMethod" -> Full]]
```

<!-- => 1 -->

## Properties and Relations

[HighlightRule]() renders one graphic per match, so its length agrees with the number of match records that the rule itself returns:

```wl
Length[HighlightRule[HypergraphRule[{{1, 2}}, {{1, 2}, {2, 3}}], Hypergraph[{{1, 2}, {2, 3}}]]] == Length[HypergraphRule[{{1, 2}}, {{1, 2}, {2, 3}}][Hypergraph[{{1, 2}, {2, 3}}]]]
```

<!-- => True -->

---

[EnumerateHypergraphRules]() enumerates canonical rules of a given signature as [HypergraphRule]() objects:

```wl
HypergraphRuleQ[First[EnumerateHypergraphRules[{{1, 2}} -> {{1, 2}}]]]
```

<!-- => True -->

## Possible Issues

A condition is only checked within a single vertex ordering of a pattern hyperedge; with the default unordered symmetry, an alternate ordering that violates the condition can still appear in the match list:

```wl
rule = HypergraphRule[Hypergraph[{{x, y}}, VertexLabels -> {x -> x, y -> y}], Hypergraph[{{x, y}, {y, x}}], x < y];
target = Hypergraph[{{1, 2}, {2, 1}}, VertexLabels -> {1 -> 1, 2 -> 2}];
#["Bindings"] & /@ rule[target]
```

<!-- => {{HoldPattern[x] :> 1, HoldPattern[y] :> 2}, {HoldPattern[x] :> 2, HoldPattern[y] :> 1}} (the second binding violates x < y) -->
