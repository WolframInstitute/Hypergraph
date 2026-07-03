---
Template: Symbol
Name: ToPatternRules
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/ToPatternRules
Keywords: [pattern rule, SetReplace, Wolfram model, hyperedge rewriting]
SeeAlso: [HypergraphRule, ToLabeledEdges, ToLabeledPatternEdges]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[ToPatternRules]()[*rule*]</code> gives the pattern rule equivalent to the [HypergraphRule]() *rule*, in the <code>{*lhs*} :> Module[{*new*}, {*rhs*}]</code> form used by SetReplace-style hypergraph rewriting.

<code>[ToPatternRules]()[*lhs*, *rhs*]</code> gives the same pattern rule directly from a list of input hyperedges *lhs* and a list of output hyperedges *rhs*.

<code>[ToPatternRules]()[*rules*]</code> maps [ToPatternRules]() over a list of [HypergraphRule]() objects.

## Details & Options

- Every vertex occurring in *lhs* or *rhs* is replaced by an ordinary pattern variable named <code>\[FormalV]*i*</code>, numbered in order of first appearance across *lhs* then *rhs*.
- A vertex that appears only in *rhs* (one created by the rule) becomes a local variable of the resulting `Module`, so a fresh vertex is generated on every application of the rule, matching how a `WolframModel`-style rule creates new atoms.
- The rule's left-hand side is a plain list pattern that matches at the level of a list of hyperedges; apply it to a hyperedge list with `Replace` or `ResourceFunction["MultiReplace"]` to perform the rewrite.

## Basic Examples

Convert a rule that attaches a new edge to a matched pair of vertices into a pattern rule:

```wl
ToPatternRules[HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]]]
```

<!-- => {{\[FormalV]1_, \[FormalV]2_}} :> Module[{\[FormalV]3}, {{\[FormalV]1, \[FormalV]2}, {\[FormalV]2, \[FormalV]3}}] -->

---

Applying the pattern rule to a hyperedge list performs the same rewrite `HypergraphRule` would, generating a fresh vertex name for the new one:

```wl
Replace[{{10, 20}}, ToPatternRules[HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]]]]
```

<!-- => {{10, 20}, {20, N}} for some freshly generated vertex name N -->

## Scope

The same pattern rule is obtained directly from a list of input and output hyperedges, without first constructing a `HypergraphRule`:

```wl
ToPatternRules[{{1, 2}}, {{1, 2}, {2, 3}}]
```

<!-- => {{\[FormalV]1_, \[FormalV]2_}} :> Module[{\[FormalV]3}, {{\[FormalV]1, \[FormalV]2}, {\[FormalV]2, \[FormalV]3}}] -->

---

Given a list of rules, `ToPatternRules` maps over it, giving one pattern rule per input rule:

```wl
Length[ToPatternRules[{HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 3}}]], HypergraphRule[Hypergraph[{{1}}], Hypergraph[{{1, 2}}]]}]]
```

<!-- => 2 -->
