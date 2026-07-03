---
Template: Symbol
Name: HypergraphRuleQ
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphRuleQ
Keywords: [hypergraph rule validity, type test, predicate, rewriting rule]
SeeAlso: [HypergraphRule, HypergraphQ, HighlightRule, EnumerateHypergraphRules]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphRuleQ]()[*expr*]</code> gives `True` if *expr* is a valid [HypergraphRule]() object, and `False` otherwise.

## Details & Options

- [HypergraphRuleQ]() returns `True` for a [HypergraphRule]() whose input and output sides both satisfy [HypergraphQ](), and `False` for anything else.
- Applying a rule as <code>*rule*[*hg*]</code>, and functions such as [HighlightRule]() and [CanonicalHypergraphRule](), require their [HypergraphRule]() argument to satisfy [HypergraphRuleQ]().

## Basic Examples

A rule built with the [HypergraphRule]() constructor is valid:

```wl
HypergraphRuleQ[HypergraphRule[{{1, 2}}, {{1, 2, 3}}]]
```

<!-- => True -->

---

An expression that is not a hypergraph rule gives `False`:

```wl
HypergraphRuleQ[Hypergraph[{{1, 2}}]]
```

<!-- => False -->

## Properties and Relations

[HypergraphRule]() coerces its arguments to [Hypergraph](), so a plain list of hyperedges on either side still yields a valid rule:

```wl
HypergraphRuleQ[HypergraphRule[{{1, 2}}, {{2, 3}}]]
```

<!-- => True -->
