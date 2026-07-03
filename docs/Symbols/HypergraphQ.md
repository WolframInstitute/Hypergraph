---
Template: Symbol
Name: HypergraphQ
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphQ
Keywords: [hypergraph validity, type test, predicate]
SeeAlso: [Hypergraph, HypergraphRuleQ, HyperedgesQ, SimpleHypergraphQ]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphQ]()[*expr*]</code> gives `True` if *expr* is a valid [Hypergraph]() object, and `False` otherwise.

## Details & Options

- [HypergraphQ]() returns `True` for any expression with head [Hypergraph]() that was built by the package's own constructors, and `False` for anything else, including an unevaluated or malformed [Hypergraph]()[…] expression.
- Most other functions in the package, such as [EdgeList](), [VertexList]() and [HypergraphRule](), require their [Hypergraph]() arguments to satisfy [HypergraphQ]().

## Basic Examples

A hypergraph built with the [Hypergraph]() constructor is valid:

```wl
HypergraphQ[Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => True -->

---

An expression that is not a hypergraph gives `False`:

```wl
HypergraphQ[42]
```

<!-- => False -->

## Properties and Relations

A [HypergraphRule]() is not itself a hypergraph:

```wl
HypergraphQ[HypergraphRule[{{1, 2}}, {{1, 2, 3}}]]
```

<!-- => False -->

## Possible Issues

The head [Hypergraph]() alone does not make an expression valid; an expression built by hand without going through the package's constructors fails the test:

```wl
HypergraphQ[Unevaluated[Hypergraph[{1, 2}, {{1, 2}}]]]
```

<!-- => False -->
