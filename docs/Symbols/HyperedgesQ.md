---
Template: Symbol
Name: HyperedgesQ
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HyperedgesQ
Keywords: [hyperedges validity, type test, predicate, edge list]
SeeAlso: [Hyperedges, Hyperedge, Hypergraph, HypergraphQ]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HyperedgesQ]()[*expr*]</code> gives `True` if *expr* is a valid [Hyperedges]() object, and `False` otherwise.

## Details & Options

- `Hyperedges[`*e*<sub>1</sub>, *e*<sub>2</sub>, …`]` is the internal edge-list object that [Hypergraph]() builds from an edge specification; each *e*<sub>i</sub> is a hyperedge, optionally tagged as <code>*edge* -> *tag*</code>.
- [HyperedgesQ]() returns `True` for a [Hyperedges]() object built by the package's own constructors, and `False` for anything else, including an unevaluated or malformed `Hyperedges[…]` expression.
- <code>[Hypergraph]()[*he*, *opts*]</code> accepts a `Hyperedges` object *he* directly whenever *he* satisfies [HyperedgesQ]().

## Basic Examples

A `Hyperedges` object built from a list of edges is valid:

```wl
HyperedgesQ[Hyperedges[{1, 2}, {2, 3}]]
```

<!-- => True -->

---

An expression that is not a `Hyperedges` object gives `False`:

```wl
HyperedgesQ[{1, 2}]
```

<!-- => False -->

## Properties and Relations

A valid `Hyperedges` object can be passed directly to [Hypergraph]():

```wl
HypergraphQ[Hypergraph[Hyperedges[{1, 2}, {2, 3}]]]
```

<!-- => True -->
