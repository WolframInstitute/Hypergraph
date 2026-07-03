---
Template: Symbol
Name: HypermatrixQ
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypermatrixQ
Keywords: [hypermatrix validity, type test, predicate, adjacency arrays]
SeeAlso: [Hypermatrix, HypermatrixGraph, Hypergraph, HypergraphQ]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypermatrixQ]()[*expr*]</code> gives `True` if *expr* is a valid [Hypermatrix]() object, and `False` otherwise.

## Details & Options

- [HypermatrixQ]() returns `True` for a [Hypermatrix]() object built by the package's own constructors, and `False` for anything else.
- Properties such as <code>*hm*["Association"]</code>, <code>*hm*["Arrays"]</code> and <code>*hm*["Dimensions"]</code>, and [HypermatrixGraph](), require their [Hypermatrix]() argument to satisfy [HypermatrixQ]().

## Basic Examples

A hypermatrix built from a hypergraph is valid:

```wl
HypermatrixQ[Hypermatrix[Hypergraph[{{1, 2}, {2, 3}}]]]
```

<!-- => True -->

---

An expression that is not a hypermatrix gives `False`:

```wl
HypermatrixQ[{1, 2, 3}]
```

<!-- => False -->

## Properties and Relations

A [Hypermatrix]() built directly from arity/symmetry rules is valid when its arrays all share the same dimension:

```wl
HypermatrixQ[Hypermatrix[{2, {Cycles[{}]}} -> SparseArray[{{0, 1}, {1, 0}}]]]
```

<!-- => True -->

---

Combining arrays of mismatched dimension gives an invalid `Hypermatrix`:

```wl
HypermatrixQ[Hypermatrix[{2, {Cycles[{}]}} -> SparseArray[{{0, 1}, {1, 0}}], {3, {Cycles[{}]}} -> SparseArray[ConstantArray[0, {3, 3, 3}]]]]
```

<!-- => False -->
