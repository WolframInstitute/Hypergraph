---
Template: Symbol
Name: KoszulSign
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/KoszulSign
Keywords: [Koszul sign, graded permutation sign, superalgebra, signature, antisymmetrization]
SeeAlso: [HypergraphInsertionBracket, HypergraphInsertion]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[KoszulSign]()[*permutation*]</code> gives the Koszul sign incurred by reordering a sequence of graded elements according to *permutation*, treating every element as degree 0.

<code>[KoszulSign]()[*permutation*, "Degree" -> *deg*]</code> looks up the degree of each element (its label as it appears in *permutation*) through the function *deg*.

## Details & Options

- *permutation* is a list of the labels $1, 2, \ldots, n$ giving the new order of $n$ graded elements.
- For every pair of positions $i < j$ at which *permutation* is inverted (<code>[Part]()[*permutation*, *i*] > [Part]()[*permutation*, *j*]</code>), the sign picks up a factor $(-1)^{p + \deg(a) \deg(b)}$, where $a$ and $b$ are the two elements out of order and $p$ is the "Parity" setting; the result is the product of these factors over every inverted pair.
- Swapping two elements of odd degree contributes a factor of $-1$; swapping two even-degree elements, or an even- and an odd-degree element, contributes $+1$.
- The following options can be given:

| option | default | effect |
|---|---|---|
| <code>"Degree"</code> | <code>Function[x, 0]</code> | function giving the degree of an element from its label |
| <code>"Parity"</code> | <code>0</code> | extra parity added to the exponent of every inversion's sign factor |

## Basic Examples

The Koszul sign of a transposition of two degree-0 elements is 1:

```wl
KoszulSign[{2, 1}]
```

<!-- => 1 -->

---

Setting every element to odd degree, a transposition instead contributes -1:

```wl
KoszulSign[{2, 1}, "Degree" -> (1 &)]
```

<!-- => -1 -->

---

`"Parity" -> 1` flips the sign of a single inversion regardless of degree:

```wl
KoszulSign[{2, 1}, "Parity" -> 1]
```

<!-- => -1 -->

## Scope

With a per-label degree function, only pairs of odd-degree elements pick up a sign; here element 1 is odd and element 2 is even, so their exchange contributes no sign:

```wl
KoszulSign[{2, 1}, "Degree" -> ({1, 0}[[#]] &)]
```

<!-- => 1 -->

---

For longer permutations, `KoszulSign` composes the sign contributed by every inverted pair:

```wl
KoszulSign[{3, 1, 4, 2}, "Degree" -> (1 &)]
```

<!-- => -1 -->

## Properties and Relations

When every element has odd degree, `KoszulSign` coincides with the ordinary permutation sign given by [Signature]():

```wl
AllTrue[Permutations[{1, 2, 3, 4}], KoszulSign[#, "Degree" -> (1 &)] === Signature[#] &]
```

<!-- => True -->

---

When every element has even degree and `"Parity" -> 0`, the Koszul sign is always 1, regardless of the permutation:

```wl
AllTrue[Permutations[{1, 2, 3, 4}], KoszulSign[#] === 1 &]
```

<!-- => True -->
