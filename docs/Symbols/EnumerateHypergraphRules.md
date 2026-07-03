---
Template: Symbol
Name: EnumerateHypergraphRules
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/EnumerateHypergraphRules
Keywords: [Wolfram model rules, rule enumeration, hypergraph rewriting, rule signature]
SeeAlso: [HypergraphRule, RandomHypergraphRule, EnumerateHypergraphs, EnumerateOrderedHypergraphs, HighlightRule]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[EnumerateHypergraphRules]()[{{$m_1$, $a_1$}, …} -> {{$k_1$, $b_1$}, …}]</code> enumerates canonical hypergraph rewrite rules whose input has $m_1$ hyperedges of arity $a_1$, etc, and whose output has $k_1$ hyperedges of arity $b_1$, etc.

<code>[EnumerateHypergraphRules]()[*sig*, *s*]</code> enumerates rules with exactly *s* distinct atoms.

<code>[EnumerateHypergraphRules]()[*sig*, {*s*, *type*}]</code> enumerates rules with connectivity *type*.

## Details & Options

- [EnumerateHypergraphRules]() returns a list of [HypergraphRule]() objects, one representative per equivalence class of Wolfram-model-style rules with the given signature.
- The signature is a rule of two hyperedge specifications `{{m1, a1}, {m2, a2}, ...}`, each specifying `m1` hyperedges of arity `a1`, `m2` hyperedges of arity `a2`, etc.
- With no explicit atom count, all rules with up to the maximum number of atoms that the connectivity *type* allows are enumerated, matching the default of the Wolfram Function Repository function `EnumerateWolframModelRules`.
- An explicit atom count *s* selects the rules with exactly *s* distinct atoms; this differs from the Wolfram Function Repository function, whose integer argument means up to *s* atoms.
- Possible connectivity types are:

| type | constraint |
|---|---|
| <code>Automatic</code> | the input is connected, and connected to the output |
| <code>All</code> | input and output are each connected, and connected to each other |
| <code>None</code> | no connectivity constraint |

- <code>[EnumerateHypergraphRules]()[*sig*, *type*]</code> is equivalent to <code>[EnumerateHypergraphRules]()[*sig*, {Automatic, *type*}]</code>.
- Additional options are forwarded to [HypergraphRule]().

## Basic Examples

Enumerate all canonical rules with one binary hyperedge on each side:

```wl
EnumerateHypergraphRules[{{1, 2}} -> {{1, 2}}]
```

<!-- => list of 11 HypergraphRule objects -->

---

Count the rules taking one binary hyperedge to two binary hyperedges:

```wl
Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}]]
```

<!-- => 73 -->

## Scope

Enumerate rules with exactly two distinct atoms:

```wl
EnumerateHypergraphRules[{{1, 2}} -> {{1, 2}}, 2]
```

<!-- => list of 6 HypergraphRule objects -->

---

Count the rules with exactly three atoms and the default connectivity:

```wl
Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}, 3]]
```

<!-- => 37 -->

---

The connectivity type `None` lifts the connectivity constraint:

```wl
Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}, {3, None}]]
```

<!-- => 50 -->

---

The connectivity type `All` additionally requires each side to be connected on its own:

```wl
Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}, {3, All}]]
```

<!-- => 33 -->

---

A bare connectivity type applies to the whole default atom-count range:

```wl
Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}, None]]
```

<!-- => 117 -->

## Properties and Relations

The number of rules with exactly *s* atoms, for *s* up to the connectivity maximum:

```wl
Table[Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}, s]], {s, 4}]
```

<!-- => {1, 17, 37, 18} -->

---

The default enumeration combines every exact atom-count slice:

```wl
Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}]] == Total[Table[Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}, s]], {s, 4}]]
```

<!-- => True -->

---

Each rule is a [HypergraphRule]() whose sides are [Hypergraph]() objects:

```wl
First[EnumerateHypergraphRules[{{1, 2}} -> {{1, 2}}]]["Input"]
```

<!-- => Hypergraph with the single hyperedge {1, 1} -->

## Possible Issues

The number of rules grows combinatorially with the signature:

```wl
Length[EnumerateHypergraphRules[{{2, 2}} -> {{2, 2}}]]
```

<!-- => 562 -->

---

An atom count larger than the connectivity maximum gives no rules:

```wl
EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}, 5]
```

<!-- => {} -->
