---
Template: Symbol
Name: EnumerateWolframModelRules
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/EnumerateWolframModelRules
Keywords: [Wolfram model rules, rule enumeration, hypergraph rewriting, rule signature]
SeeAlso: [EnumerateHypergraphRules, HypergraphRule, RandomHypergraphRule, EnumerateHypergraphs]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[EnumerateWolframModelRules]()[{{$m_1$, $a_1$}, …} -> {{$k_1$, $b_1$}, …}]</code> enumerates canonical Wolfram-model-style rules whose input has $m_1$ hyperedges of arity $a_1$, etc, and whose output has $k_1$ hyperedges of arity $b_1$, etc, each rule given as a plain rule between two lists of hyperedges.

<code>[EnumerateWolframModelRules]()[*sig*, *s*]</code> enumerates rules with exactly *s* distinct atoms.

<code>[EnumerateWolframModelRules]()[*sig*, {*s*, *type*}]</code> enumerates rules with connectivity *type*.

## Details & Options

- [EnumerateWolframModelRules]() is the underlying engine behind [EnumerateHypergraphRules](): the two take the same arguments, but where [EnumerateHypergraphRules]() wraps each result in a [HypergraphRule]() object, [EnumerateWolframModelRules]() returns the bare rule of two hyperedge lists, e.g. `{{1, 2}} -> {{1, 3}, {1, 4}}`.
- The signature is a rule of two hyperedge specifications `{{m1, a1}, {m2, a2}, ...}`, each specifying `m1` hyperedges of arity `a1`, `m2` hyperedges of arity `a2`, etc.
- With no explicit atom count, all rules with up to the maximum number of atoms that the connectivity *type* allows are enumerated, matching the default of the Wolfram Function Repository function `EnumerateWolframModelRules`.
- An explicit atom count *s* selects the rules with exactly *s* distinct atoms; this differs from the Wolfram Function Repository function, whose integer argument means up to *s* atoms.
- Possible connectivity types are:

| type | constraint |
|---|---|
| <code>Automatic</code> | the input is connected, and connected to the output |
| <code>All</code> | input and output are each connected, and connected to each other |
| <code>None</code> | no connectivity constraint |

- <code>[EnumerateWolframModelRules]()[*sig*, *type*]</code> is equivalent to <code>[EnumerateWolframModelRules]()[*sig*, {Automatic, *type*}]</code>.

## Basic Examples

Enumerate the raw rules taking a single binary hyperedge to itself:

```wl
EnumerateWolframModelRules[{{1, 2}} -> {{1, 2}}]
```

<!-- => list of 11 rules of edge lists, e.g. {{1, 1}} -> {{1, 1}} -->

---

Count the rules taking one binary hyperedge to two binary hyperedges:

```wl
Length[EnumerateWolframModelRules[{{1, 2}} -> {{2, 2}}]]
```

<!-- => 73 -->

## Scope

Enumerate rules with exactly two distinct atoms:

```wl
EnumerateWolframModelRules[{{1, 2}} -> {{1, 2}}, 2]
```

<!-- => {{{1, 1}} -> {{1, 2}}, {{1, 1}} -> {{2, 1}}, {{1, 2}} -> {{1, 1}}, {{1, 2}} -> {{1, 2}}, {{1, 2}} -> {{2, 1}}, {{1, 2}} -> {{2, 2}}} -->

---

The connectivity type `None` lifts the connectivity constraint on an exact-atom-count slice:

```wl
Length[EnumerateWolframModelRules[{{1, 2}} -> {{2, 2}}, {3, None}]]
```

<!-- => 50 -->

---

A bare connectivity type applies to the whole default atom-count range:

```wl
Length[EnumerateWolframModelRules[{{1, 2}} -> {{2, 2}}, None]]
```

<!-- => 117 -->

## Properties and Relations

The number of rules with exactly *s* atoms, for *s* up to the connectivity maximum:

```wl
Table[Length[EnumerateWolframModelRules[{{1, 2}} -> {{2, 2}}, s]], {s, 4}]
```

<!-- => {1, 17, 37, 18} -->

---

[EnumerateHypergraphRules]() is [EnumerateWolframModelRules]() with each rule wrapped in [HypergraphRule]():

```wl
EnumerateHypergraphRules[{{1, 2}} -> {{1, 2}}] === (HypergraphRule @@@ EnumerateWolframModelRules[{{1, 2}} -> {{1, 2}}])
```

<!-- => True -->

---

Apply [HypergraphRule]() to a raw rule's input and output to get a [HypergraphRule]() object:

```wl
HypergraphRuleQ[HypergraphRule @@ First[EnumerateWolframModelRules[{{1, 2}} -> {{1, 2}}]]]
```

<!-- => True -->

## Possible Issues

The number of rules grows combinatorially with the signature:

```wl
Length[EnumerateWolframModelRules[{{2, 2}} -> {{2, 2}}]]
```

<!-- => 562 -->

---

An atom count larger than the connectivity maximum gives no rules:

```wl
EnumerateWolframModelRules[{{1, 2}} -> {{2, 2}}, 5]
```

<!-- => {} -->
