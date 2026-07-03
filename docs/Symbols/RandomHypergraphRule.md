---
Template: Symbol
Name: RandomHypergraphRule
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/RandomHypergraphRule
Keywords: [random rule, Wolfram model, hypergraph rewriting, random generation]
SeeAlso: [HypergraphRule, EnumerateHypergraphRules, RandomHypergraph, HighlightRule]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[RandomHypergraphRule]()[{{$m_1$, $a_1$}, …} -> {{$k_1$, $b_1$}, …}]</code> gives a pseudorandom hypergraph rewrite rule whose input has $m_1$ hyperedges of arity $a_1$, etc, and whose output has $k_1$ hyperedges of arity $b_1$, etc.

<code>[RandomHypergraphRule]()[*sig*, *n*]</code> uses at most *n* distinct atoms.

## Details & Options

- [RandomHypergraphRule]() returns a [HypergraphRule]() object.
- The signature is a rule of two hyperedge specifications `{{m1, a1}, {m2, a2}, ...}`, each specifying `m1` hyperedges of arity `a1`, `m2` hyperedges of arity `a2`, etc.
- [RandomHypergraphRule]() delegates the sampling to the Wolfram Function Repository function `RandomWolframModel`; the first use in a session downloads it, which requires access to the Wolfram Cloud.
- With no explicit atom count, at most the maximum number of atoms that a connected rule of the signature can have is used.
- <code>[RandomHypergraphRule]()[*sig*, *n*, *type*]</code> additionally specifies a connectivity type (`Automatic`, `All` or `None`), as in [EnumerateHypergraphRules]().
- Additional options are forwarded to [HypergraphRule]().
- Use [SeedRandom]() to get a reproducible result.

## Basic Examples

Generate a pseudorandom rule taking two binary hyperedges to three binary hyperedges:

```wl
SeedRandom[1234]; RandomHypergraphRule[{{2, 2}} -> {{3, 2}}]
```

<!-- => HypergraphRule with input edges {{1, 2}, {2, 3}} and output edges {{4, 5}, {4, 1}, {6, 5}} -->

## Scope

Restrict the rule to at most four distinct atoms:

```wl
SeedRandom[123]; RandomHypergraphRule[{{2, 2}} -> {{3, 2}}, 4]
```

<!-- => HypergraphRule with input edges {{1, 2}, {3, 2}} and output edges {{2, 1}, {2, 3}, {1, 4}} -->

---

Lift the connectivity constraint with the connectivity type `None`:

```wl
SeedRandom[123]; RandomHypergraphRule[{{2, 2}} -> {{3, 2}}, 4, None]
```

<!-- => HypergraphRule with input edges {{1, 2}, {1, 3}} and output edges {{4, 4}, {4, 4}, {2, 2}} -->

## Properties and Relations

The sides of the generated rule are [Hypergraph]() objects:

```wl
SeedRandom[1234]; RandomHypergraphRule[{{2, 2}} -> {{3, 2}}]["Input"]
```

<!-- => Hypergraph with edges {{1, 2}, {2, 3}} -->

---

Seeding the random generator makes the result reproducible:

```wl
(SeedRandom[42]; RandomHypergraphRule[{{2, 2}} -> {{3, 2}}]) === (SeedRandom[42]; RandomHypergraphRule[{{2, 2}} -> {{3, 2}}])
```

<!-- => True -->

## Possible Issues

[RandomHypergraphRule]() requires access to the Wolfram Cloud the first time it is used in a session, in order to download the underlying Wolfram Function Repository function; subsequent calls use the locally cached copy.
