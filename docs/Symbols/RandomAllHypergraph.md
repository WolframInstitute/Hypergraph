---
Template: Symbol
Name: RandomAllHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/RandomAllHypergraph
Keywords: [random hypergraph, hypergraph signature, uniform sampling]
SeeAlso: [RandomHypergraph, RandomConnectedHypergraph, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[RandomAllHypergraph]()[{*n*, {{$m_1$, $a_1$}, {$m_2$, $a_2$}, …}}]</code> gives a pseudorandom hypergraph on *n* atoms with $m_1$ hyperedges of arity $a_1$, $m_2$ hyperedges of arity $a_2$, etc, with every hyperedge slot drawn independently and uniformly.

<code>[RandomAllHypergraph]()[{Automatic, {{$m_1$, $a_1$}, …}}]</code> picks *n* uniformly at random between 1 and the total number of vertex slots the signature requests.

## Details & Options

- Unlike [RandomHypergraph](), the atom count and the signature are bundled into a single list argument `{n, sig}`, and [RandomAllHypergraph]() takes no options: there is no `"Simple"` or `"Connected"` option, and no [Hypergraph]() options are applied to the result.
- Each hyperedge slot is filled independently and uniformly from 1 through *n*, with no rejection sampling: the result can contain repeated vertices within a hyperedge, duplicate hyperedges, and can be disconnected.
- The result is `Hypergraph[Range[n], edges]`: all *n* vertices stay in the vertex list even if some never appear in an edge, unlike [RandomHypergraph]() which drops unused atoms.
- With `Automatic`, *n* is a random integer between 1 and `Total[Times @@@ sig]`, the total number of vertex slots the signature requests.
- Use [SeedRandom]() to get a reproducible result.

## Basic Examples

Generate a pseudorandom hypergraph on five atoms with three binary hyperedges:

```wl
SeedRandom[1]; RandomAllHypergraph[{5, {{3, 2}}}]
```

<!-- => Hypergraph with edges {{5, 3}, {5, 1}, {2, 1}} -->

## Scope

With `Automatic`, the atom count itself is randomly chosen:

```wl
SeedRandom[2]; RandomAllHypergraph[{Automatic, {{2, 2}}}]
```

<!-- => Hypergraph on {1, 2, 3} with edges {{3, 1}, {3, 2}} -->

## Properties and Relations

The result realizes the requested signature exactly:

```wl
SeedRandom[9]; Length /@ EdgeList[RandomAllHypergraph[{5, {{3, 2}, {1, 3}}}]]
```

<!-- => {2, 2, 2, 3} -->

---

All *n* atoms are kept, even ones that never occur in a hyperedge:

```wl
SeedRandom[4]; VertexCount[RandomAllHypergraph[{6, {{1, 2}}}]]
```

<!-- => 6 -->

---

Seeding the random generator makes the result reproducible:

```wl
(SeedRandom[7]; RandomAllHypergraph[{5, {{3, 2}}}]) === (SeedRandom[7]; RandomAllHypergraph[{5, {{3, 2}}}])
```

<!-- => True -->

## Possible Issues

With no rejection sampling, a hyperedge can repeat a vertex or a hyperedge can be duplicated; here a ternary hyperedge on only two atoms is forced to repeat one:

```wl
SeedRandom[1]; SimpleHypergraphQ[RandomAllHypergraph[{2, {{1, 3}}}]]
```

<!-- => False -->
