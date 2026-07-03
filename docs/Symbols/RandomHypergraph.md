---
Template: Symbol
Name: RandomHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/RandomHypergraph
Keywords: [random hypergraph, hypergraph signature, random generation]
SeeAlso: [Hypergraph, RandomHypergraphRule, EnumerateHypergraphs, EnumerateOrderedHypergraphs]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[RandomHypergraph]()[*n*, {{$m_1$, $a_1$}, {$m_2$, $a_2$}, …}]</code> gives a pseudorandom hypergraph on *n* atoms with $m_1$ hyperedges of arity $a_1$, $m_2$ hyperedges of arity $a_2$, etc.

## Details & Options

- [RandomHypergraph]() returns a [Hypergraph]() object whose vertices are sampled from the integers 1 through *n*; atoms that end up in no hyperedge do not appear in the result.
- The signature `{{m1, a1}, {m2, a2}, ...}` specifies `m1` hyperedges of arity `a1`, `m2` hyperedges of arity `a2`, etc, where the arity of a hyperedge is the number of vertices it connects.
- [RandomHypergraph]() delegates the sampling to the Wolfram Function Repository function `RandomHypergraph`; the first use in a session downloads it, which requires access to the Wolfram Cloud.
- Candidates are resampled until they satisfy the `"Simple"` and `"Connected"` constraints.
- Options of [Hypergraph](), such as [VertexLabels](), are applied to the result.
- Use [SeedRandom]() to get a reproducible result.
- The following options can be given:

| option | default | effect |
|---|---|---|
| <code>"Simple"</code> | <code>True</code> | resample until the hypergraph is simple (`True`) or non-simple (`False`); `All` accepts any hypergraph |
| <code>"Connected"</code> | <code>All</code> | resample until the hypergraph is connected (`True`) or disconnected (`False`); `All` accepts any hypergraph |

## Basic Examples

Generate a pseudorandom simple hypergraph on five atoms with three binary and one ternary hyperedge:

```wl
SeedRandom[7]; RandomHypergraph[5, {{3, 2}, {1, 3}}]
```

<!-- => Hypergraph with edges {{1, 3}, {3, 4}, {2, 1}, {4, 5, 3}} -->

## Scope

Options of [Hypergraph]() are applied to the result:

```wl
SeedRandom[2]; RandomHypergraph[6, {{2, 3}}, VertexLabels -> Automatic]
```

<!-- => Hypergraph with edges {{2, 6, 5}, {4, 1, 6}} and labeled vertices -->

## Options

### "Connected"

Require the hypergraph to be connected:

```wl
SeedRandom[3]; RandomHypergraph[4, {{3, 2}}, "Connected" -> True]
```

<!-- => Hypergraph with edges {{4, 3}, {3, 2}, {1, 2}} -->

### "Simple"

Allow repeated vertices within a hyperedge and repeated hyperedges:

```wl
SeedRandom[3]; RandomHypergraph[4, {{3, 2}}, "Simple" -> All]
```

<!-- => Hypergraph with edges {{2, 4}, {3, 3}, {3, 1}} -->

## Properties and Relations

The result realizes the requested signature:

```wl
SeedRandom[7]; Length /@ EdgeList[RandomHypergraph[5, {{3, 2}, {1, 3}}]]
```

<!-- => {2, 2, 2, 3} -->

---

Seeding the random generator makes the result reproducible:

```wl
(SeedRandom[7]; RandomHypergraph[5, {{3, 2}, {1, 3}}]) === (SeedRandom[7]; RandomHypergraph[5, {{3, 2}, {1, 3}}])
```

<!-- => True -->

## Possible Issues

If no hypergraph of the signature can satisfy the constraints, [RandomHypergraph]() resamples forever; a simple binary hyperedge is impossible on a single atom:

```wl
TimeConstrained[RandomHypergraph[1, {{1, 2}}], 1]
```

<!-- => $Aborted -->
