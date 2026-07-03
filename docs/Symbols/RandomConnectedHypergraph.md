---
Template: Symbol
Name: RandomConnectedHypergraph
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/RandomConnectedHypergraph
Keywords: [random hypergraph, connected hypergraph, hypergraph signature]
SeeAlso: [RandomAllHypergraph, RandomHypergraph, Hypergraph, ConnectedHypergraphQ]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[RandomConnectedHypergraph]()[{*n*, {{$m_1$, $a_1$}, {$m_2$, $a_2$}, …}}]</code> gives a pseudorandom connected hypergraph on at most *n* atoms with $m_1$ hyperedges of arity $a_1$, $m_2$ hyperedges of arity $a_2$, etc.

<code>[RandomConnectedHypergraph]()[{Automatic, {{$m_1$, $a_1$}, …}}]</code> picks *n* uniformly at random, up to the largest atom count a connected hypergraph of that signature can use.

## Details & Options

- Unlike [RandomHypergraph](), the atom count and the signature are bundled into a single list argument `{n, sig}`, and [RandomConnectedHypergraph]() takes no options.
- [RandomConnectedHypergraph]() returns a bare list of hyperedges over the atoms 0 through *n* - 1, not a [Hypergraph]() object; wrap the result in [Hypergraph]() to get one.
- The construction is direct, not rejection sampling: it draws the hyperedges, then repeatedly merges any disconnected pieces by identifying a random atom from one piece with a random atom from another, so the result is connected by construction.
- With `Automatic`, *n* is a random integer between 1 and `Total[Times @@@ sig] - Total[sig[[All, 1]]] + 1`, the largest atom count for which a connected hypergraph of the signature is possible (one shared atom per extra hyperedge).
- Use [SeedRandom]() to get a reproducible result.

## Basic Examples

Generate a pseudorandom connected hypergraph on at most five atoms with three binary hyperedges:

```wl
SeedRandom[1]; RandomConnectedHypergraph[{5, {{3, 2}}}]
```

<!-- => {{1, 2}, {1, 0}, {1, 4}} -->

## Scope

With `Automatic`, the atom count itself is randomly chosen:

```wl
SeedRandom[9]; RandomConnectedHypergraph[{Automatic, {{2, 2}}}]
```

<!-- => {{1, 0}, {0, 1}} -->

## Properties and Relations

The result realizes the requested signature exactly:

```wl
SeedRandom[13]; Sort[Length /@ RandomConnectedHypergraph[{6, {{2, 3}, {1, 2}}}]]
```

<!-- => {2, 3, 3} -->

---

The hyperedges form a single connected component:

```wl
SeedRandom[1]; edges = RandomConnectedHypergraph[{5, {{3, 2}}}]; Length[WeaklyConnectedComponents[UndirectedEdge @@@ Catenate[Partition[#, 2, 1] & /@ edges]]]
```

<!-- => 1 -->

---

Seeding the random generator makes the result reproducible:

```wl
(SeedRandom[7]; RandomConnectedHypergraph[{5, {{3, 2}}}]) === (SeedRandom[7]; RandomConnectedHypergraph[{5, {{3, 2}}}])
```

<!-- => True -->

## Possible Issues

The result is a plain list of edges, not a [Hypergraph](); wrap it to use hypergraph functions on it:

```wl
SeedRandom[1]; Hypergraph[RandomConnectedHypergraph[{5, {{3, 2}}}]]
```

<!-- => Hypergraph on {1, 2, 0, 4} with edges {{1, 2}, {1, 0}, {1, 4}} -->
