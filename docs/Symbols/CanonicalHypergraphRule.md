---
Template: Symbol
Name: CanonicalHypergraphRule
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/CanonicalHypergraphRule
Keywords: [canonical form, hypergraph rule, rule isomorphism, canonicalization]
SeeAlso: [CanonicalHypergraph, HypergraphRule, IsomorphicHypergraphQ, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[CanonicalHypergraphRule]()[*rule*]</code> gives a canonical form of the [HypergraphRule]() *rule*, with the vertices of both sides relabeled by consecutive integers.

<code>[CanonicalHypergraphRule]()[*input*, *output*]</code> gives the canonical form of <code>[HypergraphRule]()[*input*, *output*]</code>.

## Details & Options

- The canonical form is a [HypergraphRule]() whose input and output sides share one common pool of vertices, renumbered $1, 2, \ldots$ together, so that vertices shared between the two sides get consistent labels on both.
- The edge symmetry of each side is preserved in the canonical form; by default, all other annotations and styling options are dropped.
- The following options can be given, with the same meaning as for [CanonicalHypergraph]():

| option | default | effect |
|---|---|---|
| <code>Method</code> | <code>Automatic</code> | the method used to find the canonical vertex relabeling |
| <code>"Annotations"</code> | <code>False</code> | whether to transfer vertex and edge annotations to the canonical form |

## Basic Examples

Find the canonical form of a rule that attaches a new edge to a matched pair of vertices:

```wl
crule = CanonicalHypergraphRule[HypergraphRule[Hypergraph[{{a, b}}], Hypergraph[{{a, b}, {b, c}}]]]
```

<!-- => HypergraphRule displayed as an input -> output diagram -->

Both sides share consistently relabeled vertices, so the matched edge appears the same way on each side:

```wl
{EdgeList[crule["Input"]], EdgeList[crule["Output"]]}
```

<!-- => {{{2, 3}}, {{1, 3}, {2, 3}}} -->

## Scope

[CanonicalHypergraphRule]() also accepts *input* and *output* directly, or a `Rule` between them:

```wl
CanonicalHypergraphRule[Hypergraph[{{a, b}}] -> Hypergraph[{{b, c}}]] === CanonicalHypergraphRule[Hypergraph[{{a, b}}], Hypergraph[{{b, c}}]]
```

<!-- => True -->

## Options

### Method

The default method canonicalizes a graph encoding of each side, as [CanonicalHypergraph]() does:

```wl
EdgeList[CanonicalHypergraphRule[HypergraphRule[Hypergraph[{{1, 2, 3}, {3, 4}}], Hypergraph[{{1, 2}}]]]["Input"]]
```

<!-- => {{1, 4}, {2, 3, 4}} -->

The `"Combinatorial"` method can return a different, but still isomorphic, canonical representative:

```wl
EdgeList[CanonicalHypergraphRule[HypergraphRule[Hypergraph[{{1, 2, 3}, {3, 4}}], Hypergraph[{{1, 2}}]], Method -> "Combinatorial"]["Input"]]
```

<!-- => {{3, 4}, {1, 2, 3}} -->

## Properties and Relations

Isomorphic rules give canonical forms whose input and output sides have identical vertex lists, edge lists and edge symmetry, even though the underlying vertex names differed:

```wl
With[{
    c1 = CanonicalHypergraphRule[HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]]],
    c2 = CanonicalHypergraphRule[HypergraphRule[Hypergraph[{{a, b}}], Hypergraph[{{a, b}, {b, c}}]]]
},
    Through[{VertexList, EdgeList, EdgeSymmetry} @ c1["Input"]] === Through[{VertexList, EdgeList, EdgeSymmetry} @ c2["Input"]] &&
    Through[{VertexList, EdgeList, EdgeSymmetry} @ c1["Output"]] === Through[{VertexList, EdgeList, EdgeSymmetry} @ c2["Output"]]
]
```

<!-- => True -->

## Possible Issues

`"Annotations" -> True` raises an error in the current implementation, so only the default `"Annotations" -> False` is reliable:

```wl
Quiet[Check[CanonicalHypergraphRule[HypergraphRule[Hypergraph[{{a, b}}], Hypergraph[{{b, c}}]], "Annotations" -> True], $Failed]] === $Failed
```

<!-- => True -->

---

Because of this same issue, comparing two canonical rules directly with `===` can report unequal even when their input and output sides are isomorphic as hypergraphs, since a stale, un-relabeled `"EdgeSymmetry"` option value is retained on the input side; compare `VertexList`, `EdgeList` and `EdgeSymmetry` instead of the raw rules:

```wl
CanonicalHypergraphRule[HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]]] === CanonicalHypergraphRule[HypergraphRule[Hypergraph[{{a, b}}], Hypergraph[{{a, b}, {b, c}}]]]
```

<!-- => False -->
