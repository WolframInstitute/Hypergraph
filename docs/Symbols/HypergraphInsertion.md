---
Template: Symbol
Name: HypergraphInsertion
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphInsertion
Keywords: [insertion, hypergraph operad, root vertex, Koszul sign, insertion bracket]
SeeAlso: [HypergraphInsertionBracket, HypergraphInsertionBracketDegree, KoszulSign, CanonicalHypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphInsertion]()[*hg1*, *hg2*, …, *hgn*]</code> inserts *hgn* into *hg(n-1)*, the result into *hg(n-2)*, and so on down to *hg1*, at every allowed vertex at each step, giving a list of one-key associations <|*hg* -> *sign*|>, one for each way of performing the chain of insertions.

## Details & Options

- Inserting a hypergraph *hg'* at a vertex *v* of a hypergraph *hg* identifies *v* with the root of *hg'*, which by convention is the first vertex in [VertexList]()[*hg'*]; the rest of *hg'*'s vertices are added as fresh vertices attached the way they were attached to the root in *hg'*.
- [HypergraphInsertion]() is the raw building block behind [HypergraphInsertionBracket](): the bracket symmetrizes (or antisymmetrizes) [HypergraphInsertion]()'s results over every ordering of its arguments and collects the outcomes up to isomorphism, while [HypergraphInsertion]() itself returns the unsymmetrized, uncollected list of insertion terms.
- Each returned association pairs a resulting hypergraph with an integer Koszul sign (see [KoszulSign]()) that keeps track of the sign incurred by inserting into the target's vertex ordering.
- Insertion at a vertex is intended to happen only when that vertex's "Degree" annotation matches the degree of the root of the inserted hypergraph (see [HypergraphInsertionBracketDegree]()); every vertex defaults to degree 0, so plain hypergraphs (with no degree annotations) insert at every vertex.

## Basic Examples

Insert a single edge into every vertex of a triangle:

```wl
Length[HypergraphInsertion[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}]]]
```

<!-- => 3 -->

---

Because a triangle is vertex-transitive, every one of the three insertions gives an isomorphic result:

```wl
EdgeList[CanonicalHypergraph[First @ Keys[HypergraphInsertion[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}]][[1]]]]]
```

<!-- => {{1, 4}, {2, 3, 4}} -->

---

Each term carries a Koszul sign; for plain (ungraded) hypergraphs, every sign is 1:

```wl
Values /@ HypergraphInsertion[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}]]
```

<!-- => {{1}, {1}, {1}} -->

## Scope

With three arguments, [HypergraphInsertion]() folds the insertion right to left, inserting the last argument into the middle one first and then the combined result into the first:

```wl
Length[HypergraphInsertion[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}], Hypergraph[{{1}}]]]
```

<!-- => 6 -->

## Possible Issues

Per-vertex "Degree" annotations are meant to restrict insertion to vertices whose degree matches the inserted root, but in the current implementation the lookup that reads back a hypergraph's "Degree" annotation is stale, so the restriction never takes effect and insertion happens at every vertex regardless of degree (the same underlying issue documented for `"Kind" -> "Rooted"` in [HypergraphInsertionBracket]()):

```wl
Length[HypergraphInsertion[
    Hypergraph[{{1, 2}}, "VertexAnnotationRules" -> {"Degree" -> {1 -> 1, 2 -> 0}}],
    Hypergraph[{{1}}, "VertexAnnotationRules" -> {"Degree" -> {1 -> 1}}]
]]
```

<!-- => 2 -->

Only the vertex with degree 1 should have accepted the degree-1 root of the second hypergraph, so this length should be 1, not 2.
