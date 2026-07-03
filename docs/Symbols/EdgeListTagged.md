---
Template: Symbol
Name: EdgeListTagged
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/EdgeListTagged
Keywords: [hyperedges, edge tags, edge list]
SeeAlso: [Hypergraph, HyperedgeList, EdgeMultiplicity, Hyperedge]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[EdgeListTagged]()[*hg*]</code> gives the hyperedges of the hypergraph *hg*, with tagged edges given as <code>*edge* -> *tag*</code>.

<code>[EdgeListTagged]()[*hg*, *patt*]</code> gives only the tagged hyperedges that match the pattern *patt*.

## Details & Options

- An untagged hyperedge appears in <code>[EdgeListTagged]()[*hg*]</code> exactly as it does in <code>[EdgeList]()[*hg*]</code>, as a plain list of vertices; a tagged one appears as <code>*edge* -> *tag*</code>.
- <code>*hg*["EdgeTags"]</code> gives just the tags, in the same order, with `None` for an untagged edge.

## Basic Examples

Get the hyperedges of a hypergraph together with any tags:

```wl
EdgeListTagged[Hypergraph[{{1, 2}, {2, 3}, {1, 4}}]]
```

<!-- => {{1, 2}, {2, 3}, {1, 4}} -->

---

Filter to hyperedges matching a pattern:

```wl
EdgeListTagged[Hypergraph[{{1, 2}, {2, 3}, {1, 4}}], {1, _}]
```

<!-- => {{1, 2}, {1, 4}} -->

## Properties and Relations

Since none of these edges are tagged, `EdgeListTagged` agrees with `EdgeList`:

```wl
hg = Hypergraph[{{1, 2}, {2, 3}}];
EdgeList[hg] === EdgeListTagged[hg]
```

<!-- => True -->

## Possible Issues

Tags supplied directly in an edge specification, such as <code>{1, 2} -> "a"</code>, are not retained by the constructed [Hypergraph](): the current constructor rebuilds its stored edges from the untagged edge list, so `EdgeListTagged` and `EdgeTags` always report the edges as untagged:

```wl
EdgeListTagged[Hypergraph[{1, 2, 3}, {{1, 2} -> "a"}]]
```

<!-- => {{1, 2}} (the tag "a" is dropped) -->
