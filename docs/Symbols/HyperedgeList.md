---
Template: Symbol
Name: HyperedgeList
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HyperedgeList
Keywords: [hyperedges, edge symmetry, edge list]
SeeAlso: [Hyperedge, EdgeSymmetry, EdgeListTagged, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HyperedgeList]()[*hg*]</code> gives the hyperedges of the hypergraph *hg* as a list of [Hyperedge]() objects, each carrying its own edge symmetry.

<code>[HyperedgeList]()[*hg*, *patt*]</code> gives only the hyperedges that match the pattern *patt*.

## Details & Options

- Each element of <code>[HyperedgeList]()[*hg*]</code> is <code>[Hyperedge]()[*edge*, *sym*]</code>, or plain <code>[Hyperedge]()[*edge*]</code> when *sym* is `"Unordered"`.
- <code>[HyperedgeList]()[*hg*]</code> packages the same information as [EdgeListTagged]() and *hg*'s `"EdgeSymmetry"` option together, one [Hyperedge]() per hyperedge.
- Passing the resulting list back to [Hypergraph]() reconstructs a hypergraph with the same edges and symmetries.

## Basic Examples

Get the hyperedges of a hypergraph, each annotated with its symmetry:

```wl
HyperedgeList[Hypergraph[{{1, 2, 3}, {3, 4}}, "EdgeSymmetry" -> {{1, 2, 3} -> "Cyclic"}]]
```

<!-- => {Hyperedge[{1, 2, 3}, "Cyclic"], Hyperedge[{3, 4}]} -->

---

Filter to hyperedges matching a pattern:

```wl
HyperedgeList[Hypergraph[{{1, 2, 3}, {3, 4}, {1, 2}}], Hyperedge[{1, _}]]
```

<!-- => {Hyperedge[{1, 2}]} -->

## Scope

Match on the edge symmetry itself to select only the cyclic hyperedges:

```wl
HyperedgeList[Hypergraph[{{1, 2, 3}, {3, 4}}, "EdgeSymmetry" -> {{1, 2, 3} -> "Cyclic"}], Hyperedge[_, "Cyclic"]]
```

<!-- => {Hyperedge[{1, 2, 3}, "Cyclic"]} -->

## Properties and Relations

Rebuilding a [Hypergraph]() from its own `HyperedgeList` reproduces the same edges and symmetries:

```wl
hg = Hypergraph[{{1, 2, 3}, {3, 4}}, "EdgeSymmetry" -> {{1, 2, 3} -> "Cyclic"}];
EdgeSymmetry[Hypergraph[HyperedgeList[hg]]] === EdgeSymmetry[hg]
```

<!-- => True -->
