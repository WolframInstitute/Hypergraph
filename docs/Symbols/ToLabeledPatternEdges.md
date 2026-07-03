---
Template: Symbol
Name: ToLabeledPatternEdges
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/ToLabeledPatternEdges
Keywords: [pattern matching, MultiReplace, edge symmetry, hypergraph rewriting]
SeeAlso: [ToLabeledEdges, ToPatternRules, HypergraphRule, EdgeSymmetry]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[ToLabeledPatternEdges]()[*hg*]</code> gives the pattern form of the hyperedges of the [Hypergraph]() *hg*, suitable for matching with `ResourceFunction["MultiReplace"]`.

## Details & Options

- [ToLabeledPatternEdges]()[*hg*] starts from the same pattern <code>[ToLabeledEdges]()[*hg*, True]</code> builds, and additionally rewrites any hyperedge whose `"EdgeSymmetry"` is not `"Ordered"` (or `"Directed"`) so that its vertex pattern matches regardless of the order the vertices are listed in, using its edge symmetry's own permutation group.
- Optional second and third arguments *vertexCondition* and *edgeCondition* carry the same meaning as the corresponding arguments of [ToLabeledEdges]().
- This is the pattern [HypergraphRuleApply]() feeds to `ResourceFunction["MultiReplace"]` when applying a rule's input side to a hypergraph (see <code>*rule*[*hg*]</code> on [HypergraphRule]()); for a hyperedge whose symmetry is already `"Ordered"`, the result is identical to <code>[ToLabeledEdges]()[*hg*, True]</code>.

## Basic Examples

For a hyperedge with the default `"Unordered"` symmetry, the pattern includes an `Alternatives` over the ways to permute its vertices:

```wl
FreeQ[ToLabeledPatternEdges[Hypergraph[{{1, 2, 3}}]], Alternatives]
```

<!-- => False -->

---

For an `"Ordered"` hyperedge, no such alternative arrangement is added:

```wl
FreeQ[ToLabeledPatternEdges[Hypergraph[{{1, 2, 3}}, "EdgeSymmetry" -> "Ordered"]], Alternatives]
```

<!-- => True -->

## Scope

For an `"Ordered"` hypergraph, [ToLabeledPatternEdges]() gives exactly the same pattern as <code>[ToLabeledEdges]()[*hg*, True]</code>, since there is no alternative vertex arrangement to add:

```wl
With[{hg = Hypergraph[{{1, 2, 3}}, "EdgeSymmetry" -> "Ordered"]},
    ToLabeledPatternEdges[hg] === ToLabeledEdges[hg, True]
]
```

<!-- => True -->
