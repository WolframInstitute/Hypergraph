---
Template: TechNote
Name: HypergraphRewriting
Title: Hypergraph Rewriting
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/tutorial/HypergraphRewriting
Keywords: [hypergraph rewriting, Wolfram model, rule application, multiway]
RelatedGuides: [HypergraphFunctionality]
---

Hypergraph rewriting repeatedly transforms a hypergraph by replacing subhypergraphs that match the left-hand side of a rule with the right-hand side of the rule. Systems of this kind, known as Wolfram models, are studied as models of fundamental physics, and the Hypergraph paclet provides the objects to build, apply, visualize and enumerate such rules.

## Rewriting Rules

A [HypergraphRule]() consists of an input [Hypergraph]() to match and an output [Hypergraph]() to substitute:

```wl
rule = HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]]
```

<!-- => HypergraphRule object displayed as an input -> output diagram -->

The rule displays as a diagram of its input and output sides. Both sides are ordinary [Hypergraph]() objects with all their styling and symmetry options:

```wl
rule["Output"]
```

<!-- => Hypergraph object with 3 vertices and 2 edges -->

## Applying a Rule

Applying a rule to a hypergraph gives one record for every way its input matches:

```wl
matches = HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]][Hypergraph[{{a, b}, {b, c}}]];
Length[matches]
```

<!-- => 4 -->

Each record is an association describing one rewrite:

```wl
Keys[First[matches]]
```

<!-- => {"Hypergraph", "MatchVertices", "MatchEdges", "MatchEdgePositions", "NewVertices", "NewEdges", "DeletedVertices", "RuleVertexMap", "Bindings", "EdgeArities"} -->

The "Hypergraph" key holds the rewritten hypergraph; newly created vertices get fresh names:

```wl
First[matches]["Hypergraph"]
```

<!-- => Hypergraph object with 4 vertices and 3 edges -->

## Edge Symmetry and Matching

The number of matches depends on the edge symmetry of the hypergraph and the rule. By default hyperedges are unordered, so a single binary pattern edge matches each binary edge in both orientations. With "Ordered" symmetry each edge matches only one way:

```wl
orderedRule = HypergraphRule[
    Hypergraph[{{1, 2}}, "EdgeSymmetry" -> "Ordered"],
    Hypergraph[{{1, 2}, {2, 3}}, "EdgeSymmetry" -> "Ordered"]];
Length[orderedRule[Hypergraph[{{a, b}, {b, c}}, "EdgeSymmetry" -> "Ordered"]]]
```

<!-- => 2 -->

---

The rewritten hypergraphs of all ordered matches:

```wl
#["Hypergraph"] & /@ orderedRule[Hypergraph[{{a, b}, {b, c}}, "EdgeSymmetry" -> "Ordered"]]
```

<!-- => {Hypergraph object, Hypergraph object} -->

## Visualizing Matches

[HighlightRule]() renders every match of a rule inside a hypergraph, highlighting the matched edges and the produced edges:

```wl
HighlightRule[
    HypergraphRule[
        Hypergraph[{{1, 2}}, "EdgeSymmetry" -> "Ordered"],
        Hypergraph[{{1, 2}, {2, 3}}, "EdgeSymmetry" -> "Ordered"]],
    Hypergraph[{{a, b}, {b, c}}, "EdgeSymmetry" -> "Ordered"]]
```

<!-- => a list of two Graphics, one per match -->

## Evolution

Iterating a rule produces an evolution of hypergraph states. Here each step applies the first available match:

```wl
evolution = NestList[
    First[HypergraphRule[
        Hypergraph[{{1, 2}}, "EdgeSymmetry" -> "Ordered"],
        Hypergraph[{{1, 2}, {2, 3}}, "EdgeSymmetry" -> "Ordered"]][#]]["Hypergraph"] &,
    Hypergraph[{{1, 2}}, "EdgeSymmetry" -> "Ordered"],
    4]
```

<!-- => a list of 5 Hypergraph objects of growing size -->

The edge counts grow by one at each step:

```wl
Length[EdgeList[#]] & /@ evolution
```

<!-- => {1, 2, 3, 4, 5} -->

Applying all matches simultaneously, or exploring every match branch separately, leads to multiway evolution; the per-match records returned by rule application carry enough information ("MatchEdgePositions", "NewEdges", "DeletedVertices") to build either.

## Enumerating and Sampling Rules

[EnumerateHypergraphRules]() lists all canonical connected rules with a given signature; the signature counts edges of each arity on both sides:

```wl
Length[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}]]
```

<!-- => 73 -->

---

A small sample of the enumerated rules:

```wl
Take[EnumerateHypergraphRules[{{1, 2}} -> {{2, 2}}], 4]
```

<!-- => {HypergraphRule, HypergraphRule, HypergraphRule, HypergraphRule} -->

---

[RandomHypergraphRule]() samples a single rule with a given signature:

```wl
SeedRandom[5];
RandomHypergraphRule[{{2, 2}} -> {{3, 2}}]
```

<!-- => a HypergraphRule with 2 binary input edges and 3 binary output edges -->

The interactive companion to these functions is [HypergraphDraw](), which lets you draw the input and output sides of a rule by hand.
