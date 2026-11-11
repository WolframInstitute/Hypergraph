---
Template: Symbol
Name: HypergraphInsertionBracket
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphInsertionBracket
Keywords: [insertion bracket, graded Lie bracket, hypergraph operad, vertex insertion, Koszul sign]
SeeAlso: [HypergraphInsertionBracketDegree, CanonicalHypergraph, IsomorphicHypergraphQ, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphInsertionBracket]()[*hg1*, *hg2*, …]</code> gives the graded antisymmetric insertion bracket of the hypergraphs *hg1*, *hg2*, …, as an association from resulting hypergraphs to integer coefficients.

<code>[HypergraphInsertionBracket]()[*assoc1*, *hg2*, …]</code> extends the bracket linearly whenever an argument is given as an association <|*hg* -> *c*, …|>, representing a formal sum of hypergraphs.

## Details & Options

- The bracket is an association representing the signed sum of all consecutive insertions, collected modulo isomorphism. For vertex insertion, a term has the form $\pm hg_{p_1}\prec_{v_{p_1}}(\cdots\prec (hg_{p_{n-1}}\prec_{v_{p_{n-1}}}hg_{p_n})\cdots)$, where $p$ ranges over all permutations of the arguments and each insertion is summed over the admissible vertices of its target.
- The **vertex insertion** $hg\prec_v hg'$ identifies the vertex $v$ of $hg$ with the **root** of $hg'$, which by convention is the first vertex in [VertexList]()[*hg'*].
- The sign is the product of the **insertion sign**, obtained by applying the Koszul convention to insertion on graded vertices, and the graded antisymmetrization sign of the permutation of the arguments. Each hypergraph is graded by its **root-suspended vertex degree** $|hg| = \deg(hg) - \deg(\text{root})$, where $\deg(hg)$ is the sum of the vertex degrees. See [HypergraphInsertionBracketDegree]().
- A vertex is admissible for insertion if its degree matches the degree of the root of the inserted hypergraph. Vertex degrees come from the `"Degree"` entry of `"VertexAnnotationRules"` and default to zero.
- The default isomorphism is an order-preserving bijection of vertices that induces a bijection of hyperedges. Isomorphic terms are collected by adding their coefficients, and terms with coefficient zero are discarded.
- For two homogeneous inputs in the graded construction, graded antisymmetry takes the form $[hg_1,hg_2] = -(-1)^{|hg_1||hg_2|}[hg_2,hg_1]$.
- The following option can be given:

| option | default | effect |
|---|---|---|
| <code>"Kind"</code> | <code>"Ordered"</code> | which isomorphism classes of hypergraphs the bracket is computed on |

- Possible settings for `"Kind"` include:

| setting | hypergraphs |
|---|---|
| <code>"Ordered"</code> | hypergraphs with ordered vertices |
| <code>"Rooted"</code> | hypergraphs with a distinguished root vertex |
| <code>"Tagged"</code> | hypergraphs with a distinguished hyperedge |

- The **tagged** insertion bracket is analogous to the rooted bracket, with insertion given by identification of hyperedges. The **tagged edge**, which takes the role of the root vertex, is the first edge in the edge list. Inserting a hypergraph into another sums over the target hyperedges of matching arity and the identifications allowed by the symmetry of the inserted hypergraph's tagged edge. The symmetry may be `"Unordered"` (the default), `"Ordered"`, or `"Cyclic"`.
- The ordered and rooted constructions use vertex grading. The rooted construction has a known limitation for mixed degrees. The tagged construction uses ordinary antisymmetrization.

## Basic Examples

Compute the insertion bracket of a triangle and a single edge:

```wl
Dataset[KeyMap[EdgeList, HypergraphInsertionBracket[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}]]]]
```

<!-- => Dataset with 5 rows: {{1,2},{1,3,4}}->1, {{2,3},{1,2,4}}->1, {{3,4},{1,2,3}}->1, {{1,4},{1,2,3}}->-1, {{1,2},{2,3,4}}->-1 -->

---

The bracket of a hypergraph with itself vanishes:

```wl
HypergraphInsertionBracket[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}}]]
```

<!-- => <||> -->

## Scope

The bracket accepts any number of hypergraph arguments, summing consecutive insertions over every permutation of them:

```wl
Length[HypergraphInsertionBracket[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}], Hypergraph[{{1}}]]]
```

<!-- => 12 -->

---

An argument given as an association is a formal sum of hypergraphs, and the bracket distributes linearly over it:

```wl
Dataset[KeyMap[EdgeList, HypergraphInsertionBracket[<|Hypergraph[{{1, 2}, {2, 3}}] -> 2|>, Hypergraph[{{1, 2}}]]]]
```

<!-- => Dataset with 3 rows, each coefficient twice that of HypergraphInsertionBracket[Hypergraph[{{1, 2}, {2, 3}}], Hypergraph[{{1, 2}}]] -->

## Properties and Relations

For these degree-zero hypergraphs, swapping the two arguments negates the bracket, so their sum cancels to zero:

```wl
With[{hg1 = Hypergraph[{{1, 2}, {2, 3}}], hg2 = Hypergraph[{{1, 2}}]},
    DeleteCases[Merge[{HypergraphInsertionBracket[hg1, hg2], HypergraphInsertionBracket[hg2, hg1]}, Total], 0] === <||>
]
```

<!-- => True -->

---

The bracket satisfies the graded Jacobi identity:

```wl
With[{hg1 = Hypergraph[{1, 2, 3}, {{1, 2, 3}}], hg2 = Hypergraph[{1, 2}, {{1, 2}}], hg3 = Hypergraph[{1}, {{1}}], b = HypergraphInsertionBracket[#1, #2] &},
    DeleteCases[Merge[{-b[hg1, b[hg2, hg3]], b[b[hg1, hg2], hg3], b[hg2, b[hg1, hg3]]}, Total], 0] === <||>
]
```

<!-- => True -->

## Possible Issues

In the current implementation, only the default `"Kind" -> "Ordered"` is reliable: `"Kind" -> "Rooted"` and `"Kind" -> "Tagged"` raise errors on this build:

```wl
Quiet[Check[HypergraphInsertionBracket[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}}], "Kind" -> "Rooted"], $Failed]] === $Failed
```

<!-- => True -->
