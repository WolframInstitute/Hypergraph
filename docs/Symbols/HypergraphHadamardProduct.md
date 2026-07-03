---
Template: Symbol
Name: HypergraphHadamardProduct
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphHadamardProduct
Keywords: [Hadamard product, hyperedge intersection, NonCommutativeMultiply, multiplicity]
SeeAlso: [HypergraphUnion, Hypergraph, EdgeMultiplicity, EdgeList]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphHadamardProduct]()[*hg1*, *hg2*, …]</code> gives the hypergraph whose vertices are the union of the vertices of *hg1*, *hg2*, … and whose hyperedges are those common to all of the arguments, with multiplicity the product of their multiplicities.

## Details & Options

- [HypergraphHadamardProduct]() backs `NonCommutativeMultiply` on [Hypergraph]() objects, so <code>*hg1* \[NonCommutativeMultiply] *hg2*</code> and <code>[HypergraphHadamardProduct]()[*hg1*, *hg2*]</code> are the same operation.
- A hyperedge that appears $k_1$ times in *hg1* and $k_2$ times in *hg2* appears $k_1 \, k_2$ times in the product; a hyperedge missing from either argument does not appear in the product at all.
- For more than two arguments, the product is computed by folding <code>[HypergraphHadamardProduct]()[*hg1*, *hg2*]</code> pairwise over the arguments.

## Basic Examples

Take the Hadamard product of two hypergraphs that share an edge:

```wl
EdgeList[HypergraphHadamardProduct[Hypergraph[{{1, 2}, {2, 3}}], Hypergraph[{{1, 2}, {1, 2}}]]]
```

<!-- => {{1, 2}, {1, 2}} -->

The shared edge `{1, 2}` appears once in the first hypergraph and twice in the second, so it appears twice in the product; `{2, 3}`, present only in the first argument, is dropped.

---

[HypergraphHadamardProduct]() backs `NonCommutativeMultiply`, so multiplying two hypergraphs gives the same result:

```wl
EdgeList[Hypergraph[{{1, 2}, {2, 3}}] ** Hypergraph[{{1, 2}, {1, 2}}]]
```

<!-- => {{1, 2}, {1, 2}} -->

## Scope

Hypergraphs with no common hyperedges give an empty edge list, but the vertex sets still union:

```wl
{EdgeList[#], VertexList[#]} & @ HypergraphHadamardProduct[Hypergraph[{{1, 2}}], Hypergraph[{{3, 4}}]]
```

<!-- => {{}, {1, 2, 3, 4}} -->

---

[HypergraphHadamardProduct]() accepts any number of hypergraphs, folding the product pairwise over them:

```wl
EdgeList[HypergraphHadamardProduct[Hypergraph[{{1, 2}, {1, 2}}], Hypergraph[{{1, 2}, {1, 2}}], Hypergraph[{{1, 2}}]]]
```

<!-- => {{1, 2}, {1, 2}, {1, 2}, {1, 2}} -->

---

A vertex style set on only one argument is transferred to the product:

```wl
Lookup[Options[HypergraphHadamardProduct[Hypergraph[{{a, b}}, VertexStyle -> {a -> Red}], Hypergraph[{{a, b}}]]], VertexStyle][[1]]
```

<!-- => Verbatim[a] -> RGBColor[1, 0, 0] -->
