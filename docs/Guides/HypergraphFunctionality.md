---
Template: Guide
Name: HypergraphFunctionality
Title: Hypergraph Functionality
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/guide/HypergraphFunctionality
Description: Create, visualize, transform and enumerate hypergraphs and hypergraph rewriting rules
Keywords: [hypergraph, hyperedge, rewriting, Wolfram model, enumeration, visualization]
---

## Abstract

The Hypergraph paclet processes hypergraphs as symbolic mathematical structures and produces effective visualizations. A [Hypergraph]() encodes its hyperedges as vertex lists, each carrying a symmetry type ("Ordered", "Unordered", "Cyclic" or "Directed") that determines which vertex arrangements represent the same edge. On top of the core object the paclet provides interactive drawing, 2D and 3D rendering, canonicalization and isomorphism testing, matrix and tensor representations, exhaustive and random generation, and hypergraph rewriting rules in the style of Wolfram model systems.

## Functions

### Core objects

- `Hypergraph` construct a hypergraph from a list of hyperedges, with per-edge symmetry and styling
- `Hypergraph3D` construct a hypergraph with a three-dimensional layout
- `Hyperedges` the edge container underlying a hypergraph
- `Hyperedge`, `CyclicEdge` edge specifications with explicit symmetry
- `HypergraphRule` a rewriting rule transforming matches of one hypergraph into another

### Visualization

- `SimpleHypergraphPlot`, `SimpleHypergraphPlot3D` render a hypergraph in 2D or 3D
- `HypergraphDraw`, `HypergraphRuleDraw` draw and edit hypergraphs and rules interactively
- `HighlightRule` highlight the matches of a rule inside a hypergraph
- `HypergraphEmbedding` vertex coordinates of a hypergraph embedding
- `LinkedHypergraph` display a hypergraph as a linked, node-framed diagram
- `SetHypergraphSummaryThresholds` configure when large hypergraphs display as summary boxes

### Properties and predicates

- `HypergraphQ`, `HypergraphRuleQ`, `HyperedgesQ`, `HypermatrixQ` validity predicates
- `SimpleHypergraphQ`, `ConnectedHypergraphQ`, `HypergraphLargeQ` structural predicates
- `EdgeSymmetry` the permutation symmetry of each hyperedge
- `EdgeListTagged`, `EdgeMultiplicity`, `HyperedgeList` edge accessors
- `HypergraphIncidence` the hyperedges incident to each vertex

### Canonicalization

- `CanonicalHypergraph` canonical form of a hypergraph under vertex relabeling and edge symmetry
- `CanonicalHypergraphRule` canonical form of a hypergraph rewriting rule
- `IsomorphicHypergraphQ` test whether two hypergraphs are isomorphic

### Transformations

- `HypergraphUnion` union of hypergraphs, also available as `Plus`
- `HypergraphHadamardProduct` shared hyperedges with multiplied multiplicities, also available as `NonCommutativeMultiply`
- `SimpleHypergraph` merge duplicate hyperedges and drop self-loops
- `ToOrderedHypergraph` expand every hyperedge over its symmetry group
- `HypergraphArityReduce` reduce hyperedges to fixed-arity subsets
- `HypergraphToGraph`, `OrderedHypergraphToGraph` convert a hypergraph to a graph

### Pattern rules

- `ToPatternRules` convert a hypergraph rule to a SetReplace-style pattern rule
- `ToLabeledEdges`, `ToLabeledPatternEdges` labeled edge encodings used for rule matching

### Enumeration and random generation

- `EnumerateHypergraphs` all canonical hypergraphs with a given edge signature
- `EnumerateOrderedHypergraphs` all canonical hypergraphs with ordered hyperedges
- `EnumerateHypergraphRules` all canonical hypergraph rewriting rules with a given signature
- `EnumerateWolframModelRules` the raw edge-list rules behind the rule enumeration
- `RandomHypergraph` a pseudorandom simple connected hypergraph with a given signature
- `RandomAllHypergraph`, `RandomConnectedHypergraph` direct pseudorandom constructions
- `RandomHypergraphRule` a pseudorandom hypergraph rewriting rule

### Matrices and tensors

- `AdjacencyTensor` the adjacency tensor of a hypergraph
- `AdjacencyHypergraph` the hypergraph with a given adjacency tensor
- `HypergraphIncidenceMatrix` the vertex-edge incidence matrix of a hypergraph
- `IncidenceHypergraph` the hypergraph with a given incidence matrix
- `HypergraphTransitionMatrix` the random-walk transition matrix of a hypergraph
- `Hypermatrix` the hypermatrix representation of a hypergraph
- `HypermatrixGraph` the hypergraph with a given hypermatrix

### Algebraic operations

- `HypergraphInsertionBracket` the insertion bracket of hypergraphs
- `HypergraphInsertionBracketDegree` the degree of a hypergraph in the insertion bracket algebra
- `HypergraphInsertion` all insertions of one hypergraph into another
- `KoszulSign` the Koszul sign of a graded permutation

## Tech Notes

- [Hypergraph Rewriting](paclet:WolframInstitute/Hypergraph/tutorial/HypergraphRewriting)
