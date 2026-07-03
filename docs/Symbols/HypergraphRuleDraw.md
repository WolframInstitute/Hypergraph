---
Template: Symbol
Name: HypergraphRuleDraw
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphRuleDraw
Keywords: [hypergraph rule, interactive drawing, canvas, editor]
SeeAlso: [HypergraphDraw, HypergraphRule, Hypergraph]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphRuleDraw]()[]</code> opens two side-by-side interactive canvases for drawing the input and output of a [HypergraphRule]().

<code>[HypergraphRuleDraw]()[*hg1*, *hg2*]</code> opens the canvases starting from initial hypergraphs *hg1* (input) and *hg2* (output).

<code>[HypergraphRuleDraw]()[*hr*]</code> opens the canvases starting from the input and output of an existing [HypergraphRule]() *hr*.

## Details & Options

- [HypergraphRuleDraw]() produces a dynamic widget; it requires a notebook front end and cannot be used in a headless kernel.
- Each side is a [HypergraphDraw]() canvas with its `"Return"` button hidden, so all of that widget's interactions apply: click an empty spot to create a vertex, click or drag a vertex or hyperedge to select or move it, hold Option (Alt) while clicking to extend the selection, press space or `"e"` to connect the selected vertices into a hyperedge, and right-click for the rename, recolor, and symmetry palette.
- Between the two canvases, three buttons act on the drawing: the right-arrow button copies the whole input canvas onto the output canvas; the ellipsis-arrow button appends the vertices and hyperedges currently selected on the input canvas onto the output canvas, without disturbing what is already there; and `"Print"` prints the current [HypergraphRule]()`[`*input*`,` *output*`]` as a new input cell.
- The `"Return"` button below the two canvases evaluates the widget to the drawn [HypergraphRule]()`[`*input*`,` *output*`]`, the same convention [HypergraphDraw]() uses for a single hypergraph.
- [HypergraphRuleDraw]() accepts all [Hypergraph]() options. They set up the initial input and output hypergraphs when *hg1* or *hg2* is omitted, but only the input (left) canvas receives them live; the output (right) canvas always opens with default canvas options.

## Basic Examples

Draw a rule's input and output from scratch; press the arrow button to copy the input onto the output, edit the output, then press `"Return"`:

```wl
#| eval: false
HypergraphRuleDraw[]
```

---

Start from an existing rule, editing its input and output side by side:

```wl
#| eval: false
HypergraphRuleDraw[HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{1, 2}, {2, 3}}]]]
```

## Scope

Seed the two canvases with different starting hypergraphs:

```wl
#| eval: false
HypergraphRuleDraw[Hypergraph[{{1, 2, 3}}], Hypergraph[{{1, 2}, {2, 3}, {3, 1}}]]
```

## Options

### ImageSize

Options passed to [HypergraphRuleDraw]() size the input canvas; the output canvas keeps its default size:

```wl
#| eval: false
HypergraphRuleDraw[Hypergraph[{{1, 2, 3}}], ImageSize -> 512]
```
