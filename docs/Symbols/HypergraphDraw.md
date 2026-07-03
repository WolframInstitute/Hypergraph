---
Template: Symbol
Name: HypergraphDraw
Context: WolframInstitute`Hypergraph`
Paclet: WolframInstitute/Hypergraph
URI: WolframInstitute/Hypergraph/ref/HypergraphDraw
Keywords: [hypergraph, interactive drawing, canvas, editor]
SeeAlso: [Hypergraph, HypergraphRule, SimpleHypergraphPlot]
RelatedGuides: [HypergraphFunctionality]
---

## Usage

<code>[HypergraphDraw]()[]</code> opens an interactive canvas for drawing a hypergraph.

<code>[HypergraphDraw]()[*hg*]</code> opens the canvas starting from an initial hypergraph *hg*.

<code>[HypergraphDraw]()[Dynamic[*hg*]]</code> opens a canvas that dynamically writes the drawn hypergraph to the symbol *hg*.

## Details & Options

- [HypergraphDraw]() produces a dynamic widget; it requires a notebook front end and cannot be used in a headless kernel.
- The widget is an [Interpretation](): evaluating it, or pressing its `"Return"` button, yields the drawn [Hypergraph]() object; the `"Print"` button prints the current hypergraph as a new input cell.
- In the form <code>[HypergraphDraw]()[*hg*]</code>, *hg* can also be anything the [Hypergraph]() constructor accepts, such as a list of hyperedges.
- Clicking an empty spot on the canvas creates a new vertex and selects it; clicking a vertex or a hyperedge toggles its selection, and dragging moves it. Holding Option (Alt) while clicking adds vertices or selections without resetting the current selection, holding Shift while dragging pans the canvas, and Shift-clicking a selected element repaints it with the current color.
- Pressing the space bar (or `"e"`) creates a new hyperedge connecting the currently selected vertices; with no vertices selected it creates an empty hyperedge, drawn as a circle.
- Right-clicking opens a palette for renaming, relabeling and recoloring the clicked vertex or hyperedge, setting a hyperedge's symmetry (`"Unordered"`, `"Cyclic"` or `"Ordered"`), and copying or pasting hypergraphs through the clipboard.
- The opener at the bottom of the widget reveals additional controls: input fields for vertex names and labels and hyperedge labels, a symmetry selector for new hyperedges, and a color slider.
- Keyboard shortcuts act on the canvas under the mouse pointer:

| key | action |
|---|---|
| space or <code>"e"</code> | create a hyperedge from the selected vertices |
| backspace | delete the selection |
| <code>"z"</code> / <code>"d"</code> | undo / redo the last action |
| Escape or <code>"q"</code> | reset the selection |
| <code>"r"</code> | reset the canvas to the initial hypergraph |
| <code>"u"</code> | update the returned hypergraph |

- [HypergraphDraw]() accepts all [Hypergraph]() options, which are applied to the resulting hypergraph, together with the following options:

| option | default | effect |
|---|---|---|
| <code>"InitialColor"</code> | <code>Automatic</code> | initial color for new vertices and hyperedges |
| <code>"VertexLabels"</code> | <code>{"A", "B", "C"}</code> | choices offered in the vertex label popup menu |
| <code>"EdgeLabels"</code> | <code>{"f", "g", "h"}</code> | choices offered in the hyperedge label popup menu |
| <code>"InterfaceColor"</code> | <code>LightDarkSwitched[Black, White]</code> | color of the interface elements drawn on the canvas |
| <code>"HideReturn"</code> | <code>False</code> | whether to hide the <code>"Return"</code> button |

- The related function `HypergraphRuleDraw` opens two such canvases side by side for drawing the input and output of a [HypergraphRule]().

## Basic Examples

Draw a hypergraph starting from a single ternary hyperedge; press the `"Return"` button to replace the widget with the resulting [Hypergraph]() object:

```wl
#| eval: false
HypergraphDraw[Hypergraph[{{1, 2, 3}}], ImageSize -> 512]
```

---

Start from an empty canvas; click to create vertices, then press the space bar to connect the selected vertices into a hyperedge:

```wl
#| eval: false
HypergraphDraw[]
```

## Scope

Edit an existing hypergraph, specifying it by its list of hyperedges:

```wl
#| eval: false
HypergraphDraw[{{1, 2, 3}, {2, 3}, {3, 1, 4, 5}, {1}}, VertexLabels -> Automatic, ImageSize -> 512]
```

---

Bind the canvas to a symbol, so that the drawn hypergraph is continuously written to `hg`:

```wl
#| eval: false
HypergraphDraw[Dynamic[hg]]
```

## Options

### InterfaceColor

Draw on a dark background with white interface elements:

```wl
#| eval: false
HypergraphDraw[{{1, 2, 3}, {2, 3}, {3, 1, 4, 5}, {1}},
    Background -> Black, EdgeStyle -> White, VertexStyle -> White,
    VertexLabels -> Automatic, VertexLabelStyle -> White,
    ImageSize -> 512, "InterfaceColor" -> White
]
```
