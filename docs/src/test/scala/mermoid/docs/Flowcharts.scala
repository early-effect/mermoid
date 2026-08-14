package mermoid.docs

import mermoid.ascent.MermoidAscent
import specular.*
import specular.ziotest.DocSpecSuite
import _root_.mermoid.{EdgeStyle, NodeShape}

/** Every flowchart construct mermoid implements, rendered live. */
object Flowcharts extends DocSpecSuite:

  def doc = page("Flowcharts")(
    md"""
`flowchart <direction>` (or the older `graph`) opens a flowchart. Every diagram on this page is parsed and rendered by
mermoid as the page is built — if a shape or edge style stopped working, this page would fail to build.
""",
    section("Direction")(
      md"""
Five directions: `TB` (top-to-bottom), `TD` (a synonym for `TB`), `BT`, `LR`, `RL`. The direction decides both the flow
axis and where self-loops attach.
""",
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    A[Read] --> B[Transform]
                            |    B --> C[Write]
                            |""".stripMargin)
      },
      example {
        MermoidAscent.svgDiagram("""flowchart TD
                            |    A[Read] --> B[Transform]
                            |    B --> C[Write]
                            |""".stripMargin)
      },
      md"""
Chained `A --> B --> C` is one hop per pair, same as writing each edge on its own line. `%%` comments are ignored.
""",
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    %% pipeline sketch
                            |    A[Read] --> B[Transform] --> C[Write]
                            |""".stripMargin)
      },
    ),
    section("Node shapes")(
      md"""
${NodeShape.values.size} shapes (`NodeShape`). A bare id with no bracket syntax is a `Rect` labelled with the id itself.

${NodeShape.markdownTable}
""",
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    R[Rect]
                            |    O(Round)
                            |    S([Stadium])
                            |    U[[Subroutine]]
                            |    Y[(Cylinder)]
                            |""".stripMargin)
      },
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    C((Circle))
                            |    D(((Double)))
                            |    H{Rhombus}
                            |    X{{Hexagon}}
                            |""".stripMargin)
      },
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    P[/Parallelogram/]
                            |    Q[\ParallelogramAlt\]
                            |    T[/Trapezoid\]
                            |    V[\TrapezoidAlt/]
                            |""".stripMargin)
      },
      md"""
The shape name lands in the wrapper's class list, so `.node-rhombus .node-shape { fill: gold }` restyles every decision
node without touching the diagram source. See [SVG structure](svg-structure.html).
""",
    ),
    section("Edge styles")(
      md"""
${EdgeStyle.values.size} edge styles (`EdgeStyle`). Each contributes a class to the edge group, and the dashing lives in CSS rather than in the geometry.

${EdgeStyle.markdownTable}
""",
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    A1[Arrow] --> A2[ ]
                            |    B1[Open] --- B2[ ]
                            |    C1[Dotted] -.-> C2[ ]
                            |    D1[DottedOpen] -.- D2[ ]
                            |    E1[Thick] ==> E2[ ]
                            |""".stripMargin)
      },
    ),
    section("Edge labels")(
      md"""
Two spellings, both supported: `-->|label|` and `-- label -->`. Labels get a background rect so they stay readable where
they cross an edge, and the layout widens the gap between layers to fit them.
""",
      example {
        MermoidAscent.svgDiagram("""flowchart TD
                            |    Check{Valid?} -->|yes| Save[(Database)]
                            |    Check -- no --> Reject[/Error response/]
                            |""".stripMargin)
      },
    ),
    section("Self-loops")(
      md"""
An edge from a node to itself renders as a loop, with the `self-loop` class added to the edge group. Multiple loops on
one node stack their labels rather than overlapping, and the layout reserves room for them — to the right in a vertical
flowchart, above in a horizontal one.
""",
      example {
        MermoidAscent.svgDiagram("""flowchart TD
                            |    Poll[Poll queue] -->|empty| Poll
                            |    Poll -->|error| Poll
                            |    Poll -->|message| Handle[Handle]
                            |""".stripMargin)
      },
    ),
    section("Cycles")(
      md"""
Layering is longest-path, and it breaks cycles rather than diverging on them: a node already being resolved contributes
nothing to its own depth. Within each layer, a barycenter sweep reorders nodes to cut crossings, and long edges bend
through invisible waypoints so they do not slice intermediate nodes. A graph with a back edge lays out fine.
""",
      example {
        MermoidAscent.svgDiagram("""flowchart TD
                            |    A[Attempt] --> B{Succeeded?}
                            |    B -->|no| C[Back off]
                            |    C --> A
                            |    B -->|yes| D([Done])
                            |""".stripMargin)
      },
    ),
    section("Subgraphs")(
      md"""
`subgraph <id> [label] … end` draws a dashed frame around its nodes, with an optional `direction` line inside. The frame
is a `<g class="subgraph" id="subgraph-{id}">` rendered behind the edges and nodes.
""",
      example {
        MermoidAscent.svgDiagram("""flowchart TD
                            |    subgraph ingest [Ingest]
                            |        direction LR
                            |        Fetch[Fetch] --> Parse[Parse]
                            |    end
                            |    Parse --> Store[(Store)]
                            |""".stripMargin)
      },
    ),
    section("Styling from the diagram source")(
      md"""
Three statements (and one suffix), all of which end up as CSS rather than as baked-in attributes:

- `classDef name prop:value,…` becomes a CSS rule in the embedded stylesheet
- `class A,B name` adds `name` to those nodes' class lists
- `A:::name` (and `A[Label]:::name`) is the same assignment written on the node
- `style A prop:value,…` becomes an inline `style` attribute on that one node

`classDef` scales — one rule, however many nodes carry the class — so prefer it over `style`. The same statements work
on [state diagrams](state-diagrams.html).
""",
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    classDef hot fill:#ffdddd,stroke:#cc0000
                            |    A[Cold] --> B[Hot]:::hot
                            |    B --> C[Hot too]
                            |    class C hot
                            |    style A fill:#ddeeff
                            |""".stripMargin)
      },
      md"""
Note where each landed: `classDef` in the `<style>` block, `class` in the class attribute, `style` inline. See
[Custom CSS](custom-css.html) for supplying a whole stylesheet from Scala instead.
""",
    ),
    section("Edge aliases")(
      md"""
mermoid adds one thing Mermaid does not have: `as <name>` on an edge, which fixes that edge's element id.

Without an alias an edge is `edge-{from}-{to}-{index}`, so inserting an earlier parallel edge renumbers it, and any CSS
or test that selected `#edge-A-B-1` silently moves. An alias pins it:
""",
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    A[Start] --> B[Finish] as happy
                            |    A --> B
                            |""".stripMargin)
      },
      md"""
The first edge is `#edge-happy` no matter how many siblings appear later; the unaliased one keeps its positional id
(`#edge-A-B-1` here). Notes in [state diagrams](state-diagrams.html) take `as` the same way.
""",
    ),
    section("Clicks")(
      md"""
Mermaid `click` lines attach interactions to a node. mermoid stores them on the scene; painters decide how to surface
them. The SVG painter emits `<title>` for tooltips and wraps `href` targets in an `<a>`. `mermoid-ascent` turns the same
bindings into hover cards and links (callback **names** are stored; JavaScript is never executed).

Supported forms:

```
click A callback "tooltip text"
click A call myHandler() "tooltip"
click A href "https://example.com" "Open docs" _blank
click A "https://example.com"
```

Link targets: `_blank`, `_self`, `_parent`, `_top`.
""",
      example {
        MermoidAscent.svgDiagram("""flowchart LR
                            |    A[Parse] --> B[Layout]
                            |    B --> C[Paint]
                            |    click A callback "Mermaid → AST"
                            |    click B callback "DiagramScene + routes"
                            |    click C href "https://www.earlyeffect.rocks" "Open Early Effect" _blank
                            |""".stripMargin)
      },
      md"""
Try the same source under hybrid selection and hover on [Interactive](interactive.html).
""",
    ),
    section("Special cases")(
      md"""
| Case | Behaviour |
|---|---|
| Chained edges `A --> B --> C` | One hop per pair, same as writing each edge on its own line. |
| `%%` comments / `%%{init:…}%%` | Comments are ignored. Init directives are skipped; they do not pick a theme. |
| Parallel edges (same endpoints twice) | Both render, offset so they do not overlap. Alias with `as` if you CSS-select one. |
| Cycles / back-edges | Layering breaks cycles; barycenter cuts crossings; long edges use waypoints. |
| `linkStyle` | Not implemented. |
| Nested subgraphs | Supported; frames paint behind edges and nodes. |
| Semicolon separators | OK as statement separators (in addition to newlines). |
| `end` vs `endpoint` | Bare `end` closes a subgraph; ids like `endpoint` are fine. |
""",
      example {
        MermoidAscent.svgDiagram("""flowchart TD
                            |    A[Source] --> B[Sink]
                            |    A --> B
                            |    A -.-> B as dotted
                            |""".stripMargin)
      },
    ),
  )
end Flowcharts
