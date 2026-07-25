package mermoid.docs

import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

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
        MermoidUi.diagram("""flowchart LR
                            |    A[Read] --> B[Transform]
                            |    B --> C[Write]
                            |""".stripMargin)
      },
      example {
        MermoidUi.diagram("""flowchart TD
                            |    A[Read] --> B[Transform]
                            |    B --> C[Write]
                            |""".stripMargin)
      },
      md"""
One edge per statement — mermoid does not implement Mermaid's chained `A --> B --> C` shorthand yet. Write the second
edge on its own line, referring to `B` by id.
""",
    ),
    section("Node shapes")(
      md"""
Thirteen shapes. A bare id with no bracket syntax is a `Rect` labelled with the id itself.

| Syntax | `NodeShape` | CSS class |
|---|---|---|
| `A[text]` | `Rect` | `node-rect` |
| `A(text)` | `Round` | `node-round` |
| `A([text])` | `Stadium` | `node-stadium` |
| `A[[text]]` | `Subroutine` | `node-subroutine` |
| `A[(text)]` | `Cylinder` | `node-cylinder` |
| `A((text))` | `Circle` | `node-circle` |
| `A(((text)))` | `DoubleCircle` | `node-double-circle` |
| `A{text}` | `Rhombus` | `node-rhombus` |
| `A{{text}}` | `Hexagon` | `node-hexagon` |
| `A[/text/]` | `Parallelogram` | `node-parallelogram` |
| `A[\\text\\]` | `ParallelogramAlt` | `node-parallelogram-alt` |
| `A[/text\\]` | `Trapezoid` | `node-trapezoid` |
| `A[\\text/]` | `TrapezoidAlt` | `node-trapezoid-alt` |
""",
      example {
        MermoidUi.diagram("""flowchart LR
                            |    R[Rect]
                            |    O(Round)
                            |    S([Stadium])
                            |    U[[Subroutine]]
                            |    Y[(Cylinder)]
                            |""".stripMargin)
      },
      example {
        MermoidUi.diagram("""flowchart LR
                            |    C((Circle))
                            |    D(((Double)))
                            |    H{Rhombus}
                            |    X{{Hexagon}}
                            |""".stripMargin)
      },
      example {
        MermoidUi.diagram("""flowchart LR
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
      exampleValue {
        import _root_.mermoid.*
        MermaidParser.parse("flowchart TD\n  A{Decide} --> B([Done])").map(SvgRenderer.render(_)) match
          case Right(svg) =>
            List("node-rhombus", "node-stadium").filter(svg.contains).mkString(", ")
          case Left(err) => s"parse error: $err"
      }.assert(s => assertTrue(s == "node-rhombus, node-stadium")),
    ),
    section("Edge styles")(
      md"""
Five edge styles. Each contributes a class to the edge group, and the dashing lives in CSS rather than in the geometry.

| Syntax | `EdgeStyle` | CSS class | Arrowhead |
|---|---|---|---|
| `A --> B` | `Arrow` | `edge-arrow` | yes |
| `A --- B` | `Open` | `edge-open` | no |
| `A -.-> B` | `Dotted` | `edge-dotted` | yes |
| `A -.- B` | `DottedOpen` | `edge-dotted-open` | no |
| `A ==> B` | `Thick` | `edge-thick` | yes |
""",
      example {
        MermoidUi.diagram("""flowchart LR
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
        MermoidUi.diagram("""flowchart TD
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
        MermoidUi.diagram("""flowchart TD
                            |    Poll[Poll queue] -->|empty| Poll
                            |    Poll -->|error| Poll
                            |    Poll -->|message| Handle[Handle]
                            |""".stripMargin)
      },
    ),
    section("Cycles")(
      md"""
Layering is longest-path, and it breaks cycles rather than diverging on them: a node already being resolved contributes
nothing to its own depth. A graph with a back edge lays out fine.
""",
      example {
        MermoidUi.diagram("""flowchart TD
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
        MermoidUi.diagram("""flowchart TD
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
Three statements, all of which end up as CSS rather than as baked-in attributes:

- `classDef name prop:value,…` becomes a CSS rule in the embedded stylesheet
- `class A,B name` adds `name` to those nodes' class lists
- `style A prop:value,…` becomes an inline `style` attribute on that one node

`classDef` scales — one rule, however many nodes carry the class — so prefer it over `style`.
""",
      example {
        MermoidUi.diagram("""flowchart LR
                            |    classDef hot fill:#ffdddd,stroke:#cc0000
                            |    A[Cold] --> B[Hot]
                            |    B --> C[Hot too]
                            |    class B,C hot
                            |    style A fill:#ddeeff
                            |""".stripMargin)
      },
      exampleValue {
        import _root_.mermoid.*
        MermaidParser
          .parse("flowchart LR\n  classDef hot fill:#ffdddd\n  A[a] --> B[b]\n  class B hot\n  style A fill:#eee")
          .map(SvgRenderer.render(_))
          .map(svg => (svg.contains(".hot {"), svg.contains("""class="node node-rect hot""""), svg.contains("style=")))
      }.assert(r => assertTrue(r == Right((true, true, true)))),
      md"""
Note where each landed: `classDef` in the `<style>` block, `class` in the class attribute, `style` inline. See
[Custom CSS](custom-css.html) for supplying a whole stylesheet from Scala instead.
""",
    ),
    section("Edge aliases")(
      md"""
mermoid adds one thing Mermaid does not have: `as <name>` on an edge, which fixes that edge's element id.

Without an alias an edge is `edge-{from}-{to}-{index}`, so inserting an earlier parallel edge renumbers it — and any CSS
or test that selected `#edge-A-B-1` silently moves. An alias pins it:
""",
      exampleValue {
        import _root_.mermoid.*
        MermaidParser
          .parse("flowchart LR\n  A[a] --> B[b] as happy\n  A --> B\n")
          .map(SvgRenderer.render(_))
          .map(svg => (svg.contains("""id="edge-happy""""), svg.contains("""id="edge-A-B-1"""")))
      }.assert(r => assertTrue(r == Right((true, true)))),
      md"""
The first edge is `#edge-happy` no matter how many siblings appear later; the unaliased one keeps its positional id.
Notes in [state diagrams](state-diagrams.html) take `as` the same way.
""",
    ),
  )
end Flowcharts
