package mermoid.docs

import mermoid.ascent.MermoidAscent
import mermoid.css.{Theme, ThemeColors, ThemeName}
import mermoid.{RenderConfig, Viewport}
import specular.*

/** Every interactive / hybrid feature, exercisable via `docsPreview`.
  *
  * Shared `DocSpec` (not `DocSpecSuite`) so docsJS can remount `.interactive` examples. JVM discovery is
  * [[InteractiveSuite]].
  */
object Interactive extends DocSpec:

  /** Charcoal / cream / terracotta aligned with the Early Effect docs theme. */
  private val chalkboard: RenderConfig =
    val colors = ThemeColors(
      primaryColor = "#2a2b2e",
      primaryBorderColor = "#c46a52",
      primaryTextColor = "#e8e6dc",
      secondaryColor = "#3f4145",
      secondaryBorderColor = "#9a978c",
      secondaryTextColor = "#e8e6dc",
      tertiaryColor = "#121314",
      tertiaryBorderColor = "#3f4145",
      tertiaryTextColor = "#e8e6dc",
      lineColor = "#d4a574",
      textColor = "#e8e6dc",
      mainBkg = "#2a2b2e",
      nodeBorder = "#c46a52",
      background = "#1c1d1f",
      fontFamily = """"Avenir Next", Avenir, "Segoe UI", "Helvetica Neue", Helvetica, Arial, sans-serif""",
      fontSize = "14px",
      edgeLabelBackground = "#1c1d1f",
      noteBackground = "#3f4145",
      noteBorderColor = "#9a978c",
      noteTextColor = "#e8e6dc",
    )
    RenderConfig(theme = ThemeName.Dark, customStylesheet = Some(Theme.toStylesheet(colors)))
  end chalkboard

  private val hybridFlow =
    """flowchart LR
      |  A[Start] --> B{Decide}
      |  B -->|yes| C[Ship]
      |  B -->|no| D[Fix]
      |  D --> B
      |""".stripMargin

  private val stateNotes =
    """stateDiagram-v2
      |  [*] --> Idle
      |  Idle --> Active: start
      |  Active --> Idle: pause
      |  Active --> Done: finish
      |  Done --> [*]
      |  note right of Idle
      |    Waiting for input
      |  end note
      |""".stripMargin

  private val tooltips =
    """flowchart LR
      |  A[Parse] --> B[Layout]
      |  B --> C[Paint]
      |  click A callback "Mermaid → AST"
      |  click B callback "DiagramScene + routes"
      |  click C href "https://www.earlyeffect.rocks" "Open Early Effect" _blank
      |""".stripMargin

  private val reflowWide =
    """flowchart LR
      |  A[One] --> B[Two]
      |  B --> C[Three]
      |  C --> D[Four]
      |  D --> E[Five]
      |  E --> F[Six]
      |  A -.-> F
      |""".stripMargin

  private val denseFit =
    """flowchart LR
      |  A --> B
      |  A --> C
      |  A --> D
      |  A --> E
      |  A --> F
      |  A --> G
      |  B --> H
      |  C --> H
      |  D --> H
      |  E --> H
      |  F --> H
      |  G --> H
      |""".stripMargin

  def doc = page("Interactive")(
    md"""
Hybrid HTML + SVG diagrams from **`mermoid-ascent`**: real HTML nodes (click, hover, tooltips), SVG edges/splines
that **re-layout** when the viewport changes. Run `sbt docsPreview` and work through every section below.

```scala
libraryDependencies += "rocks.earlyeffect" %% "mermoid-ascent" % "<version>"   // JVM
libraryDependencies += "rocks.earlyeffect" %%% "mermoid-ascent" % "<version>"  // Scala.js

import mermoid.ascent.MermoidAscent
import mermoid.{RenderConfig, Viewport}

MermoidAscent.diagram(source, viewport = Some(Viewport(640)))
MermoidAscent.diagramInteractive(source, initialWidth = 720)
```

Existing guide pages still embed inert SVG via `MermoidAscent.svgDiagram` so structure docs stay byte-stable.
""",
    section("Hybrid flowchart")(
      md"""
HTML nodes + SVG edges (not an inert `SvgNode → UI` map). Inspect the DOM: nodes are `<button class="mermoid-node">`.
""",
      example {
        MermoidAscent.diagram(hybridFlow, chalkboard, Some(Viewport(640)))
      },
    ),
    section("Hybrid state + notes")(
      md"""
States and note cards are HTML; connectors stay in the SVG layer. Click a state or its note.
""",
      exampleIO {
        MermoidAscent.diagramInteractive(stateNotes, chalkboard, initialWidth = 560)
      }.interactive,
    ),
    section("Hover + selection")(
      md"""
Click a node: it gets `is-selected` and incident edges get `is-incident`. Click again to clear.
""",
      exampleIO {
        MermoidAscent.diagramInteractive(hybridFlow, chalkboard, initialWidth = 640)
      }.interactive,
    ),
    section("Tooltips")(
      md"""
Mermaid `click … "tooltip"` becomes an ascent hover card (and SVG `<title>` on the SVG painter). Hover a node.
""",
      exampleIO {
        MermoidAscent.diagramInteractive(tooltips, chalkboard, initialWidth = 640)
      }.interactive,
    ),
    section("Links from click")(
      md"""
`click C href "…" "…" _blank` wraps the node as a link. The **Paint** node opens earlyeffect.rocks in a new tab.
""",
      example {
        MermoidAscent.diagram(tooltips, chalkboard, Some(Viewport(640)))
      },
    ),
    section("Reactive reflow")(
      md"""
Use **Narrow / Medium / Wide** (or shrink the browser). Below 640px diagrams prefer TB; at/above they prefer LR.
Spacing compresses to the viewport. Edges are **recomputed**, not stretched.

See [Responsive layout](responsive-layout.html) for the mechanics: direction flips, spacing compression, and scale-to-fit.
""",
      exampleIO {
        MermoidAscent.diagramInteractive(reflowWide, chalkboard, initialWidth = 720)
      }.interactive,
    ),
    section("Scale-to-fit safety")(
      md"""
A dense hub still overflows after minimum spacing; uniform `transform: scale` keeps HTML and SVG aligned.
""",
      example {
        MermoidAscent.diagram(denseFit, chalkboard, Some(Viewport(320)))
      },
    ),
    section("Selection across reflow")(
      md"""
Select a node, then hit Narrow/Wide: the selected id is preserved while geometry updates.
""",
      exampleIO {
        MermoidAscent.diagramInteractive(reflowWide, chalkboard, initialWidth = 720)
      }.interactive,
    ),
    section("Theme / chalkboard")(
      md"""
Interactive diagram under docs chalkboard colors (Dark base + Early Effect palette).
""",
      exampleIO {
        MermoidAscent.diagramInteractive(hybridFlow, chalkboard, initialWidth = 600)
      }.interactive,
    ),
    section("Side-by-side contrast")(
      md"""
Same source: inert SVG embed vs hybrid interactive.
""",
      md"""
**SVG** (`svgDiagram`):
""",
      example {
        MermoidAscent.svgDiagram(hybridFlow, chalkboard)
      },
      md"""
**Hybrid** (`diagramInteractive`):
""",
      exampleIO {
        MermoidAscent.diagramInteractive(hybridFlow, chalkboard, initialWidth = 560)
      }.interactive,
    ),
  )
end Interactive
