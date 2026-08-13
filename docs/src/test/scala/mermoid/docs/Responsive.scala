package mermoid.docs

import mermoid.ascent.MermoidAscent
import _root_.mermoid.{DiagramLayout, DiagramScene, MermaidParser, RenderConfig, ResponsiveConfig, Viewport}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** How a viewport drives direction flips, spacing compression, and scale-to-fit. */
object Responsive extends DocSpecSuite:

  private val chain =
    """flowchart LR
      |  A[One] --> B[Two]
      |  B --> C[Three]
      |  C --> D[Four]
      |  D --> E[Five]
      |""".stripMargin

  private def sceneOf(src: String, viewport: Option[Viewport]): DiagramScene =
    MermaidParser
      .parse(src)
      .map(d => DiagramLayout.scene(d, RenderConfig(), viewport))
      .getOrElse(throw new AssertionError(s"unparseable: $src"))

  def doc = page("Responsive layout")(
    md"""
Passing a `Viewport(maxWidth)` to the renderer tells it how much horizontal space is available. Three things may happen,
all controlled by `ResponsiveConfig`:

| Mechanism | What it does | Controlled by |
|---|---|---|
| **Direction flip** | Narrow viewports prefer vertical flow; wide ones prefer horizontal | `flipDirectionBelow` |
| **Spacing compression** | Scales spacing and padding toward the viewport target | `compressSpacing`, `minSpacingScale`, `maxSpacingScale` |
| **Scale-to-fit** | Uniform `transform: scale` when the scene still overflows width | `scaleToFit` |

All three are enabled by default. Disabling them is how you lock a diagram at its natural size.
""",
    section("Direction flip")(
      md"""
The author writes `flowchart LR`. Below `flipDirectionBelow` (default 640), the layout reorients to vertical so content
gets height instead of fighting for width. At or above the threshold, horizontal is preferred.

| Author direction | Below threshold | At / above threshold |
|---|---|---|
| `LR` | stays `LR` (already horizontal) — actually flips to `TB` | stays `LR` |
| `RL` | flips to `BT` | stays `RL` |
| `TB` / `TD` | stays `TB` | flips to `LR` |
| `BT` | stays `BT` | flips to `RL` |

The flip is a layout decision, not a CSS transform. The SVG dimensions and edge routes are recomputed for the new
direction.
""",
      example {
        MermoidAscent.svgDiagram(chain, RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = None)))
      },
      md"""
That is the unconstrained layout: five nodes in a horizontal chain. Now with viewport-driven direction flip at 640px:
""",
      exampleValue {
        val wide   = sceneOf(chain, Some(Viewport(900)))
        val narrow = sceneOf(chain, Some(Viewport(400)))
        List(
          s"Wide (${wide.width.toInt}×${wide.height.toInt}) direction: ${wide.direction}",
          s"Narrow (${narrow.width.toInt}×${narrow.height.toInt}) direction: ${narrow.direction}",
          s"Flipped: ${wide.direction != narrow.direction}",
        ).mkString("\n")
      }.assert(s =>
        assertTrue(
          s.contains("Wide"),
          s.contains("Narrow"),
          s.contains("Flipped: true"),
        )
      ),
    ),
    section("Spacing compression")(
      md"""
When `compressSpacing` is `true` (default), the layout estimates how many nodes sit along the main axis and scales
spacing, padding, and parallel edge offset toward the viewport target. The scale is clamped between
`minSpacingScale` (default 0.45) and `maxSpacingScale` (default 1.75).

A diagram that needs 960px of width in a 320px viewport compresses aggressively but never below 45% of default spacing.
""",
      exampleValue {
        val big   = sceneOf(chain, Some(Viewport(1200)))
        val small = sceneOf(chain, Some(Viewport(320)))
        List(
          s"Wide width: ${big.width.toInt}",
          s"Narrow width: ${small.width.toInt}",
          s"Compression ratio: ${(small.width / big.width).toString.take(4)}",
        ).mkString("\n")
      }.assert(s =>
        assertTrue(
          s.contains("Wide"),
          s.contains("Narrow"),
          s.contains("Compression ratio:"),
          // Narrow should be materially smaller than wide.
          s.indexOf("Narrow width:") > -1,
        )
      ),
      md"""
Disable compression to keep the author's geometry intact regardless of viewport:
""",
      exampleValue {
        val compressed = sceneOf(chain, Some(Viewport(320))) // default compressSpacing = true
        val noCompress = MermaidParser
          .parse(chain)
          .map { d =>
            DiagramLayout.scene(
              d,
              RenderConfig(responsive = ResponsiveConfig(compressSpacing = false)),
              Some(Viewport(320)),
            )
          }
          .getOrElse(throw new AssertionError("unparseable"))
        List(
          s"With compression:  ${compressed.width.toInt}px wide",
          s"Without compression: ${noCompress.width.toInt}px wide",
          s"Rigid is wider: ${noCompress.width > compressed.width}",
        ).mkString("\n")
      }.assert(s =>
        assertTrue(
          s.contains("Rigid is wider: true")
        )
      ),
    ),
    section("Scale-to-fit")(
      md"""
After layout, if `scene.width > viewport.maxWidth` and `scaleToFit` is enabled (default), the painters may apply a
uniform `transform: scale(scene.fitScale(maxWidth))`. This keeps HTML nodes and SVG edges aligned in hybrid mode —
both scale together instead of one overflowing.

Spacing compression handles moderate overflows, but a dense hub still exceeds its budget even at minimum spacing. That
is when uniform scaling kicks in:
""",
      exampleValue {
        val hub =
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
        val scene = sceneOf(hub, Some(Viewport(320)))
        List(
          s"Scene width: ${scene.width.toInt}",
          s"Viewport: 320",
          s"fitScale(320): ${scene.fitScale(320)}",
          s"Needs scaling: ${scene.fitScale(320) < 1.0}",
        ).mkString("\n")
      }.assert(s =>
        assertTrue(
          s.contains("Needs scaling: true"),
          s.contains("fitScale(320):"),
        )
      ),
    ),
    section("Disabling responsive entirely")(
      md"""
Three knobs to turn off. Omitting the `Viewport` altogether is the simplest approach — layout runs unconstrained.
""",
      exampleValue {
        val constrained   = sceneOf(chain, Some(Viewport(320)))
        val unconstrained = sceneOf(chain, None)
        List(
          s"Constrained: ${constrained.width.toInt}×${constrained.height.toInt}",
          s"Unconstrained: ${unconstrained.width.toInt}×${unconstrained.height.toInt}",
          s"Unconstrained is wider: ${unconstrained.width > constrained.width}",
        ).mkString("\n")
      }.assert(s =>
        assertTrue(
          s.contains("Constrained:"),
          s.contains("Unconstrained:"),
          s.contains("Unconstrained is wider: true"),
        )
      ),
    ),
    section("In hybrid mode")(
      md"""
`mermoid-ascent` recomputes the entire `DiagramScene` on every width change — geometry, edge routes, and direction.
Selection state is preserved by node id, so clicking a node before reflow keeps it selected after.

See [Interactive](interactive.html) for live Narrow / Medium / Wide controls. The built-in buttons set 360px, 640px,
and 900px; the threshold between vertical and horizontal flow sits at 640px by default.
""",
      example {
        MermoidAscent.diagram(chain, RenderConfig(), Some(Viewport(640)))
      },
      md"""
For external width sources (e.g., a `ResizeObserver` on the container), use `MermoidAscent.diagramResponsive` with a
`Source[Double]`. The built-in Narrow/Medium/Wide buttons are hidden when `showWidthControls = false`.
""",
    ),
  )
end Responsive
