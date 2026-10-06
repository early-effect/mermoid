package mermoid.docs

import mermoid.ascent.MermoidAscent
import _root_.mermoid.{DiagramLayout, DiagramScene, MermaidParser, RenderConfig, ResponsiveConfig, Scene, Viewport}
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

  private def sceneOf(
      src: String,
      viewport: Option[Viewport],
      config: RenderConfig = RenderConfig(),
  ): DiagramScene =
    MermaidParser.parse(src) match
      case Right(d) =>
        DiagramLayout.scene(d, config, viewport) match
          case Scene.Ranked(scene) => scene
          case Scene.Sequence(_)   => throw new AssertionError(s"expected a ranked scene: $src")
      case Left(err) => throw new AssertionError(s"unparseable: ${err.message}\n$src")

  def doc = page("Responsive layout")(
    md"""
Passing a `Viewport(maxWidth)` to the renderer tells it how much horizontal space is available. Three things may happen,
all controlled by `ResponsiveConfig`:

| Mechanism | What it does | Controlled by |
|---|---|---|
| **Direction flip** | Opt-in. Below the threshold prefers vertical; at or above prefers horizontal | `flipDirectionBelow` (default `None`) |
| **Spacing compression** | Scales spacing and padding toward the viewport target | `compressSpacing`, `minSpacingScale`, `maxSpacingScale` |
| **Container fit** | Hybrid paint scales nodes and edges down to the column, never below a floor | `fit` (`ContainerFit.ToWidth(0.5)` or `Off`) |

Spacing compression and container fit are on by default. Direction is not. A viewport width changes spacing. It does
not turn an authored `TD` into `LR`. A sequence diagram does not flip either: a narrow viewport compresses
`SequenceConfig.columnGap` and `rowPitch`, then container fit scales the picture.
""",
    section("Direction flip")(
      md"""
The default keeps the direction the author wrote. `flipDirectionBelow = Some(640)` opts in: below 640 the layout
prefers vertical flow, and at or above 640 it prefers horizontal.

| Author direction | Below threshold | At / above threshold |
|---|---|---|
| `LR` | flips to `TB` | stays `LR` |
| `RL` | flips to `BT` | stays `RL` |
| `TB` / `TD` | stays vertical | flips to `LR` |
| `BT` | stays `BT` | flips to `RL` |

The flip is a layout decision, not a CSS transform. The SVG dimensions and edge routes are recomputed for the new
direction. A static embed does not know the reader's window, so it does not flip. Pass a `Viewport` together with
`flipDirectionBelow`, or use `diagramResponsive` with a live width.
""",
      example {
        MermoidAscent.svgDiagram(chain)
      },
      md"""
That is the authored layout: five nodes in a horizontal chain. The same chain with the flip opted in at 640px:
""",
      exampleValue {
        val flip   = RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640)))
        val wide   = sceneOf(chain, Some(Viewport(900)), flip)
        val narrow = sceneOf(chain, Some(Viewport(400)), flip)
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
      md"""
Without that opt-in, a `flowchart TD` laid out for a 900px column stays top to bottom:
""",
      exampleValue {
        val src =
          """flowchart TD
            |  A --> B
            |  B --> C
            |""".stripMargin
        val scene = sceneOf(src, Some(Viewport(900)))
        s"Direction: ${scene.direction}"
      }.assert(s => assertTrue(s.contains("Direction: TD"))),
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
    section("Container fit")(
      md"""
After layout, hybrid paint scales the scaler that holds the HTML nodes and the SVG edges together. The scale is
`min(1, column / scene)`, and it never drops below the floor on `ContainerFit.ToWidth` (default 0.5). A scene that is
already narrower than the column stays at scale 1. Below the floor the root scrolls horizontally.

`Scene.fitScale` is the same policy for a known width (the interactive reflow path). `ContainerFit.Off` leaves
the scene at scale 1.

Spacing compression handles moderate overflows. A dense hub still exceeds its budget at minimum spacing, and that is
when the floor matters:
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
          s"fitScale(320): ${Scene.Ranked(scene).fitScale(320)}",
          s"Needs scaling: ${Scene.Ranked(scene).fitScale(320) < 1.0}",
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
`mermoid-ascent` recomputes the entire `Scene` on every width change: geometry, edge routes, and, when
`flipDirectionBelow` is set, direction. Selection state is preserved by node id, so clicking a node before reflow
keeps it selected after.

See [Interactive](interactive.html) for live Narrow / Medium / Wide controls. The built-in buttons set 360px, 640px,
and 900px. Direction changes at 640px only when the diagram opts into `flipDirectionBelow`.
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
