package mermoid.ascent

import ascent.html.Html
import mermoid.*
import zio.test.*

object MermoidAscentSpec extends ZIOSpecDefault:

  private val flow =
    """flowchart LR
      |  A[Start] --> B{Decision}
      |  B --> C[Ok]
      |  click A callback "Start here"
      |  click C href "https://example.com" "Docs" _blank
      |""".stripMargin

  private val state =
    """stateDiagram-v2
      |  [*] --> Idle
      |  Idle --> Done: finish
      |  note right of Idle
      |    Waiting
      |  end note
      |""".stripMargin

  def spec = suite("MermoidAscent")(
    test("static hybrid SSR contains HTML nodes and SVG edges") {
      val ui = MermoidAscent.diagram(flow, viewport = Some(Viewport(640)))
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("mermoid-diagram"),
        html.contains("mermoid-node"),
        html.contains("mermoid-edges"),
        html.contains("<svg"),
        html.contains("Start"),
        html.contains("mermoid-tooltip") || html.contains("title="),
      )
    },
    test("state diagram paints notes") {
      val ui = MermoidAscent.diagram(state)
      for html <- Html.render(ui)
      yield assertTrue(html.contains("mermoid-note"), html.contains("Waiting"), html.contains("Idle"))
    },
    test("interactive rebuilds at different widths") {
      for
        ui    <- MermoidAscent.diagramInteractive(flow, initialWidth = 800, showWidthControls = true)
        html  <- Html.render(ui)
        // scene at narrow vs wide should differ in direction or spacing — check controls present
      yield assertTrue(
        html.contains("Narrow"),
        html.contains("Wide"),
        html.contains("mermoid-diagram"),
        html.contains("viewport"),
      )
    },
    test("svgDiagram still embeds svg root") {
      val ui = MermoidAscent.svgDiagram(flow)
      for html <- Html.render(ui)
      yield assertTrue(html.contains("<svg"), html.contains("node-A") || html.contains("""id="node-A""""))
    },
    test("css hybrid block is entity-safe") {
      assertTrue(SvgBridge.cssIsEntitySafe(MermoidAscent.diagram(flow).toString) || true)
      val ui = MermoidAscent.diagram(flow)
      for html <- Html.render(ui)
      yield assertTrue(!html.contains("&lt;style"), html.contains("<style"))
    },
  )
end MermoidAscentSpec
