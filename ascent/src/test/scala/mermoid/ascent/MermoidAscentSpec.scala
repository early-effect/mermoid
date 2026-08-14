package mermoid.ascent

import ascent.html.Html
import mermoid.*
import zio.*
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
        ui   <- MermoidAscent.diagramInteractive(flow, initialWidth = 800, showWidthControls = true)
        html <- Html.render(ui)
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
    test("static hybrid CSS-fits the parent column") {
      val ui = MermoidAscent.diagram(flow)
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("mermoid-fit"),
        html.contains("--mermoid-scene-width"),
      )
    },
    test("diagramControlled paints the host-selected node") {
      import _root_.ascent.squawk.sq
      for
        selected <- sq(Option.empty[String])
        width    <- sq(640.0)
        _        <- selected.set(Some("A"))
        html     <- Html.render(
          MermoidAscent.diagramControlled(flow, selected, _ => ZIO.unit, width)
        )
      yield assertTrue(
        html.contains("is-selected"),
        html.contains("mermoid-node"),
        !html.contains("Narrow"),
      )
      end for
    },
    test("embedded style is not HTML-escaped") {
      val ui = MermoidAscent.diagram(flow)
      for html <- Html.render(ui)
      yield assertTrue(!html.contains("&lt;style"), html.contains("<style"))
    },
    test("class and classDef paint the hybrid node-shape") {
      val src =
        """flowchart LR
          |  classDef warn fill:#4a4030,stroke:#e0c070
          |  A[Tired] --> B[Zipx]
          |  class A warn
          |""".stripMargin
      val ui = MermoidAscent.diagram(src)
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("node-shape"),
        html.contains("class=\"mermoid-node"),
        html.contains("warn"),
        html.contains("background: #4a4030") || html.contains("background:#4a4030"),
        html.contains("border-color: #e0c070") || html.contains("border-color:#e0c070"),
      )
    },
    test("state classDef paints the hybrid node-shape") {
      val src =
        """stateDiagram-v2
          |  classDef happy fill:#1f4a35,stroke:#7dcea0
          |  [*] --> Green
          |  class Green happy
          |""".stripMargin
      val ui = MermoidAscent.diagram(src)
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("happy"),
        html.contains("background: #1f4a35") || html.contains("background:#1f4a35"),
        html.contains("border-color: #7dcea0") || html.contains("border-color:#7dcea0"),
      )
    },
    test("subgraph frames land in the hybrid SVG layer") {
      val src =
        """flowchart LR
          |  subgraph g [Group]
          |    A --> B
          |  end
          |""".stripMargin
      val ui = MermoidAscent.diagram(src)
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("subgraph-rect"),
        html.contains("subgraph-g") || html.contains("id=\"subgraph-g\""),
      )
    },
  )
end MermoidAscentSpec
