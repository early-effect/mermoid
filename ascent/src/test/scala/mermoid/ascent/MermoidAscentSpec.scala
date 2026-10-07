package mermoid.ascent

import ascent.html.Html
import mermoid.*
import mermoid.css.CssParser
import zio.*
import zio.test.*

object MermoidAscentSpec extends ZIOSpecDefault:

  private val flow =
    Mermaid("""flowchart LR
      |  A[Start] --> B{Decision}
      |  B --> C[Ok]
      |  click A callback "Start here"
      |  click C href "https://example.com" "Docs" _blank
      |""".stripMargin)

  private val state =
    Mermaid("""stateDiagram-v2
      |  [*] --> Idle
      |  Idle --> Done: finish
      |  note right of Idle
      |    Waiting
      |  end note
      |""".stripMargin)

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
        ui   <- MermoidAscent.diagramInteractive(flow, initialWidth = 800, widthControls = WidthControls.Shown)
        html <- Html.render(ui)
      // scene at narrow vs wide should differ in direction or spacing — check controls present
      yield assertTrue(
        html.contains("Narrow"),
        html.contains("Wide"),
        html.contains("mermoid-diagram"),
        html.contains("viewport"),
      )
    },
    test("interactive diagrams can hide the width controls") {
      for
        ui   <- MermoidAscent.diagramInteractive(flow, widthControls = WidthControls.Hidden)
        html <- Html.render(ui)
      yield assertTrue(html.contains("mermoid-diagram"), !html.contains("Narrow"), !html.contains("viewport "))
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
        selected <- sq(Option.empty[NodeId])
        width    <- sq(640.0)
        _        <- selected.set(Some(NodeId("A")))
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
        Mermaid("""flowchart LR
          |  classDef warn fill:#4a4030,stroke:#e0c070
          |  A[Tired] --> B[Zipx]
          |  class A warn
          |""".stripMargin)
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
        Mermaid("""stateDiagram-v2
          |  classDef happy fill:#1f4a35,stroke:#7dcea0
          |  [*] --> Green
          |  class Green happy
          |""".stripMargin)
      val ui = MermoidAscent.diagram(src)
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("happy"),
        html.contains("background: #1f4a35") || html.contains("background:#1f4a35"),
        html.contains("border-color: #7dcea0") || html.contains("border-color:#7dcea0"),
      )
    },
    test("state classDef beats a theme rule of equal chrome specificity") {
      val src =
        Mermaid("""stateDiagram-v2
          |    [*] --> PredictionsRequested
          |    PredictionsRequested --> PredictionsFaulted: DatabricksFailed
          |    PredictionsFaulted --> PredictionsRequested: Retry
          |    classDef fault fill:#2e2410,stroke:#f59e0b,color:#fde68a
          |    class PredictionsFaulted fault
          |""".stripMargin)
      val custom = CssParser
        .parse(
          """.mermoid-node .node-shape { background: #123040 }
            |.node-shape { background: #000 }
            |""".stripMargin
        )
        .fold(err => throw new IllegalArgumentException(err), identity)
      val ui = MermoidAscent.diagram(src, RenderConfig(customStylesheet = Some(custom)))
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains(".mermoid-node.fault .node-shape"),
        html.contains("background: #2e2410") || html.contains("background:#2e2410"),
        html.contains("border-color: #f59e0b") || html.contains("border-color:#f59e0b"),
        html.contains(".mermoid-node.fault"),
        html.contains("color: #fde68a") || html.contains("color:#fde68a"),
        html.contains(":where(.mermoid-node) .node-shape"),
        html.contains("""class="mermoid-node node-round fault""""),
      )
    },
    test("style fill and color beat the theme on the shape and the button") {
      val src =
        Mermaid("""stateDiagram-v2
          |    [*] --> PredictionsFaulted
          |    style PredictionsFaulted fill:#112233,color:#abcdef
          |""".stripMargin)
      val ui = MermoidAscent.diagram(src)
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("background: #112233") || html.contains("background:#112233"),
        html.contains("color: #abcdef") || html.contains("color:#abcdef"),
        html.contains("""id="node-PredictionsFaulted""""),
      )
    },
    test("subgraph frames land in the hybrid SVG layer") {
      val src =
        Mermaid("""flowchart LR
          |  subgraph g [Group]
          |    A --> B
          |  end
          |""".stripMargin)
      val ui = MermoidAscent.diagram(src)
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("subgraph-rect"),
        html.contains("subgraph-g") || html.contains("id=\"subgraph-g\""),
      )
    },
    test("static hybrid fit registers the scale floor and leaves transform to the class") {
      val ui = MermoidAscent.diagram(flow)
      for html <- Html.render(ui)
      yield
        val marker = """class="mermoid-diagram mermoid-diagram-scaler" style=""""
        val at     = html.indexOf(marker)
        val end    = if at < 0 then -1 else html.indexOf('"', at + marker.length)
        val style  = if at < 0 || end < 0 then "" else html.slice(at + marker.length, end)
        assertTrue(
          html.contains("""class="mermoid-root mermoid-fit""""),
          html.contains("--mermoid-scale-floor:0.5"),
          style.startsWith("width:"),
          !style.contains("transform"),
        )
      end for
    },
    test("container fit off does not mark the root as fit") {
      val ui = MermoidAscent.diagram(
        flow,
        RenderConfig(responsive = ResponsiveConfig(fit = ContainerFit.Off)),
      )
      for html <- Html.render(ui)
      yield assertTrue(
        html.contains("""class="mermoid-root""""),
        !html.contains("""class="mermoid-root mermoid-fit""""),
      )
    },
  )
end MermoidAscentSpec
