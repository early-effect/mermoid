package mermoid

import zio.test.*
import mermoid.css.*

object SvgRendererSpec extends ZIOSpecDefault:

  def spec = suite("SvgRenderer")(
    suite("collectNodes")(
      test("collects nodes from edge statements") {
        val stmts = List(
          FlowStatement.EdgeSt(
            Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
            NodeDef(NodeId("A"), None, NodeShape.Rect),
            NodeDef(NodeId("B"), None, NodeShape.Rect),
          )
        )
        val nodes = StyleResolver.collectNodes(stmts)
        assertTrue(
          nodes.contains(NodeId("A")),
          nodes.contains(NodeId("B")),
          nodes.size == 2,
        )
      },
      test("collects explicit node definitions") {
        val stmts = List(
          FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hello"), NodeShape.Round)),
          FlowStatement.EdgeSt(
            Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
            NodeDef(NodeId("A"), None, NodeShape.Rect),
            NodeDef(NodeId("B"), None, NodeShape.Rect),
          ),
        )
        val a = StyleResolver.collectNodes(stmts).get(NodeId("A"))
        assertTrue(a.flatMap(_.label).contains("Hello"), a.map(_.shape).contains(NodeShape.Round))
      },
      test("collects nodes from subgraphs") {
        val stmts = List(
          FlowStatement.SubgraphSt(
            "sg",
            None,
            None,
            List(
              FlowStatement.EdgeSt(
                Edge(NodeId("X"), NodeId("Y"), EdgeStyle.Arrow, None),
                NodeDef(NodeId("X"), None, NodeShape.Rect),
                NodeDef(NodeId("Y"), None, NodeShape.Rect),
              )
            ),
          )
        )
        val nodes = StyleResolver.collectNodes(stmts)
        assertTrue(nodes.contains(NodeId("X")), nodes.contains(NodeId("Y")))
      },
    ),
    suite("collectEdges")(
      test("collects edges including from subgraphs") {
        val stmts = List(
          FlowStatement.EdgeSt(
            Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
            NodeDef(NodeId("A"), None, NodeShape.Rect),
            NodeDef(NodeId("B"), None, NodeShape.Rect),
          ),
          FlowStatement.SubgraphSt(
            "sg",
            None,
            None,
            List(
              FlowStatement.EdgeSt(
                Edge(NodeId("C"), NodeId("D"), EdgeStyle.Dotted, Some("label")),
                NodeDef(NodeId("C"), None, NodeShape.Rect),
                NodeDef(NodeId("D"), None, NodeShape.Rect),
              )
            ),
          ),
        )
        val edges = StyleResolver.collectEdges(stmts)
        assertTrue(
          edges.size == 2,
          edges(0).from == NodeId("A"),
          edges(1).from == NodeId("C"),
        )
      }
    ),
    suite("layout")(
      test("positions nodes in topological order for TB direction") {
        val config = RenderConfig()
        val nodes  = Map(
          NodeId("A") -> NodeDef(NodeId("A"), Some("Start"), NodeShape.Rect),
          NodeId("B") -> NodeDef(NodeId("B"), Some("End"), NodeShape.Rect),
        )
        val edges          = List(Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None))
        val laid           = Layout.layout(config.layout, Direction.TB, nodes, edges).visibleNodes
        def at(id: NodeId) = laid.find(_.id == id).map(_.center)
        assertTrue(laid.size == 2, at(NodeId("A")).zip(at(NodeId("B"))).exists((a, b) => a.y < b.y))
      },
      test("positions nodes horizontally for LR direction") {
        val config = RenderConfig()
        val nodes  = Map(
          NodeId("A") -> NodeDef(NodeId("A"), None, NodeShape.Rect),
          NodeId("B") -> NodeDef(NodeId("B"), None, NodeShape.Rect),
        )
        val edges          = List(Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None))
        val laid           = Layout.layout(config.layout, Direction.LR, nodes, edges).visibleNodes
        def at(id: NodeId) = laid.find(_.id == id).map(_.center)
        assertTrue(at(NodeId("A")).zip(at(NodeId("B"))).exists((a, b) => a.x < b.x && a.y == b.y))
      },
    ),
    suite("findLabelPosition")(
      test("places label at midpoint when no nodes are nearby") {
        val pos = EdgeRenderer.findLabelPosition(0, 0, 200, 0, 40, 20, Nil)
        assertTrue(pos._1 == 100.0, pos._2 == 0.0)
      },
      test("avoids a node at the midpoint by shifting along the edge") {

        val blockingNode = LayoutNode(NodeId("X"), "X", NodeShape.Rect, Point(100, 0), 80, 50, Map.empty)
        val (mx, _)      = EdgeRenderer.findLabelPosition(0, 0, 200, 0, 40, 20, List(blockingNode))
        // The label should have moved away from x=100
        assertTrue(mx < 99.0 || mx > 101.0)
      },
      test("falls back to midpoint when all positions overlap nodes") {

        // Create a wall of nodes covering the entire edge
        val nodes = (0 to 10).map { i =>
          LayoutNode(NodeId.trusted(s"N$i"), s"N$i", NodeShape.Rect, Point(i * 20.0, 0), 30, 30, Map.empty)
        }
        val (mx, my) = EdgeRenderer.findLabelPosition(0, 0, 200, 0, 40, 20, nodes)
        assertTrue(mx == 100.0, my == 0.0)
      },
    ),
    suite("render")(
      test("produces valid SVG with svg tags") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.EdgeSt(
              Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
              NodeDef(NodeId("A"), None, NodeShape.Rect),
              NodeDef(NodeId("B"), None, NodeShape.Rect),
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.startsWith("<svg"),
          svg.endsWith("</svg>"),
          svg.contains("arrowhead"),
          svg.contains("""class="diagram-bg""""),
          svg.contains("<path"),
          svg.contains("edge-line"),
          svg.contains("<rect"),
          svg.contains("<text"),
        )
      },
      test("respects custom theme") {
        val config  = RenderConfig(theme = ThemeName.Dark)
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Test"), NodeShape.Rect))
          ),
        )
        val svg = SvgRenderer.render(diagram, config)
        assertTrue(
          svg.contains("<style>"),
          svg.contains("--mermoid-primary"),
        )
      },
      test("renders state diagram") {
        val diagram = Diagram.StateDiagram(
          Direction.TB,
          List(
            StateStatement.TransitionSt(StateTransition(NodeId("[*]"), NodeId("Created"), None)),
            StateStatement.TransitionSt(StateTransition(NodeId("Created"), NodeId("Done"), Some("Finish"))),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.startsWith("<svg"),
          svg.endsWith("</svg>"),
          svg.contains("Created"),
          svg.contains("Done"),
          svg.contains("Finish"),
          svg.contains("<circle"), // [*] renders as circle
        )
      },
      test("escapes XML special characters in labels") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.NodeSt(
              NodeDef(NodeId("A"), Some("a < b & c"), NodeShape.Rect)
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("a &lt; b &amp; c"),
          !svg.contains("a < b & c"),
        )
      },
      test("wraps nodes in <g> with class and id") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hello"), NodeShape.Round))
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("""<g class="node node-round" id="node-A">"""),
          svg.contains("""class="node-shape""""),
          svg.contains("""class="node-label""""),
        )
      },
      test("node shape class matches shape type") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.NodeSt(NodeDef(NodeId("D"), Some("Decision"), NodeShape.Rhombus))
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(svg.contains("""<g class="node node-rhombus" id="node-D">"""))
      },
      test("wraps edges in <g> with class, id, and data attributes") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.EdgeSt(
              Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
              NodeDef(NodeId("A"), None, NodeShape.Rect),
              NodeDef(NodeId("B"), None, NodeShape.Rect),
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("""<g class="edge edge-arrow" id="edge-A-B-0" data-from="A" data-to="B">"""),
          svg.contains("""class="edge-line""""),
        )
      },
      test("edge uses alias for id when present") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.EdgeSt(
              Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None, Some("myEdge")),
              NodeDef(NodeId("A"), None, NodeShape.Rect),
              NodeDef(NodeId("B"), None, NodeShape.Rect),
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(svg.contains("""id="edge-myEdge""""))
      },
      test("edge labels paint at LayoutConfig.edgeLabelFontSize") {
        val src =
          Mermaid("""flowchart LR
            |  A -->|go| B
            |""".stripMargin)
        val svg = SvgRenderer.render(
          src.diagram,
          RenderConfig(layout = LayoutConfig(edgeLabelFontSize = 18)),
        )
        assertTrue(svg.contains("""class="edge-label""""), svg.contains("font-size: 18px"))
      },
      test("edge labels have CSS classes") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.EdgeSt(
              Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, Some("yes")),
              NodeDef(NodeId("A"), None, NodeShape.Rect),
              NodeDef(NodeId("B"), None, NodeShape.Rect),
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("""class="edge-label-bg""""),
          svg.contains("""class="edge-label""""),
        )
      },
      test("wraps notes in <g> with class and auto id") {
        val diagram = Diagram.StateDiagram(
          Direction.TB,
          List(
            StateStatement.TransitionSt(StateTransition(NodeId("[*]"), NodeId("Idle"), None)),
            StateStatement.NoteSt(NotePosition.RightOf, NodeId("Idle"), "hello"),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("""<g class="note" id="note-Idle-0">"""),
          svg.contains("""class="note-connector""""),
          svg.contains("""class="note-rect""""),
          svg.contains("""class="note-text""""),
        )
      },
      test("note uses alias for id when present") {
        val diagram = Diagram.StateDiagram(
          Direction.TB,
          List(
            StateStatement.TransitionSt(StateTransition(NodeId("[*]"), NodeId("Idle"), None)),
            StateStatement.NoteSt(NotePosition.RightOf, NodeId("Idle"), "hello", Some("myNote")),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(svg.contains("""id="note-myNote""""))
      },
      test("renders subgraph with CSS classes and id") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.SubgraphSt(
              "sg1",
              Some("My Group"),
              None,
              List(
                FlowStatement.EdgeSt(
                  Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
                  NodeDef(NodeId("A"), None, NodeShape.Rect),
                  NodeDef(NodeId("B"), None, NodeShape.Rect),
                )
              ),
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("""<g class="subgraph" id="subgraph-sg1">"""),
          svg.contains("""class="subgraph-rect""""),
          svg.contains("""class="subgraph-label""""),
          svg.contains("My Group"),
        )
      },
      test("subgraph uses id as label when no label provided") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.SubgraphSt(
              "backend",
              None,
              None,
              List(
                FlowStatement.NodeSt(NodeDef(NodeId("X"), Some("Server"), NodeShape.Rect))
              ),
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("""id="subgraph-backend""""),
          svg.contains(">backend</text>"),
        )
      },
      test("emits <style> block with theme CSS variables") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hi"), NodeShape.Rect))),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("<style>"),
          svg.contains("</style>"),
          svg.contains(".node-shape"),
          svg.contains(".edge-line"),
          svg.contains(".arrowhead"),
        )
      },
      test("resolves CSS variables when resolveVariables is true") {
        val config  = RenderConfig(resolveVariables = true)
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hi"), NodeShape.Rect))),
        )
        val svg = SvgRenderer.render(diagram, config)
        // resolved means no var() references in the CSS
        assertTrue(!svg.contains("var(--mermoid-"))
      },
      test("keeps CSS variables when resolveVariables is false") {
        val config  = RenderConfig(resolveVariables = false)
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hi"), NodeShape.Rect))),
        )
        val svg = SvgRenderer.render(diagram, config)
        assertTrue(
          svg.contains("var(--mermoid-"),
          svg.contains(":root"),
        )
      },
      test("classDef produces CSS rules in <style>") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.ClassDefSt("highlight", Map(CssProperty.Fill -> "#ff0", CssProperty.Stroke -> "#f00")),
            FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hi"), NodeShape.Rect)),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains(".highlight"),
          svg.contains(".highlight .node-shape"),
          svg.contains("fill: #ff0"),
          svg.contains("stroke: #f00"),
          svg.indexOf(".node-shape {") < svg.indexOf(".highlight {"),
        )
      },
      test("class statement adds CSS classes to node <g>") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hi"), NodeShape.Rect)),
            FlowStatement.ClassSt(List(NodeId("A")), "highlight"),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(svg.contains("""class="node node-rect highlight" id="node-A""""))
      },
      test("style statement adds inline style to node <g>") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hi"), NodeShape.Rect)),
            FlowStatement.StyleSt(NodeId("A"), Map(CssProperty.Fill -> "#f00")),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(svg.contains("""style="fill: #f00""""))
      },
      test("::: class suffix adds CSS classes to flowchart nodes") {
        val src =
          Mermaid("""flowchart LR
            |  classDef hot fill:#ffdddd,stroke:#cc0000
            |  A[Start]:::hot --> B[End]
            |""".stripMargin)
        val svg = SvgRenderer.render(src.diagram)
        assertTrue(
          svg.contains("""class="node node-rect hot" id="node-A""""),
          svg.contains(".hot"),
          svg.contains("fill: #ffdddd"),
        )
      },
      test("state classDef and class paint like flowcharts") {
        val src =
          Mermaid("""stateDiagram-v2
            |  classDef happy fill:#1f4a35,stroke:#7dcea0
            |  [*] --> Green
            |  Green --> Yellow: Timer
            |  class Green happy
            |""".stripMargin)
        val svg = SvgRenderer.render(src.diagram)
        assertTrue(
          svg.contains("""class="node node-round happy" id="node-Green""""),
          svg.contains(".happy"),
          svg.contains(".happy .node-shape"),
          svg.contains("fill: #1f4a35"),
          svg.contains("start-end"),
        )
      },
      test("state ::: class suffix paints the target state") {
        val src =
          Mermaid("""stateDiagram-v2
            |  classDef warn fill:#4a4030,stroke:#e0c070
            |  [*] --> Yellow:::warn
            |""".stripMargin)
        val svg = SvgRenderer.render(src.diagram)
        assertTrue(svg.contains("""class="node node-round warn" id="node-Yellow""""))
      },
      test("custom stylesheet merges into <style> block") {
        val custom = Stylesheet(
          rules = List(
            CssRule(
              CssSelector.Class("my-custom"),
              List(CssDeclaration("fill", CssValue.Color("#abc"))),
            )
          )
        )
        val config  = RenderConfig(customStylesheet = Some(custom))
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hi"), NodeShape.Rect))),
        )
        val svg = SvgRenderer.render(diagram, config)
        assertTrue(
          svg.contains(".my-custom"),
          svg.contains("#abc"),
        )
      },
      test("arrowhead uses CSS class instead of inline fill") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.EdgeSt(
              Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
              NodeDef(NodeId("A"), None, NodeShape.Rect),
              NodeDef(NodeId("B"), None, NodeShape.Rect),
            )
          ),
        )
        val svg         = SvgRenderer.render(diagram)
        val markerStart = svg.indexOf("<marker")
        val markerEnd   = svg.indexOf("</marker>", markerStart)
        val markerBlock = svg.substring(markerStart, markerEnd)
        assertTrue(
          markerBlock.contains("""class="arrowhead""""),
          !markerBlock.contains("fill="),
        )
      },
      test("[*] state nodes get start-end CSS class") {
        val diagram = Diagram.StateDiagram(
          Direction.TB,
          List(
            StateStatement.TransitionSt(StateTransition(NodeId("[*]"), NodeId("Idle"), None))
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("start-end"),
          svg.contains("""class="node node-circle start-end""""),
        )
      },
      test("[*] start and end are two markers with distinct ids") {
        val diagram = Diagram.StateDiagram(
          Direction.TB,
          List(
            StateStatement.TransitionSt(StateTransition(NodeId("[*]"), NodeId("A"), None)),
            StateStatement.TransitionSt(StateTransition(NodeId("A"), NodeId("[*]"), None)),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("""id="node-[*]""""),
          svg.contains("""id="node-[*]-end""""),
        )
      },
      test("no inline fill or stroke on node shapes") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(FlowStatement.NodeSt(NodeDef(NodeId("A"), Some("Hi"), NodeShape.Round))),
        )
        val svg = SvgRenderer.render(diagram)
        // node-shape elements should not have inline fill= or stroke= attributes
        val shapeStart = svg.indexOf("""class="node-shape"""")
        val shapeEnd   = svg.indexOf("/>", shapeStart)
        val shapeTag   = svg.substring(shapeStart, shapeEnd)
        assertTrue(
          !shapeTag.contains("fill="),
          !shapeTag.contains("stroke="),
        )
      },
    ),
  )
end SvgRendererSpec
