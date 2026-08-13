package mermoid

import zio.test.*
import mermoid.css.*

object SvgRendererSpec extends ZIOSpecDefault:

  def spec = suite("SvgRenderer")(
    suite("collectNodes")(
      test("collects nodes from edge statements") {
        val stmts = List(
          FlowStatement.EdgeSt(
            Edge("A", "B", EdgeStyle.Arrow, None),
            NodeDef("A", None, NodeShape.Rect),
            NodeDef("B", None, NodeShape.Rect),
          )
        )
        val nodes = StyleResolver.collectNodes(stmts)
        assertTrue(
          nodes.contains("A"),
          nodes.contains("B"),
          nodes.size == 2,
        )
      },
      test("collects explicit node definitions") {
        val stmts = List(
          FlowStatement.NodeSt(NodeDef("A", Some("Hello"), NodeShape.Round)),
          FlowStatement.EdgeSt(
            Edge("A", "B", EdgeStyle.Arrow, None),
            NodeDef("A", None, NodeShape.Rect),
            NodeDef("B", None, NodeShape.Rect),
          ),
        )
        val nodes = StyleResolver.collectNodes(stmts)
        assertTrue(
          nodes("A").label == Some("Hello"),
          nodes("A").shape == NodeShape.Round,
        )
      },
      test("collects nodes from subgraphs") {
        val stmts = List(
          FlowStatement.SubgraphSt(
            "sg",
            None,
            None,
            List(
              FlowStatement.EdgeSt(
                Edge("X", "Y", EdgeStyle.Arrow, None),
                NodeDef("X", None, NodeShape.Rect),
                NodeDef("Y", None, NodeShape.Rect),
              )
            ),
          )
        )
        val nodes = StyleResolver.collectNodes(stmts)
        assertTrue(nodes.contains("X"), nodes.contains("Y"))
      },
    ),
    suite("collectEdges")(
      test("collects edges including from subgraphs") {
        val stmts = List(
          FlowStatement.EdgeSt(
            Edge("A", "B", EdgeStyle.Arrow, None),
            NodeDef("A", None, NodeShape.Rect),
            NodeDef("B", None, NodeShape.Rect),
          ),
          FlowStatement.SubgraphSt(
            "sg",
            None,
            None,
            List(
              FlowStatement.EdgeSt(
                Edge("C", "D", EdgeStyle.Dotted, Some("label")),
                NodeDef("C", None, NodeShape.Rect),
                NodeDef("D", None, NodeShape.Rect),
              )
            ),
          ),
        )
        val edges = StyleResolver.collectEdges(stmts)
        assertTrue(
          edges.size == 2,
          edges(0).from == "A",
          edges(1).from == "C",
        )
      }
    ),
    suite("collectStyleDefs")(
      test("applies classDef styles via class statement") {
        val stmts = List(
          FlowStatement
            .ClassDefSt("highlight", Map(CssProperty.Fill -> "#ff0", CssProperty.Stroke -> "#f00")),
          FlowStatement.ClassSt(List("A", "B"), "highlight"),
        )
        val styles = StyleResolver.collectStyleDefs(stmts)
        assertTrue(
          styles("A")(CssProperty.Fill) == "#ff0",
          styles("B")(CssProperty.Stroke) == "#f00",
        )
      },
      test("direct style overrides class style") {
        val stmts = List(
          FlowStatement.ClassDefSt("cls", Map(CssProperty.Fill -> "#aaa")),
          FlowStatement.ClassSt(List("A"), "cls"),
          FlowStatement.StyleSt("A", Map(CssProperty.Fill -> "#bbb")),
        )
        val styles = StyleResolver.collectStyleDefs(stmts)
        assertTrue(styles("A")(CssProperty.Fill) == "#bbb")
      },
    ),
    suite("layout")(
      test("positions nodes in topological order for TB direction") {
        val config = RenderConfig()
        val nodes  = Map(
          "A" -> NodeDef("A", Some("Start"), NodeShape.Rect),
          "B" -> NodeDef("B", Some("End"), NodeShape.Rect),
        )
        val edges = List(Edge("A", "B", EdgeStyle.Arrow, None))
        val laid  = Layout.layout(config.layout, Direction.TB, nodes, edges).visibleNodes
        assertTrue(
          laid.size == 2,
          laid.find(_.id == "A").get.center.y < laid
            .find(_.id == "B")
            .get
            .center
            .y,
        )
      },
      test("positions nodes horizontally for LR direction") {
        val config = RenderConfig()
        val nodes  = Map(
          "A" -> NodeDef("A", None, NodeShape.Rect),
          "B" -> NodeDef("B", None, NodeShape.Rect),
        )
        val edges = List(Edge("A", "B", EdgeStyle.Arrow, None))
        val laid  = Layout.layout(config.layout, Direction.LR, nodes, edges).visibleNodes
        assertTrue(
          laid
            .find(_.id == "A")
            .get
            .center
            .x < laid.find(_.id == "B").get.center.x,
          laid.find(_.id == "A").get.center.y == laid
            .find(_.id == "B")
            .get
            .center
            .y,
        )
      },
    ),
    suite("findLabelPosition")(
      test("places label at midpoint when no nodes are nearby") {
        val pos = EdgeRenderer.findLabelPosition(0, 0, 200, 0, 40, 20, Nil)
        assertTrue(pos._1 == 100.0, pos._2 == 0.0)
      },
      test("avoids a node at the midpoint by shifting along the edge") {

        val blockingNode = LayoutNode("X", "X", NodeShape.Rect, Point(100, 0), 80, 50, Map.empty)
        val (mx, _)      = EdgeRenderer.findLabelPosition(0, 0, 200, 0, 40, 20, List(blockingNode))
        // The label should have moved away from x=100
        assertTrue(mx < 99.0 || mx > 101.0)
      },
      test("falls back to midpoint when all positions overlap nodes") {

        // Create a wall of nodes covering the entire edge
        val nodes = (0 to 10).map { i =>
          LayoutNode(s"N$i", s"N$i", NodeShape.Rect, Point(i * 20.0, 0), 30, 30, Map.empty)
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
              Edge("A", "B", EdgeStyle.Arrow, None),
              NodeDef("A", None, NodeShape.Rect),
              NodeDef("B", None, NodeShape.Rect),
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
            FlowStatement.NodeSt(NodeDef("A", Some("Test"), NodeShape.Rect))
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
          List(
            StateStatement.TransitionSt(StateTransition("[*]", "Created", None)),
            StateStatement.TransitionSt(StateTransition("Created", "Done", Some("Finish"))),
          )
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
              NodeDef("A", Some("a < b & c"), NodeShape.Rect)
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
            FlowStatement.NodeSt(NodeDef("A", Some("Hello"), NodeShape.Round))
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
            FlowStatement.NodeSt(NodeDef("D", Some("Decision"), NodeShape.Rhombus))
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
              Edge("A", "B", EdgeStyle.Arrow, None),
              NodeDef("A", None, NodeShape.Rect),
              NodeDef("B", None, NodeShape.Rect),
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
              Edge("A", "B", EdgeStyle.Arrow, None, Some("myEdge")),
              NodeDef("A", None, NodeShape.Rect),
              NodeDef("B", None, NodeShape.Rect),
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(svg.contains("""id="edge-myEdge""""))
      },
      test("edge labels have CSS classes") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.EdgeSt(
              Edge("A", "B", EdgeStyle.Arrow, Some("yes")),
              NodeDef("A", None, NodeShape.Rect),
              NodeDef("B", None, NodeShape.Rect),
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
          List(
            StateStatement.TransitionSt(StateTransition("[*]", "Idle", None)),
            StateStatement.NoteSt(NotePosition.RightOf, "Idle", "hello"),
          )
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
          List(
            StateStatement.TransitionSt(StateTransition("[*]", "Idle", None)),
            StateStatement.NoteSt(NotePosition.RightOf, "Idle", "hello", Some("myNote")),
          )
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
                  Edge("A", "B", EdgeStyle.Arrow, None),
                  NodeDef("A", None, NodeShape.Rect),
                  NodeDef("B", None, NodeShape.Rect),
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
                FlowStatement.NodeSt(NodeDef("X", Some("Server"), NodeShape.Rect))
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
          List(FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Rect))),
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
          List(FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Rect))),
        )
        val svg = SvgRenderer.render(diagram, config)
        // resolved means no var() references in the CSS
        assertTrue(!svg.contains("var(--mermoid-"))
      },
      test("keeps CSS variables when resolveVariables is false") {
        val config  = RenderConfig(resolveVariables = false)
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Rect))),
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
            FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Rect)),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains(".highlight"),
          svg.contains("fill: #ff0"),
          svg.contains("stroke: #f00"),
        )
      },
      test("class statement adds CSS classes to node <g>") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Rect)),
            FlowStatement.ClassSt(List("A"), "highlight"),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(svg.contains("""class="node node-rect highlight" id="node-A""""))
      },
      test("style statement adds inline style to node <g>") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Rect)),
            FlowStatement.StyleSt("A", Map(CssProperty.Fill -> "#f00")),
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(svg.contains("""style="fill: #f00""""))
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
          List(FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Rect))),
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
              Edge("A", "B", EdgeStyle.Arrow, None),
              NodeDef("A", None, NodeShape.Rect),
              NodeDef("B", None, NodeShape.Rect),
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
          List(
            StateStatement.TransitionSt(StateTransition("[*]", "Idle", None))
          )
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(
          svg.contains("start-end"),
          svg.contains("""class="node node-circle start-end""""),
        )
      },
      test("no inline fill or stroke on node shapes") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Round))),
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
