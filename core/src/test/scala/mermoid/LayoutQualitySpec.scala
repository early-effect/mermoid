package mermoid

import zio.test.*

object LayoutQualitySpec extends ZIOSpecDefault:

  private def rect(id: String) = id -> NodeDef(id, Some(id), NodeShape.Rect)

  private def edge(from: String, to: String) = Edge(from, to, EdgeStyle.Arrow, None)

  /** Classic crossing: A→D and B→C with layers [A,B] / [C,D] in wrong order. */
  private val crossedLayers = List(List("A", "B"), List("C", "D"))
  private val crossedEdges  = List(("A", "D"), ("B", "C"))

  def spec = suite("LayoutQuality")(
    suite("CrossingMinimizer")(
      test("barycenter reduces crossings on a swapped bipartite graph") {
        val before  = LayoutMetrics.totalCrossings(crossedLayers, crossedEdges)
        val after   = CrossingMinimizer.orderLayers(crossedLayers, crossedEdges, iterations = 4)
        val reduced = LayoutMetrics.totalCrossings(after, crossedEdges)
        assertTrue(before == 1, reduced == 0, after(1) == List("D", "C") || after(0) == List("B", "A"))
      },
      test("ordering is idempotent on an already-sorted layering") {
        val good  = List(List("A", "B"), List("C", "D"))
        val edges = List(("A", "C"), ("B", "D"))
        val once  = CrossingMinimizer.orderLayers(good, edges, 4)
        assertTrue(LayoutMetrics.totalCrossings(once, edges) == 0)
      },
    ),
    suite("DummyVertices")(
      test("a two-layer span inserts no dummies") {
        val layers   = List(List("A"), List("B"))
        val expanded = DummyVertices.expand(layers, List(edge("A", "B")))
        assertTrue(expanded.dummies.isEmpty, expanded.routes.isEmpty)
      },
      test("a long-span edge inserts one dummy per intermediate layer") {
        val layers   = List(List("A"), List("B"), List("C"))
        val expanded = DummyVertices.expand(layers, List(edge("A", "C")))
        assertTrue(
          expanded.dummies.size == 1,
          expanded.routes(("A", "C")).size == 1,
          expanded.layers(1).exists(_.startsWith("__dummy_")),
        )
      },
    ),
    suite("layout properties")(
      test("visible nodes never overlap") {
        val nodes = Map(rect("A"), rect("B"), rect("C"), rect("D"))
        val edges = List(edge("A", "C"), edge("A", "D"), edge("B", "C"), edge("B", "D"))
        val laid  = Layout.layout(LayoutConfig(), Direction.TB, nodes, edges)
        assertTrue(!LayoutMetrics.anyNodeOverlap(laid.visibleNodes, gap = 1.0))
      },
      test("forward edges are layer-monotonic after longest-path ranking") {
        val reverseAdj = Map("B" -> List("A"), "C" -> List("B"))
        val layers     = Layout.longestPathLayers(List("A", "B", "C"), reverseAdj)
        assertTrue(layers("A") < layers("B"), layers("B") < layers("C"))
      },
      test("diamond layout has no geometric edge crossings") {
        val nodes = Map(rect("A"), rect("B"), rect("C"), rect("D"))
        val edges = List(edge("A", "B"), edge("A", "C"), edge("B", "D"), edge("C", "D"))
        val laid  = Layout.layout(LayoutConfig(), Direction.TB, nodes, edges)
        assertTrue(LayoutMetrics.edgeCrossings(laid.visibleNodes, edges) == 0)
      },
      test("long-span edges receive route waypoints") {
        val nodes = Map(rect("A"), rect("B"), rect("C"))
        val edges = List(edge("A", "B"), edge("B", "C"), edge("A", "C"))
        val laid  = Layout.layout(LayoutConfig(), Direction.TB, nodes, edges)
        assertTrue(
          laid.routes.contains(("A", "C")),
          laid.routes(("A", "C")).nonEmpty,
          laid.nodes.exists(_.dummy),
          !laid.visibleNodes.exists(_.dummy),
        )
      },
      test("crossed bipartite fixture lays out with few geometric crossings") {
        val nodes = Map(rect("A"), rect("B"), rect("C"), rect("D"))
        val edges = List(edge("A", "D"), edge("B", "C"))
        val laid  = Layout.layout(LayoutConfig(), Direction.TB, nodes, edges)
        assertTrue(LayoutMetrics.edgeCrossings(laid.visibleNodes, edges) == 0)
      },
      test("rendered SVG uses path edges and omits dummy node ids") {
        val src =
          """flowchart TD
            |  A[A] --> B[B]
            |  B --> C[C]
            |  A --> C
            |""".stripMargin
        val svg = MermaidParser.parse(src).map(SvgRenderer.render(_)).toOption.get
        assertTrue(
          svg.contains("""class="edge-line""""),
          svg.contains("<path"),
          !svg.contains("node-__dummy_"),
          svg.contains("markerUnits"),
        )
      },
      test("parallel edges bow with quadratic or cubic curves") {
        val src =
          """flowchart LR
            |  A[A] -->|one| B[B]
            |  A -->|two| B
            |  A -->|three| B
            |""".stripMargin
        val svg   = MermaidParser.parse(src).map(SvgRenderer.render(_)).toOption.get
        val paths = """d="([^"]+)"""".r.findAllMatchIn(svg).map(_.group(1)).filter(_.startsWith("M")).toList
        // The middle parallel (zero offset) stays straight; the outer ones bow.
        assertTrue(paths.size >= 3, paths.count(d => d.contains("Q") || d.contains("C")) >= 2)
      },
      test("long-span edges emit cubic segments through waypoints") {
        val src =
          """flowchart TD
            |  A[A] --> B[B]
            |  B --> C[C]
            |  C --> D[D]
            |  A --> D
            |""".stripMargin
        val svg = MermaidParser.parse(src).map(SvgRenderer.render(_)).toOption.get
        assertTrue(svg.contains(" C"), svg.contains("<path"))
      },
    ),
  )
end LayoutQualitySpec
