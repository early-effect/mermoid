package mermoid

import zio.test.*

object LayoutQualitySpec extends ZIOSpecDefault:

  private def rect(id: String) = id -> NodeDef(id, Some(id), NodeShape.Rect)

  private def edge(from: String, to: String) = Edge(from, to, EdgeStyle.Arrow, None)

  /** Issue 48: a pipeline with one Retry per fault leaf and two cycle-closers. */
  private val initiativeMachine =
    """stateDiagram-v2
      |    [*] --> Draft
      |    Draft --> JourneyPreparing: Launch
      |    Draft --> Archived: Archive
      |    JourneyPreparing --> PredictionsRequested: JourneyPublished
      |    JourneyPreparing --> JourneyPreparationFaulted: DsmlBridgeFailed, PreparationWatch
      |    JourneyPreparationFaulted --> JourneyPreparing: Retry
      |    PredictionsRequested --> Enqueueing: PredictionsPromoted
      |    PredictionsRequested --> PredictionsFaulted: DatabricksFailed +3
      |    PredictionsFaulted --> PredictionsRequested: Retry
      |    Enqueueing --> Live: EnqueueComplete
      |    Enqueueing --> EnqueueFaulted: DsmlBridgeFailed, EnqueueWatch
      |    EnqueueFaulted --> Enqueueing: Retry
      |    Live --> CycleExiting: EndCycle
      |    Live --> Stopping: Stop
      |    Live --> Archived: Archive
      |    CycleExiting --> JourneyRepublishing: ExitsComplete
      |    CycleExiting --> CycleExitFaulted: JourneysFailed, ExitsWatch
      |    CycleExitFaulted --> CycleExiting: Retry
      |    JourneyRepublishing --> PredictionsRequested: JourneyPublished
      |    JourneyRepublishing --> JourneyRepublishFaulted: DsmlBridgeFailed, RepublishWatch
      |    JourneyRepublishFaulted --> JourneyRepublishing: Retry
      |    Stopping --> Stopped: ExitsComplete
      |    Stopping --> StopFaulted: JourneysFailed, ExitsWatch
      |    StopFaulted --> Stopping: Retry
      |    Stopped --> JourneyRepublishing: Restart
      |    Stopped --> Archived: Archive
      |""".stripMargin

  private val faultOf = List(
    "JourneyPreparationFaulted" -> "JourneyPreparing",
    "PredictionsFaulted"        -> "PredictionsRequested",
    "EnqueueFaulted"            -> "Enqueueing",
    "CycleExitFaulted"          -> "CycleExiting",
    "JourneyRepublishFaulted"   -> "JourneyRepublishing",
    "StopFaulted"               -> "Stopping",
  )

  private def sceneOf(src: String): DiagramScene =
    MermaidParser.parse(src) match
      case Right(d)  => DiagramLayout.scene(d)
      case Left(err) => throw new IllegalArgumentException(err)

  /** Cluster centers that share a rank. Layer pitch is ~100px; same-rank height jitter stays under 30. */
  private def ranks(scene: DiagramScene, axis: LayoutNode => Double): Map[String, Int] =
    val sorted = scene.visibleNodes.map(n => n.id -> axis(n)).sortBy(_._2)
    val groups = sorted.foldLeft(List.empty[List[(String, Double)]]) {
      case (Nil, item)             => List(List(item))
      case (current :: rest, item) =>
        val anchor = current.map(_._2).min
        if item._2 - anchor <= 30.0 then (item :: current) :: rest
        else List(item) :: current :: rest
    }
    groups.reverse.zipWithIndex.flatMap((group, idx) => group.map((id, _) => id -> idx)).toMap
  end ranks

  private def rankOk(layer: Map[String, Int]): Boolean =
    val draft   = layer("Draft")
    val others  = layer.filter((id, _) => id != "[*]" && id != "Draft")
    val faults  = faultOf.forall((fault, hop) => layer(fault) == layer(hop) + 1)
    val deepest = layer.values.max
    layer("[*]") < draft && others.forall((_, rank) => rank > draft) && faults &&
    layer("Archived") == deepest && layer("Live") > draft

  /** Crossings of the routed polyline (center, waypoints, center), not the straight chord. */
  private def routedCrossings(scene: DiagramScene): Int =
    val pos       = scene.visibleNodes.map(n => n.id -> n.center).toMap
    val polylines = scene.edges.filter(e => e.from != e.to).flatMap { e =>
      pos.get(e.from).zip(pos.get(e.to)).map { (a, b) =>
        a :: scene.routes.getOrElse((e.from, e.to), Nil) ::: List(b)
      }
    }
    val segs = polylines.flatMap(pts => pts.zip(pts.tail))
    segs.indices.foldLeft(0) { (acc, i) =>
      val (p1, p2) = segs(i)
      acc + segs.drop(i + 1).count { (p3, p4) =>
        val share = p1 == p3 || p1 == p4 || p2 == p3 || p2 == p4
        !share && segmentsCross(p1, p2, p3, p4)
      }
    }
  end routedCrossings

  private def segmentsCross(p1: Point, p2: Point, p3: Point, p4: Point): Boolean =
    def cross(a: Point, b: Point, c: Point): Double =
      (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
    val d1 = cross(p3, p4, p1)
    val d2 = cross(p3, p4, p2)
    val d3 = cross(p1, p2, p3)
    val d4 = cross(p1, p2, p4)
    ((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0))

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
      test("a two-cycle ranks the forward node first for either id order") {
        def ys(ids: List[String]): Map[String, Double] =
          val nodes = ids.map(id => id -> NodeDef(id, Some(id), NodeShape.Rect)).toMap
          val edges = List(edge("A", "B"), edge("B", "A"))
          Layout.layout(LayoutConfig(), Direction.TB, nodes, edges).visibleNodes.map(n => n.id -> n.center.y).toMap
        val ab = ys(List("A", "B"))
        val ba = ys(List("B", "A"))
        assertTrue(ab("A") < ab("B"), ba("A") < ba("B"))
      },
      test("state retry edges sit one rank after their hop, not on the first rank") {
        val tb = sceneOf(initiativeMachine)
        val lr =
          sceneOf(s"stateDiagram-v2\n    direction LR\n${initiativeMachine.linesIterator.drop(1).mkString("\n")}")
        val tbR = ranks(tb, _.center.y)
        val lrR = ranks(lr, _.center.x)
        assertTrue(
          rankOk(tbR),
          rankOk(lrR),
          tb.direction == Direction.TB,
          lr.direction == Direction.LR,
          lr.width > lr.height,
          routedCrossings(tb) <= 8,
          routedCrossings(lr) <= 8,
        )
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
