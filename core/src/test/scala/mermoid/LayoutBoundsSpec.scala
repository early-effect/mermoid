package mermoid

import zio.test.*

/** Guards against edge paths / labels being clipped by a too-tight viewBox. */
object LayoutBoundsSpec extends ZIOSpecDefault:

  private val orderFsm =
    """stateDiagram-v2
      |    [*] --> Pending
      |    Pending --> Paid: payment captured
      |    Pending --> Cancelled: customer cancels
      |    Paid --> Shipped: carrier accepts
      |    Shipped --> Delivered: scan
      |    Delivered --> [*]
      |    Cancelled --> [*]
      |""".stripMargin

  private val branchingFlow =
    """flowchart TD
      |  A[Start] -->|go left| L[Left]
      |  A -->|go right| R[Right]
      |  L --> Z[Done]
      |  R --> Z
      |""".stripMargin

  private def parse(src: String): Diagram =
    MermaidParser.parse(src) match
      case Right(d)  => d
      case Left(err) => throw new IllegalArgumentException(err)

  private def viewBoxSize(svg: String): (Double, Double) =
    val w = """width="([^"]+)"""".r.findFirstMatchIn(svg).map(_.group(1).toDouble).get
    val h = """height="([^"]+)"""".r.findFirstMatchIn(svg).map(_.group(1).toDouble).get
    (w, h)

  private def labelBoxes(svg: String): List[(Double, Double, Double, Double)] =
    """class="edge-label-bg"[^>]*x="([^"]+)"[^>]*y="([^"]+)"[^>]*width="([^"]+)"[^>]*height="([^"]+)"""".r
      .findAllMatchIn(svg)
      .map(m => (m.group(1).toDouble, m.group(2).toDouble, m.group(3).toDouble, m.group(4).toDouble))
      .toList

  private def pathNumbers(svg: String): List[Double] =
    """[-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[eE][-+]?\d+)?""".r
      .findAllIn(svg)
      .flatMap(s => s.toDoubleOption)
      .toList

  private def inside(x: Double, y: Double, w: Double, h: Double, cw: Double, ch: Double, eps: Double = 0.5) =
    x >= -eps && y >= -eps && x + w <= cw + eps && y + h <= ch + eps

  def spec = suite("LayoutBounds")(
    test("order FSM edge labels stay inside the viewBox") {
      val svg      = SvgRenderer.render(parse(orderFsm))
      val (cw, ch) = viewBoxSize(svg)
      val clipped  = labelBoxes(svg).filterNot((x, y, w, h) => inside(x, y, w, h, cw, ch))
      assertTrue(clipped.isEmpty, labelBoxes(svg).nonEmpty)
    },
    test("branching flowchart edge labels stay inside the viewBox") {
      val svg      = SvgRenderer.render(parse(branchingFlow))
      val (cw, ch) = viewBoxSize(svg)
      val clipped  = labelBoxes(svg).filterNot((x, y, w, h) => inside(x, y, w, h, cw, ch))
      // Spot-check a few edge path coordinates extracted from d="..." stay in-bounds.
      val pathAttrs = """d="([^"]+)"""".r.findAllMatchIn(svg).map(_.group(1)).mkString(" ")
      val pathXs    = pathNumbers(pathAttrs).grouped(2).collect { case Seq(x, _) => x }.toList
      val pathOk    = pathXs.forall(x => x >= -1.0 && x <= cw + 1.0)
      assertTrue(clipped.isEmpty, pathOk, labelBoxes(svg).size >= 2)
    },
    test("direction LR lays a chain out wider than tall with labels inside the viewBox") {
      val src =
        """stateDiagram-v2
          |    direction LR
          |    [*] --> Draft
          |    Draft --> Preparing: Launch
          |    Preparing --> Live: Published
          |    Live --> [*]
          |""".stripMargin
      val scene     = DiagramLayout.scene(parse(src))
      val svg       = SvgRenderer.render(parse(src))
      val (cw, ch)  = viewBoxSize(svg)
      val clipped   = labelBoxes(svg).filterNot((x, y, w, h) => inside(x, y, w, h, cw, ch))
      val draft     = scene.nodeMap("Draft").center.x
      val preparing = scene.nodeMap("Preparing").center.x
      val live      = scene.nodeMap("Live").center.x
      assertTrue(
        scene.direction == Direction.LR,
        scene.width > scene.height,
        draft < preparing,
        preparing < live,
        clipped.isEmpty,
        labelBoxes(svg).nonEmpty,
      )
    },
    test("direction LR keeps a retry self-loop and its back edge on the canvas") {
      val src =
        """stateDiagram-v2
          |    direction LR
          |    [*] --> Draft
          |    Draft --> JourneyPreparing: Launch
          |    JourneyPreparing --> JourneyPreparing: tick
          |    JourneyPreparing --> PredictionsRequested: JourneyPublished
          |    JourneyPreparing --> JourneyPreparationFaulted: DsmlBridgeFailed
          |    JourneyPreparationFaulted --> JourneyPreparing: Retry
          |    classDef fault fill:#3a2a10,stroke:#f59e0b
          |    class JourneyPreparationFaulted fault
          |""".stripMargin
      val scene    = DiagramLayout.scene(parse(src))
      val svg      = SvgRenderer.render(parse(src))
      val (cw, ch) = viewBoxSize(svg)
      val clipped  = labelBoxes(svg).filterNot((x, y, w, h) => inside(x, y, w, h, cw, ch))
      assertTrue(
        scene.direction == Direction.LR,
        scene.loopSide == SelfLoopSide.Top,
        scene.width > scene.height,
        clipped.isEmpty,
        labelBoxes(svg).nonEmpty,
      )
    },
    test("order FSM ranks start above end (split [*])") {
      val scene   = DiagramLayout.scene(parse(orderFsm))
      val pending = scene.nodeMap("Pending").center.y
      val paid    = scene.nodeMap("Paid").center.y
      val endY    = scene.nodeMap("[*]-end").center.y
      val startY  = scene.nodeMap("[*]").center.y
      assertTrue(startY < pending, pending < paid, paid < endY)
    },
    test("fit shifts negative ink and pads the far edge") {
      val boxes  = List(InkBox(-20, 10, 40, 20), InkBox(100, 50, 30, 10))
      val fitted = LayoutBounds.fit(40, boxes)
      assertTrue(
        fitted.shiftX == 60.0, // 40 - (-20)
        fitted.shiftY == 30.0, // 40 - 10
        fitted.width == 230.0, // maxX 130 + shift 60 + pad 40
        fitted.height == 130.0, // maxY 60 + shift 30 + pad 40
      )
    },
  )
end LayoutBoundsSpec
