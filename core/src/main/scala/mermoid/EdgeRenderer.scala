package mermoid

import mermoid.SvgNode.{leaf, textElem}
import mermoid.css.{PaintClass, WrapperClass}
import scala.util.boundary, boundary.break

object EdgeRenderer:

  private def overlapsNode(mx: Double, my: Double, halfW: Double, halfH: Double, node: LayoutNode): Boolean =
    val nhw = node.width / 2
    val nhh = node.height / 2
    val dx  = Math.abs(mx - node.center.x)
    val dy  = Math.abs(my - node.center.y)
    dx < halfW + nhw && dy < halfH + nhh

  private[mermoid] def findLabelPosition(
      x1: Double,
      y1: Double,
      x2: Double,
      y2: Double,
      labelW: Double,
      labelH: Double,
      nodes: Iterable[LayoutNode],
  ): (Double, Double) =
    val halfW = labelW / 2
    val halfH = labelH / 2
    val steps = 10
    boundary:
      for i <- 0 to steps do
        val offsets = if i == 0 then List(0.5) else List(0.5 - i * 0.04, 0.5 + i * 0.04)
        for t <- offsets do
          if t >= 0.1 && t <= 0.9 then
            val mx = x1 + (x2 - x1) * t
            val my = y1 + (y2 - y1) * t
            if !nodes.exists(n => overlapsNode(mx, my, halfW, halfH, n)) then break((mx, my))
      ((x1 + x2) / 2, (y1 + y2) / 2)
  end findLabelPosition

  /** Estimated bounding box of an edge label, used both for placement and for the label background. */
  private[mermoid] def labelSize(lbl: String, lc: LayoutConfig): (Double, Double) =
    (lbl.length * lc.edgeLabelFontSize * 0.6 + 8, lc.edgeLabelFontSize + 8)

  /** Path points + label rect for one edge; used to size the SVG so strokes and labels are not clipped. */
  private[mermoid] def edgeInk(
      config: RenderConfig,
      edge: LayoutEdge,
      nodeMap: Map[String, LayoutNode],
      waypoints: List[Point],
      loopSide: SelfLoopSide,
  ): List[InkBox] =
    (nodeMap.get(edge.from), nodeMap.get(edge.to)) match
      case (Some(from), Some(to)) =>
        val lc = config.layout
        if edge.from == edge.to then selfLoopInk(lc, edge, from, loopSide)
        else routedEdgeInk(lc, edge, from, to, nodeMap, waypoints)
      case _ => Nil
  end edgeInk

  private def selfLoopInk(
      lc: LayoutConfig,
      edge: LayoutEdge,
      node: LayoutNode,
      side: SelfLoopSide,
  ): List[InkBox] =
    val cx           = node.center.x
    val cy           = node.center.y
    val hw           = node.width / 2
    val hh           = node.height / 2
    val loopSize     = Math.max(hw, hh) * 0.8 + lc.selfLoopSize
    val labelYOffset = edge.selfLoopIndex * (lc.edgeLabelFontSize + 16)

    val (pathBoxes, labelCenter) =
      if side == SelfLoopSide.Right then
        val (startX, startY, endX, endY) = node.shape match
          case NodeShape.Circle | NodeShape.DoubleCircle =>
            val r      = Math.max(hw, hh)
            val angle1 = Math.toRadians(-20)
            val angle2 = Math.toRadians(-70)
            (
              cx + r * Math.cos(angle1),
              cy - r * Math.sin(angle1),
              cx + r * Math.cos(angle2),
              cy - r * Math.sin(angle2),
            )
          case _ =>
            (cx + hw, cy + hh * 0.15, cx + hw * 0.3, cy + hh)
        val apexX      = cx + hw + loopSize
        val apexY      = cy + hh + loopSize * 0.5
        val labelX     = apexX - loopSize * 0.2
        val labelBaseY = (startY + apexY) / 2 - (lc.edgeLabelFontSize + 16) * 0.5
        (
          List(
            InkBox.fromPoint(Point(startX, startY)),
            InkBox.fromPoint(Point(endX, endY)),
            InkBox.fromPoint(Point(apexX, startY)),
            InkBox.fromPoint(Point(apexX, apexY)),
          ),
          Point(labelX, labelBaseY + labelYOffset),
        )
      else
        val (startX, startY, endX, endY) = node.shape match
          case NodeShape.Circle | NodeShape.DoubleCircle =>
            val r      = Math.max(hw, hh)
            val angle1 = Math.toRadians(110)
            val angle2 = Math.toRadians(70)
            (
              cx - r * Math.cos(angle1),
              cy - r * Math.sin(angle1),
              cx - r * Math.cos(angle2),
              cy - r * Math.sin(angle2),
            )
          case _ =>
            (cx - hw * 0.25, cy - hh, cx + hw * 0.25, cy - hh)
        val apexY      = startY - loopSize
        val labelBaseY = apexY + loopSize * 0.35
        (
          List(
            InkBox.fromPoint(Point(startX, startY)),
            InkBox.fromPoint(Point(endX, endY)),
            InkBox.fromPoint(Point(startX, apexY)),
            InkBox.fromPoint(Point(endX, apexY)),
          ),
          Point(cx, labelBaseY + labelYOffset),
        )

    val labelBox = edge.label.map { lbl =>
      val (w, h) = labelSize(lbl, lc)
      InkBox.fromCenter(labelCenter.x, labelCenter.y, w, h)
    }
    pathBoxes ++ labelBox
  end selfLoopInk

  private def routedEdgeInk(
      lc: LayoutConfig,
      edge: LayoutEdge,
      from: LayoutNode,
      to: LayoutNode,
      nodeMap: Map[String, LayoutNode],
      waypoints: List[Point],
  ): List[InkBox] =
    val offset =
      if edge.edgeCount > 1 then (edge.edgeIndex - (edge.edgeCount - 1) / 2.0) * lc.parallelEdgeSpacing
      else 0.0

    val rawMids     = waypoints.map(offsetPoint(_, from.center, to.center, offset))
    val firstTarget =
      rawMids.headOption.getOrElse(
        Point(
          to.center.x + perp(from.center, to.center)._1 * offset,
          to.center.y + perp(from.center, to.center)._2 * offset,
        )
      )
    val lastApproach =
      rawMids.lastOption.getOrElse(
        Point(
          from.center.x + perp(from.center, to.center)._1 * offset,
          from.center.y + perp(from.center, to.center)._2 * offset,
        )
      )

    val (x1, y1) = SvgUtil.connectionPoint(from, firstTarget)
    val (x2, y2) = SvgUtil.connectionPoint(to, lastApproach)
    val start    = Point(x1, y1)
    val endRaw   = Point(x2, y2)
    val approach = rawMids.lastOption.getOrElse(start)
    val end      = shortenPathEnd(approach, endRaw, hasArrow(edge.style), lc)

    val points =
      if rawMids.nonEmpty then start :: rawMids ::: List(end)
      else if Math.abs(offset) > 1e-6 then
        val (nx, ny) = perp(from.center, to.center)
        val mid      = Point((start.x + end.x) / 2 + nx * offset * 1.25, (start.y + end.y) / 2 + ny * offset * 1.25)
        List(start, mid, end)
      else List(start, end)

    // Diagonal bows use a perpendicular control point outside the chord.
    val bowBoxes =
      points match
        case a :: b :: Nil if !nearlyAxisAligned(a, b) =>
          val (nx0, ny0) = perp(a, b)
          val dx         = b.x - a.x
          val (nx, ny)   = if Math.abs(dx) < 1e-6 || nx0 * dx > 0 then (nx0, ny0) else (-nx0, -ny0)
          val dist       = Math.hypot(b.x - a.x, b.y - a.y)
          val bow        = Math.min(24.0, dist * 0.12)
          List(InkBox.fromPoint(Point((a.x + b.x) / 2 + nx * bow, (a.y + b.y) / 2 + ny * bow)))
        case _ => Nil

    val pathBoxes = points.map(InkBox.fromPoint(_)) ++ bowBoxes
    val labelBox  = edge.label.map { lbl =>
      val (w, h)   = labelSize(lbl, lc)
      val mid      = labelPointOnPath(points)
      val visible  = nodeMap.values.filter(!_.dummy)
      val (mx, my) =
        if points.size <= 2 then findLabelPosition(start.x, start.y, end.x, end.y, w, h, visible)
        else mid
      InkBox.fromCenter(mx, my, w, h)
    }
    pathBoxes ++ labelBox
  end routedEdgeInk

  private def edgeLabelSvg(lbl: String, mx: Double, my: Double, lc: LayoutConfig): List[SvgNode] =
    val (w, h) = labelSize(lbl, lc)
    List(
      leaf("rect")(
        "class"  -> PaintClass.EdgeLabelBg.cssName,
        "x"      -> (mx - w / 2).f,
        "y"      -> (my - h / 2).f,
        "width"  -> w.f,
        "height" -> h.f,
        "rx"     -> "3",
        "ry"     -> "3",
      ),
      textElem("text")(
        "class"             -> PaintClass.EdgeLabel.cssName,
        "x"                 -> mx.f,
        "y"                 -> my.f,
        "text-anchor"       -> "middle",
        "dominant-baseline" -> "central",
      )(lbl),
    )
  end edgeLabelSvg

  private def edgeId(edge: LayoutEdge): String =
    edge.alias.getOrElse(s"${edge.from}-${edge.to}-${edge.edgeIndex}")

  /** `marker-end` attribute, present only for the arrow-headed edge styles. */
  private def markerAttr(style: EdgeStyle): List[(String, String)] =
    if style.arrowhead then List("marker-end" -> s"url(#${PaintClass.Arrowhead.cssName})") else Nil

  def edgeToSvg(
      config: RenderConfig,
      edge: LayoutEdge,
      nodeMap: Map[String, LayoutNode],
      selfLoopSide: SelfLoopSide = SelfLoopSide.Top,
      waypoints: List[Point] = Nil,
  ): SvgNode =
    val from   = nodeMap(edge.from)
    val to     = nodeMap(edge.to)
    val lc     = config.layout
    val marker = markerAttr(edge.style)

    val selfLoopClass = if edge.from == edge.to then s" ${PaintClass.SelfLoop.cssName}" else ""
    val children      =
      if edge.from == edge.to then renderSelfLoop(lc, edge, from, selfLoopSide, marker)
      else renderRoutedEdge(lc, edge, from, to, nodeMap, marker, waypoints)

    SvgNode.Element(
      "g",
      List(
        "class"     -> s"${WrapperClass.Edge.cssName} ${edge.style.wrapperClass}$selfLoopClass",
        "id"        -> s"edge-${edgeId(edge)}",
        "data-from" -> edge.from,
        "data-to"   -> edge.to,
      ),
      children,
    )
  end edgeToSvg

  private def curve(d: String, marker: List[(String, String)]): SvgNode =
    SvgNode.Element("path", List("class" -> PaintClass.EdgeLine.cssName, "d" -> d, "fill" -> "none") ++ marker, Nil)

  private def renderSelfLoop(
      lc: LayoutConfig,
      edge: LayoutEdge,
      node: LayoutNode,
      side: SelfLoopSide,
      marker: List[(String, String)],
  ): List[SvgNode] =
    val cx       = node.center.x
    val cy       = node.center.y
    val hw       = node.width / 2
    val hh       = node.height / 2
    val loopSize = Math.max(hw, hh) * 0.8 + lc.selfLoopSize

    // Only the first loop on a node draws the arc; subsequent ones stack their labels along it.
    val labelYOffset = edge.selfLoopIndex * (lc.edgeLabelFontSize + 16)

    if side == SelfLoopSide.Right then
      val (startX, startY, endX, endY) = node.shape match
        case NodeShape.Circle | NodeShape.DoubleCircle =>
          val r      = Math.max(hw, hh)
          val angle1 = Math.toRadians(-20)
          val angle2 = Math.toRadians(-70)
          (cx + r * Math.cos(angle1), cy - r * Math.sin(angle1), cx + r * Math.cos(angle2), cy - r * Math.sin(angle2))
        case _ =>
          (cx + hw, cy + hh * 0.15, cx + hw * 0.3, cy + hh)
      val apexX = cx + hw + loopSize
      val apexY = cy + hh + loopSize * 0.5
      // Approach from the apex so marker orientation matches the cubic's arrival, not the chord.
      val shortened = shortenPathEnd(Point(apexX, apexY), Point(endX, endY), hasArrow(edge.style), lc)
      val line      = Option.when(edge.selfLoopIndex == 0)(
        curve(
          s"M${startX.f},${startY.f} C${apexX.f},${startY.f} ${apexX.f},${apexY.f} ${shortened.x.f},${shortened.y.f}",
          marker,
        )
      )
      val labelX     = apexX - loopSize * 0.2
      val labelBaseY = (startY + apexY) / 2 - (lc.edgeLabelFontSize + 16) * 0.5
      val label      = edge.label.map(lbl => edgeLabelSvg(lbl, labelX, labelBaseY + labelYOffset, lc)).getOrElse(Nil)
      line.toList ++ label
    else
      val (startX, startY, endX, endY) = node.shape match
        case NodeShape.Circle | NodeShape.DoubleCircle =>
          val r      = Math.max(hw, hh)
          val angle1 = Math.toRadians(110)
          val angle2 = Math.toRadians(70)
          (cx - r * Math.cos(angle1), cy - r * Math.sin(angle1), cx - r * Math.cos(angle2), cy - r * Math.sin(angle2))
        case _ =>
          (cx - hw * 0.25, cy - hh, cx + hw * 0.25, cy - hh)
      val apexY     = startY - loopSize
      val shortened = shortenPathEnd(Point(endX, apexY), Point(endX, endY), hasArrow(edge.style), lc)
      val line      = Option.when(edge.selfLoopIndex == 0)(
        curve(
          s"M${startX.f},${startY.f} C${startX.f},${apexY.f} ${endX.f},${apexY.f} ${shortened.x.f},${shortened.y.f}",
          marker,
        )
      )
      val labelBaseY = apexY + loopSize * 0.35
      val label      = edge.label.map(lbl => edgeLabelSvg(lbl, cx, labelBaseY + labelYOffset, lc)).getOrElse(Nil)
      line.toList ++ label
    end if
  end renderSelfLoop

  private def hasArrow(style: EdgeStyle): Boolean = style match
    case EdgeStyle.Open | EdgeStyle.DottedOpen => false
    case _                                     => true

  /** Pull the path end back a little so the marker tip sits just clear of the node stroke.
    *
    * The arrowhead marker places its tip at the path endpoint (`refX` = tip), so we only need a small gap; pulling back
    * by a full [[LayoutConfig.arrowSize]] would leave a disconnected hole.
    */
  private def shortenPathEnd(approachFrom: Point, end: Point, arrow: Boolean, lc: LayoutConfig): Point =
    if !arrow then end
    else
      val dx   = end.x - approachFrom.x
      val dy   = end.y - approachFrom.y
      val dist = Math.sqrt(dx * dx + dy * dy)
      if dist < 1e-6 then end
      else
        val t = Math.min(lc.arrowTipPadding + 2.0, dist * 0.25)
        Point(end.x - dx / dist * t, end.y - dy / dist * t)

  private def renderRoutedEdge(
      lc: LayoutConfig,
      edge: LayoutEdge,
      from: LayoutNode,
      to: LayoutNode,
      nodeMap: Map[String, LayoutNode],
      marker: List[(String, String)],
      waypoints: List[Point],
  ): List[SvgNode] =
    val offset =
      if edge.edgeCount > 1 then (edge.edgeIndex - (edge.edgeCount - 1) / 2.0) * lc.parallelEdgeSpacing
      else 0.0

    val rawMids     = waypoints.map(offsetPoint(_, from.center, to.center, offset))
    val firstTarget =
      rawMids.headOption.getOrElse(
        Point(
          to.center.x + perp(from.center, to.center)._1 * offset,
          to.center.y + perp(from.center, to.center)._2 * offset,
        )
      )
    val lastApproach =
      rawMids.lastOption.getOrElse(
        Point(
          from.center.x + perp(from.center, to.center)._1 * offset,
          from.center.y + perp(from.center, to.center)._2 * offset,
        )
      )

    val (x1, y1) = SvgUtil.connectionPoint(from, firstTarget)
    val (x2, y2) = SvgUtil.connectionPoint(to, lastApproach)
    val start    = Point(x1, y1)
    val endRaw   = Point(x2, y2)
    val approach = rawMids.lastOption.getOrElse(start)
    val end      = shortenPathEnd(approach, endRaw, hasArrow(edge.style), lc)

    // Parallel edges need an explicit bow: offset endpoints alone stay collinear and look straight.
    // Long spans already curve through dummy waypoints (3+ points → cubics).
    val points =
      if rawMids.nonEmpty then start :: rawMids ::: List(end)
      else if Math.abs(offset) > 1e-6 then
        val (nx, ny) = perp(from.center, to.center)
        val mid      = Point((start.x + end.x) / 2 + nx * offset * 1.25, (start.y + end.y) / 2 + ny * offset * 1.25)
        List(start, mid, end)
      else List(start, end)

    val d    = smoothPath(points)
    val line = curve(d, marker)

    val label = edge.label
      .map { lbl =>
        val (w, h)   = labelSize(lbl, lc)
        val mid      = labelPointOnPath(points)
        val visible  = nodeMap.values.filter(!_.dummy)
        val (mx, my) =
          if points.size <= 2 then findLabelPosition(start.x, start.y, end.x, end.y, w, h, visible)
          else mid
        edgeLabelSvg(lbl, mx, my, lc)
      }
      .getOrElse(Nil)

    line :: label
  end renderRoutedEdge

  private def perp(a: Point, b: Point): (Double, Double) =
    val dx   = b.x - a.x
    val dy   = b.y - a.y
    val dist = Math.sqrt(dx * dx + dy * dy)
    if dist < 1e-6 then (0.0, 0.0) else (-dy / dist, dx / dist)

  private def offsetPoint(p: Point, from: Point, to: Point, offset: Double): Point =
    if offset == 0.0 then p
    else
      val (nx, ny) = perp(from, to)
      Point(p.x + nx * offset, p.y + ny * offset)

  private def labelPointOnPath(points: List[Point]): (Double, Double) =
    if points.size < 2 then (0.0, 0.0)
    else
      val mid = points.size / 2
      val p   = points(mid)
      (p.x, p.y)

  /** Smooth cubic Bezier through waypoints.
    *
    * Two-point paths stay straight when axis-aligned (typical adjacent-layer hop). A diagonal pair gets a light
    * perpendicular bow so the quadratic is not degenerate (a mid-chord control point is collinear with the ends).
    */
  private[mermoid] def smoothPath(points: List[Point]): String =
    points match
      case Nil           => ""
      case p :: Nil      => s"M${p.x.f},${p.y.f}"
      case a :: b :: Nil =>
        if nearlyAxisAligned(a, b) then s"M${a.x.f},${a.y.f} L${b.x.f},${b.y.f}"
        else
          val (nx0, ny0) = perp(a, b)
          // Bow "outward" horizontally so left-going and right-going fans mirror each other.
          // A fixed CCW normal bows left edges out and right edges in (looks backward).
          val dx       = b.x - a.x
          val (nx, ny) = if Math.abs(dx) < 1e-6 || nx0 * dx > 0 then (nx0, ny0) else (-nx0, -ny0)
          val dist     = Math.hypot(b.x - a.x, b.y - a.y)
          val bow      = Math.min(24.0, dist * 0.12)
          val mx       = (a.x + b.x) / 2 + nx * bow
          val my       = (a.y + b.y) / 2 + ny * bow
          s"M${a.x.f},${a.y.f} Q${mx.f},${my.f} ${b.x.f},${b.y.f}"
      case pts =>
        val padded = pts.head :: pts ::: List(pts.last)
        val segs   = (0 until pts.size - 1).map { i =>
          val p0 = padded(i)
          val p1 = padded(i + 1)
          val p2 = padded(i + 2)
          val p3 = padded(i + 3)
          val c1 = Point(p1.x + (p2.x - p0.x) / 6, p1.y + (p2.y - p0.y) / 6)
          val c2 = Point(p2.x - (p3.x - p1.x) / 6, p2.y - (p3.y - p1.y) / 6)
          s"C${c1.x.f},${c1.y.f} ${c2.x.f},${c2.y.f} ${p2.x.f},${p2.y.f}"
        }
        s"M${pts.head.x.f},${pts.head.y.f} ${segs.mkString(" ")}"
  end smoothPath

  private def nearlyAxisAligned(a: Point, b: Point): Boolean =
    Math.abs(a.x - b.x) < 1e-6 || Math.abs(a.y - b.y) < 1e-6
end EdgeRenderer
