package mermoid

import mermoid.SvgNode.{leaf, textElem}
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
  private def labelSize(lbl: String, lc: LayoutConfig): (Double, Double) =
    (lbl.length * lc.edgeLabelFontSize * 0.6 + 8, lc.edgeLabelFontSize + 8)

  private def edgeLabelSvg(lbl: String, mx: Double, my: Double, lc: LayoutConfig): List[SvgNode] =
    val (w, h) = labelSize(lbl, lc)
    List(
      leaf("rect")(
        "class"  -> "edge-label-bg",
        "x"      -> (mx - w / 2).f,
        "y"      -> (my - h / 2).f,
        "width"  -> w.f,
        "height" -> h.f,
        "rx"     -> "3",
        "ry"     -> "3",
      ),
      textElem("text")(
        "class"             -> "edge-label",
        "x"                 -> mx.f,
        "y"                 -> my.f,
        "text-anchor"       -> "middle",
        "dominant-baseline" -> "central",
      )(lbl),
    )
  end edgeLabelSvg

  private[mermoid] def edgeStyleCssClass(style: EdgeStyle): String = style match
    case EdgeStyle.Arrow      => "arrow"
    case EdgeStyle.Open       => "open"
    case EdgeStyle.Dotted     => "dotted"
    case EdgeStyle.Thick      => "thick"
    case EdgeStyle.DottedOpen => "dotted-open"

  private def edgeId(edge: LayoutEdge): String =
    edge.alias.getOrElse(s"${edge.from}-${edge.to}-${edge.edgeIndex}")

  /** `marker-end` attribute, present only for the arrow-headed edge styles. */
  private def markerAttr(style: EdgeStyle): List[(String, String)] = style match
    case EdgeStyle.Open | EdgeStyle.DottedOpen => Nil
    case _                                     => List("marker-end" -> "url(#arrowhead)")

  def edgeToSvg(
      config: RenderConfig,
      edge: LayoutEdge,
      nodeMap: Map[String, LayoutNode],
      selfLoopSide: SelfLoopSide = SelfLoopSide.Top,
  ): SvgNode =
    val from   = nodeMap(edge.from)
    val to     = nodeMap(edge.to)
    val lc     = config.layout
    val marker = markerAttr(edge.style)

    val styleClass    = edgeStyleCssClass(edge.style)
    val selfLoopClass = if edge.from == edge.to then " self-loop" else ""
    val children      =
      if edge.from == edge.to then renderSelfLoop(lc, edge, from, selfLoopSide, marker)
      else renderStraightEdge(lc, edge, from, to, nodeMap, marker)

    SvgNode.Element(
      "g",
      List(
        "class"     -> s"edge edge-$styleClass$selfLoopClass",
        "id"        -> s"edge-${edgeId(edge)}",
        "data-from" -> edge.from,
        "data-to"   -> edge.to,
      ),
      children,
    )
  end edgeToSvg

  private def curve(d: String, marker: List[(String, String)]): SvgNode =
    SvgNode.Element("path", List("class" -> "edge-line", "d" -> d, "fill" -> "none") ++ marker, Nil)

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
    val labelYOffset = edge.selfLoopIndex * (lc.edgeLabelFontSize + 12)

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
      val line  = Option.when(edge.selfLoopIndex == 0)(
        curve(s"M${startX.f},${startY.f} C${apexX.f},${startY.f} ${apexX.f},${apexY.f} ${endX.f},${endY.f}", marker)
      )
      val labelX     = apexX - loopSize * 0.2
      val labelBaseY = (startY + apexY) / 2 - (lc.edgeLabelFontSize + 12) * 0.5
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
      val apexY = startY - loopSize
      val line  = Option.when(edge.selfLoopIndex == 0)(
        curve(s"M${startX.f},${startY.f} C${startX.f},${apexY.f} ${endX.f},${apexY.f} ${endX.f},${endY.f}", marker)
      )
      val labelBaseY = apexY + loopSize * 0.35
      val label      = edge.label.map(lbl => edgeLabelSvg(lbl, cx, labelBaseY + labelYOffset, lc)).getOrElse(Nil)
      line.toList ++ label
    end if
  end renderSelfLoop

  private val parallelEdgeSpacing = 20.0

  private def renderStraightEdge(
      lc: LayoutConfig,
      edge: LayoutEdge,
      from: LayoutNode,
      to: LayoutNode,
      nodeMap: Map[String, LayoutNode],
      marker: List[(String, String)],
  ): List[SvgNode] =
    if edge.edgeCount > 1 then renderCurvedParallelEdge(lc, edge, from, to, nodeMap, marker)
    else
      val (x1, y1) = SvgUtil.connectionPoint(from, to.center)
      val (x2, y2) = SvgUtil.connectionPoint(to, from.center)

      val line = SvgNode.Element(
        "line",
        List(
          "class" -> "edge-line",
          "x1"    -> x1.f,
          "y1"    -> y1.f,
          "x2"    -> x2.f,
          "y2"    -> y2.f,
        ) ++ marker,
        Nil,
      )

      val label = edge.label
        .map { lbl =>
          val (w, h)   = labelSize(lbl, lc)
          val (mx, my) = findLabelPosition(x1, y1, x2, y2, w, h, nodeMap.values)
          edgeLabelSvg(lbl, mx, my, lc)
        }
        .getOrElse(Nil)

      line :: label

  private def renderCurvedParallelEdge(
      lc: LayoutConfig,
      edge: LayoutEdge,
      from: LayoutNode,
      to: LayoutNode,
      nodeMap: Map[String, LayoutNode],
      marker: List[(String, String)],
  ): List[SvgNode] =
    // Offset each parallel edge perpendicular to the line between node centers
    val dx   = to.center.x - from.center.x
    val dy   = to.center.y - from.center.y
    val dist = Math.sqrt(dx * dx + dy * dy)
    if dist == 0 then Nil
    else
      // Unit normal perpendicular to the edge direction
      val nx = -dy / dist
      val ny = dx / dist

      // Center the group of parallel edges: offset = (index - (count-1)/2) * spacing
      val offset = (edge.edgeIndex - (edge.edgeCount - 1) / 2.0) * parallelEdgeSpacing

      // Offset the target points used for connection point calculation
      val offsetFromTarget = Point(to.center.x + nx * offset, to.center.y + ny * offset)
      val offsetToTarget   = Point(from.center.x + nx * offset, from.center.y + ny * offset)
      val (x1, y1)         = SvgUtil.connectionPoint(from, offsetFromTarget)
      val (x2, y2)         = SvgUtil.connectionPoint(to, offsetToTarget)

      // Control point at midpoint offset perpendicular
      val mx = (x1 + x2) / 2 + nx * offset * 2
      val my = (y1 + y2) / 2 + ny * offset * 2

      val line = curve(s"M${x1.f},${y1.f} Q${mx.f},${my.f} ${x2.f},${y2.f}", marker)

      val label = edge.label
        .map { lbl =>
          // Label position at the quadratic bezier midpoint (t=0.5)
          val labelX = (x1 + 2 * mx + x2) / 4
          val labelY = (y1 + 2 * my + y2) / 4
          edgeLabelSvg(lbl, labelX, labelY, lc)
        }
        .getOrElse(Nil)

      line :: label
    end if
  end renderCurvedParallelEdge
end EdgeRenderer
