package mermoid

object SvgUtil:

  def escapeXml(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

  def estimateTextWidth(text: String, config: LayoutConfig): Double =
    text.length * config.charWidthEstimate

  def computeNodeSize(label: String, shape: NodeShape, config: LayoutConfig): (Double, Double) =
    val textW = estimateTextWidth(label, config)
    val baseW = Math.max(config.minNodeWidth, textW + config.nodePaddingH * 2)
    val baseH = config.nodeHeight
    shape match
      case NodeShape.Circle | NodeShape.DoubleCircle =>
        val diameter = Math.max(baseW, baseH)
        (diameter, diameter)
      case NodeShape.Rhombus =>
        (baseW * 1.4, baseH * 1.4)
      case NodeShape.Hexagon =>
        (baseW + config.hexagonIndent * 2, baseH)
      case NodeShape.Parallelogram | NodeShape.ParallelogramAlt =>
        (baseW + config.parallelogramSkew * 2, baseH)
      case NodeShape.Trapezoid | NodeShape.TrapezoidAlt =>
        (baseW + config.trapezoidIndent, baseH)
      case _ =>
        (baseW, baseH)
    end match
  end computeNodeSize

  def connectionPoint(node: LayoutNode, target: Point): (Double, Double) =
    val cx = node.center.x
    val cy = node.center.y
    val hw = node.width / 2
    val hh = node.height / 2
    val dx = target.x - cx
    val dy = target.y - cy

    if dx == 0 && dy == 0 then (cx, cy)
    else
      node.shape match
        case NodeShape.Circle | NodeShape.DoubleCircle =>
          val r    = Math.max(hw, hh)
          val dist = Math.sqrt(dx * dx + dy * dy)
          (cx + dx * r / dist, cy + dy * r / dist)
        case NodeShape.Rhombus =>
          val absDx = Math.abs(dx)
          val absDy = Math.abs(dy)
          val t     = (hw * hh) / (hh * absDx + hw * absDy)
          (cx + dx * t, cy + dy * t)
        case _ =>
          val absDx = Math.abs(dx)
          val absDy = Math.abs(dy)
          if absDx * hh > absDy * hw then
            val signX = if dx > 0 then 1.0 else -1.0
            (cx + signX * hw, cy + dy * hw / absDx)
          else
            val signY = if dy > 0 then 1.0 else -1.0
            (cx + dx * hh / absDy, cy + signY * hh)
    end if
  end connectionPoint
end SvgUtil
