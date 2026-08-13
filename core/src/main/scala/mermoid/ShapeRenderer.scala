package mermoid

import mermoid.SvgNode.{leaf, textElem}
import mermoid.css.{PaintClass, WrapperClass}

object ShapeRenderer:

  private[mermoid] def inlineStyle(styles: Map[css.CssProperty, String]): Option[String] =
    if styles.isEmpty then None
    else Some(styles.map((k, v) => s"${k.cssName}: $v").mkString("; "))

  def nodeToSvg(
      node: LayoutNode,
      config: RenderConfig,
      includeLabel: Boolean = true,
      interaction: Option[NodeInteraction] = None,
  ): SvgNode =
    val userClasses = node.cssClasses.map(c => s" $c").mkString
    val titleChild  = interaction.flatMap(_.tooltip).map(t => SvgNode.elem("title")()(SvgNode.Text(t))).toList
    val children    = titleChild ++ shapeToSvg(node, config) ++ Option.when(includeLabel)(labelToSvg(node))
    val attrs       = List(
      "class" -> s"${WrapperClass.Node.cssName} ${node.shape.wrapperClass}$userClasses",
      "id"    -> s"node-${node.id}",
    ) ++ inlineStyle(node.styles).map("style" -> _)
    val group = SvgNode.Element("g", attrs, children)
    interaction.flatMap(_.href) match
      case Some(url) =>
        val linkAttrs = List("href" -> url) ++ interaction.flatMap(_.linkTarget).map("target" -> _)
        SvgNode.Element("a", linkAttrs, List(group))
      case None => group
  end nodeToSvg

  /** The shape primitives for a node — most shapes are one element, a few need two or three. */
  def shapeToSvg(node: LayoutNode, config: RenderConfig): List[SvgNode] =
    val cx = node.center.x
    val cy = node.center.y
    val hw = node.width / 2
    val hh = node.height / 2
    val lc = config.layout

    def box(rx: Double, ry: Double): SvgNode =
      leaf("rect")(
        "class"  -> PaintClass.NodeShape.cssName,
        "x"      -> (cx - hw).f,
        "y"      -> (cy - hh).f,
        "width"  -> node.width.f,
        "height" -> node.height.f,
        "rx"     -> rx.f,
        "ry"     -> ry.f,
      )

    def polygon(points: String): SvgNode =
      leaf("polygon")("class" -> PaintClass.NodeShape.cssName, "points" -> points)

    node.shape match
      case NodeShape.Rect       => List(box(0, 0))
      case NodeShape.Round      => List(box(lc.cornerRadius, lc.cornerRadius))
      case NodeShape.Stadium    => List(box(hh, hh))
      case NodeShape.Subroutine =>
        val inset                       = lc.subroutineInset
        def divider(x: Double): SvgNode =
          leaf("line")(
            "class" -> PaintClass.NodeShape.cssName,
            "x1"    -> x.f,
            "y1"    -> (cy - hh).f,
            "x2"    -> x.f,
            "y2"    -> (cy + hh).f,
          )
        List(
          leaf("rect")(
            "class"  -> PaintClass.NodeShape.cssName,
            "x"      -> (cx - hw).f,
            "y"      -> (cy - hh).f,
            "width"  -> node.width.f,
            "height" -> node.height.f,
          ),
          divider(cx - hw + inset),
          divider(cx + hw - inset),
        )
      case NodeShape.Cylinder =>
        val ry = lc.cylinderRy
        List(
          leaf("path")(
            "class" -> PaintClass.NodeShape.cssName,
            "d" -> (s"M${(cx - hw).f},${(cy - hh + ry).f} A${hw.f},${ry.f} 0 0,1 ${(cx + hw).f},${(cy - hh + ry).f} " +
              s"V${(cy + hh - ry).f} A${hw.f},${ry.f} 0 0,1 ${(cx - hw).f},${(cy + hh - ry).f} Z"),
          ),
          leaf("path")(
            "class" -> PaintClass.NodeShape.cssName,
            "d"     -> s"M${(cx - hw).f},${(cy - hh + ry).f} A${hw.f},${ry.f} 0 0,0 ${(cx + hw).f},${(cy - hh + ry).f}",
            "fill"  -> "none",
          ),
        )
      case NodeShape.Circle =>
        val r = Math.max(hw, hh)
        List(leaf("circle")("class" -> PaintClass.NodeShape.cssName, "cx" -> cx.f, "cy" -> cy.f, "r" -> r.f))
      case NodeShape.DoubleCircle =>
        val r = Math.max(hw, hh)
        List(
          leaf("circle")("class" -> PaintClass.NodeShape.cssName, "cx" -> cx.f, "cy" -> cy.f, "r" -> r.f),
          leaf("circle")(
            "class" -> PaintClass.NodeShape.cssName,
            "cx"    -> cx.f,
            "cy"    -> cy.f,
            "r"     -> (r - lc.doubleCircleGap).f,
            "fill"  -> "none",
          ),
        )
      case NodeShape.Rhombus =>
        List(polygon(s"${cx.f},${(cy - hh).f} ${(cx + hw).f},${cy.f} ${cx.f},${(cy + hh).f} ${(cx - hw).f},${cy.f}"))
      case NodeShape.Hexagon =>
        val indent = lc.hexagonIndent
        List(
          polygon(
            s"${(cx - hw + indent).f},${(cy - hh).f} ${(cx + hw - indent).f},${(cy - hh).f} ${(cx + hw).f},${cy.f} " +
              s"${(cx + hw - indent).f},${(cy + hh).f} ${(cx - hw + indent).f},${(cy + hh).f} ${(cx - hw).f},${cy.f}"
          )
        )
      case NodeShape.Parallelogram =>
        val skew = lc.parallelogramSkew
        List(
          polygon(
            s"${(cx - hw + skew).f},${(cy - hh).f} ${(cx + hw).f},${(cy - hh).f} " +
              s"${(cx + hw - skew).f},${(cy + hh).f} ${(cx - hw).f},${(cy + hh).f}"
          )
        )
      case NodeShape.ParallelogramAlt =>
        val skew = lc.parallelogramSkew
        List(
          polygon(
            s"${(cx - hw).f},${(cy - hh).f} ${(cx + hw - skew).f},${(cy - hh).f} " +
              s"${(cx + hw).f},${(cy + hh).f} ${(cx - hw + skew).f},${(cy + hh).f}"
          )
        )
      case NodeShape.Trapezoid =>
        val indent = lc.trapezoidIndent
        List(
          polygon(
            s"${(cx - hw + indent).f},${(cy - hh).f} ${(cx + hw - indent).f},${(cy - hh).f} " +
              s"${(cx + hw).f},${(cy + hh).f} ${(cx - hw).f},${(cy + hh).f}"
          )
        )
      case NodeShape.TrapezoidAlt =>
        val indent = lc.trapezoidIndent
        List(
          polygon(
            s"${(cx - hw).f},${(cy - hh).f} ${(cx + hw).f},${(cy - hh).f} " +
              s"${(cx + hw - indent).f},${(cy + hh).f} ${(cx - hw + indent).f},${(cy + hh).f}"
          )
        )
    end match
  end shapeToSvg

  def labelToSvg(node: LayoutNode): SvgNode =
    textElem("text")(
      "class"             -> PaintClass.NodeLabel.cssName,
      "x"                 -> node.center.x.f,
      "y"                 -> node.center.y.f,
      "text-anchor"       -> "middle",
      "dominant-baseline" -> "central",
    )(node.label)
end ShapeRenderer
