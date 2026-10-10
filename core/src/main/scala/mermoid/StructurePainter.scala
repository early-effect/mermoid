package mermoid

import mermoid.SvgNode.{leaf, textElem}
import mermoid.css.{PaintClass, WrapperClass}

/** Class and entity boxes, and the glyphs that are not flowchart arrows. */
private[mermoid] object StructurePainter:

  def box(node: LayoutNode, compartment: CompartmentBox): SvgNode =
    val top  = node.center.y - node.height / 2
    val left = node.center.x - node.width / 2
    val rect = leaf("rect")(
      "class"  -> PaintClass.NodeShape.cssName,
      "x"      -> left.f,
      "y"      -> top.f,
      "width"  -> node.width.f,
      "height" -> node.height.f,
      "rx"     -> "4",
    )
    val headerLine =
      if compartment.attributeHeight > 0 || compartment.operationHeight > 0 then
        List(hline(left, top + compartment.headerHeight, node.width))
      else Nil
    val attrLine =
      if !compartment.entity && compartment.attributeHeight > 0 && compartment.operationHeight > 0 then
        List(hline(left, top + compartment.headerHeight + compartment.attributeHeight, node.width))
      else Nil
    val stereo = compartment.stereotype.toList.map { name =>
      text(node.center.x, top + 14, s"<<$name>>", middle = true)
    }
    val titleY =
      top + compartment.headerHeight - (if compartment.stereotype.isDefined then 8 else compartment.headerHeight / 2)
    val title = text(node.center.x, titleY, compartment.title, middle = true)
    val attrs = lines(left + 8, top + compartment.headerHeight + 14, compartment.attributes)
    val ops = lines(left + 8, top + compartment.headerHeight + compartment.attributeHeight + 14, compartment.operations)
    SvgNode.elem("g")(
      "class" -> s"${WrapperClass.Node.cssName} ${node.shape.wrapperClass}${node.cssClasses.map(c => s" $c").mkString}",
      "id"    -> s"node-${node.id.value}",
    )((rect :: headerLine ++ attrLine ++ stereo ++ List(title) ++ attrs ++ ops)*)
  end box

  def markers(kinds: Set[MarkKind]): SvgNode =
    SvgNode.elem("defs")()(kinds.toList.map(marker)*)

  def markerAttrs(mark: RelationMark): List[(String, String)] =
    mark.from.toList.map(kind => "marker-start" -> s"url(#${kind.markerId})") ++
      mark.to.toList.map(kind => "marker-end" -> s"url(#${kind.markerId})")

  private def hline(x: Double, y: Double, width: Double): SvgNode =
    leaf("line")(
      "class" -> PaintClass.NodeShape.cssName,
      "x1"    -> x.f,
      "y1"    -> y.f,
      "x2"    -> (x + width).f,
      "y2"    -> y.f,
    )

  private def text(x: Double, y: Double, value: String, middle: Boolean): SvgNode =
    textElem("text")(
      "class"             -> PaintClass.NodeLabel.cssName,
      "x"                 -> x.f,
      "y"                 -> y.f,
      "text-anchor"       -> (if middle then "middle" else "start"),
      "dominant-baseline" -> "central",
    )(value)

  private def lines(x: Double, y0: Double, values: List[String]): List[SvgNode] =
    values.zipWithIndex.map { (value, index) =>
      text(x, y0 + index * 16, value, middle = false)
    }

  private def marker(kind: MarkKind): SvgNode =
    val (w, h, refX, body) = kind match
      case MarkKind.Triangle =>
        (14.0, 12.0, 14.0, polygon("0,0 14,6 0,12", filled = false))
      case MarkKind.Diamond =>
        (16.0, 12.0, 16.0, polygon("0,6 8,0 16,6 8,12", filled = true))
      case MarkKind.OpenDiamond =>
        (16.0, 12.0, 16.0, polygon("0,6 8,0 16,6 8,12", filled = false))
      case MarkKind.Arrow =>
        (12.0, 10.0, 12.0, polygon("0,0 12,5 0,10", filled = true))
      case MarkKind.Lollipop =>
        (
          14.0,
          12.0,
          14.0,
          List(
            leaf("circle")(
              "class" -> PaintClass.Arrowhead.cssName,
              "cx"    -> "6",
              "cy"    -> "6",
              "r"     -> "5",
              "fill"  -> "none",
            )
          ),
        )
      case MarkKind.ExactlyOne =>
        (10.0, 14.0, 10.0, List(bar(2), bar(7)))
      case MarkKind.ZeroOrOne =>
        (
          16.0,
          14.0,
          16.0,
          List(
            leaf("circle")(
              "class" -> PaintClass.Arrowhead.cssName,
              "cx"    -> "5",
              "cy"    -> "7",
              "r"     -> "4",
              "fill"  -> "none",
            ),
            bar(12),
          ),
        )
      case MarkKind.OneOrMore =>
        (16.0, 14.0, 16.0, crow(0) :+ bar(12))
      case MarkKind.ZeroOrMore =>
        (
          18.0,
          14.0,
          18.0,
          crow(0) :+ leaf("circle")(
            "class" -> PaintClass.Arrowhead.cssName,
            "cx"    -> "13",
            "cy"    -> "7",
            "r"     -> "4",
            "fill"  -> "none",
          ),
        )
    SvgNode.elem("marker")(
      "id"           -> kind.markerId,
      "markerWidth"  -> w.f,
      "markerHeight" -> h.f,
      "refX"         -> refX.f,
      "refY"         -> (h / 2).f,
      "orient"       -> "auto",
      "markerUnits"  -> "userSpaceOnUse",
    )(body*)
  end marker

  private def polygon(points: String, filled: Boolean): List[SvgNode] =
    List(
      leaf("polygon")(
        "class"  -> PaintClass.Arrowhead.cssName,
        "points" -> points,
        "fill"   -> (if filled then "context-stroke" else "var(--mermoid-background, #fff)"),
      )
    )

  private def bar(x: Double): SvgNode =
    leaf("line")(
      "class" -> PaintClass.Arrowhead.cssName,
      "x1"    -> x.f,
      "y1"    -> "1",
      "x2"    -> x.f,
      "y2"    -> "13",
    )

  private def crow(x: Double): List[SvgNode] =
    List(
      leaf("path")(
        "class" -> PaintClass.Arrowhead.cssName,
        "d"     -> s"M${x.f},1 L${(x + 8).f},7 L${x.f},13 M${(x + 8).f},7 L${x.f},7",
        "fill"  -> "none",
      )
    )
end StructurePainter
