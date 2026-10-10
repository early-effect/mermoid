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
    SvgNode.elem("defs")()(kinds.toList.flatMap(kind => List(marker(kind, start = false), marker(kind, start = true)))*)

  def markerAttrs(mark: RelationMark): List[(String, String)] =
    mark.from.toList.map(kind => "marker-start" -> s"url(#${kind.markerId}-start)") ++
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

  /** The arrowhead class fills its shape. An inline style wins, so a foot stays a stroke and a hollow diamond stays
    * hollow.
    */
  private val ink   = "var(--mermoid-line, #333)"
  private val paper = "var(--mermoid-background, #fff)"

  private def paint(fill: String): String =
    s"fill: $fill; stroke: $ink; stroke-width: 2"

  private def marker(kind: MarkKind, start: Boolean): SvgNode =
    // Local +x runs toward the entity. refX is that tip, so the glyph hangs back on the line.
    // The maximum mark (bar or crow toes) sits on the entity. The minimum mark sits on the line.
    val (w, h, refX, body) = kind match
      case MarkKind.Triangle =>
        (14.0, 12.0, 14.0, polygon("0,0 14,6 0,12", paper))
      case MarkKind.Diamond =>
        (18.0, 12.0, 18.0, polygon("0,6 9,0 18,6 9,12", ink))
      case MarkKind.OpenDiamond =>
        (18.0, 12.0, 18.0, polygon("0,6 9,0 18,6 9,12", paper))
      case MarkKind.Arrow =>
        (12.0, 10.0, 12.0, polygon("0,0 12,5 0,10", ink))
      case MarkKind.Lollipop =>
        (14.0, 16.0, 14.0, List(circle(7, 8, 5, paper)))
      case MarkKind.ExactlyOne =>
        (14.0, 16.0, 14.0, List(bar(4), bar(9)))
      case MarkKind.ZeroOrOne =>
        (20.0, 16.0, 20.0, List(circle(6, 8, 4, paper), bar(15)))
      case MarkKind.OneOrMore =>
        (22.0, 16.0, 22.0, bar(2) :: crowToes(22))
      case MarkKind.ZeroOrMore =>
        (26.0, 16.0, 26.0, circle(6, 8, 4, paper) :: crowToes(26))
    val id = if start then s"${kind.markerId}-start" else kind.markerId
    // marker-start's auto orientation points +x along the path, into the source node. Reverse it.
    val orient = if start then "auto-start-reverse" else "auto"
    SvgNode.elem("marker")(
      "id"           -> id,
      "markerWidth"  -> w.f,
      "markerHeight" -> h.f,
      "refX"         -> refX.f,
      "refY"         -> (h / 2).f,
      "orient"       -> orient,
      "markerUnits"  -> "userSpaceOnUse",
    )(body*)
  end marker

  private def polygon(points: String, fill: String): List[SvgNode] =
    List(
      leaf("polygon")(
        "class"  -> PaintClass.Arrowhead.cssName,
        "points" -> points,
        "style"  -> paint(fill),
      )
    )

  private def circle(cx: Double, cy: Double, r: Double, fill: String): SvgNode =
    leaf("circle")(
      "class" -> PaintClass.Arrowhead.cssName,
      "cx"    -> cx.f,
      "cy"    -> cy.f,
      "r"     -> r.f,
      "style" -> paint(fill),
    )

  private def bar(x: Double): SvgNode =
    leaf("line")(
      "class" -> PaintClass.Arrowhead.cssName,
      "x1"    -> x.f,
      "y1"    -> "2",
      "x2"    -> x.f,
      "y2"    -> "14",
      "style" -> paint("none"),
    )

  /** Toes at `tip` (the entity). The heel sits back on the relationship line. */
  private def crowToes(tip: Double): List[SvgNode] =
    val heel = tip - 11
    List(
      leaf("path")(
        "class" -> PaintClass.Arrowhead.cssName,
        "d"     -> s"M${heel.f},8 L${tip.f},2 M${heel.f},8 L${tip.f},8 M${heel.f},8 L${tip.f},14",
        "style" -> paint("none"),
      )
    )
end StructurePainter
