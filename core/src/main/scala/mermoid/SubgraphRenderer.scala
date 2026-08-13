package mermoid

import mermoid.SvgNode.{leaf, textElem}
import mermoid.css.{PaintClass, WrapperClass}

object SubgraphRenderer:

  private val subgraphPadding = 20.0
  private val labelHeight     = 20.0

  /** `None` when none of the subgraph's members were laid out — there is no box to draw. */
  def subgraphToSvg(
      info: StyleResolver.SubgraphInfo,
      nodeMap: Map[String, LayoutNode],
  ): Option[SvgNode] =
    val memberNodes = info.nodeIds.flatMap(nodeMap.get)
    if memberNodes.isEmpty then None
    else
      val minX  = memberNodes.map(n => n.center.x - n.width / 2).min - subgraphPadding
      val minY  = memberNodes.map(n => n.center.y - n.height / 2).min - subgraphPadding - labelHeight
      val maxX  = memberNodes.map(n => n.center.x + n.width / 2).max + subgraphPadding
      val maxY  = memberNodes.map(n => n.center.y + n.height / 2).max + subgraphPadding
      val w     = maxX - minX
      val h     = maxY - minY
      val label = info.label.getOrElse(info.id)
      val rect  = leaf("rect")(
        "class"  -> PaintClass.SubgraphRect.cssName,
        "x"      -> minX.f,
        "y"      -> minY.f,
        "width"  -> w.f,
        "height" -> h.f,
        "rx"     -> "5",
        "ry"     -> "5",
      )
      val labelSvg = textElem("text")(
        "class" -> PaintClass.SubgraphLabel.cssName,
        "x"     -> (minX + 8).f,
        "y"     -> (minY + labelHeight - 4).f,
      )(label)
      Some(
        SvgNode.Element(
          "g",
          List("class" -> WrapperClass.Subgraph.cssName, "id" -> s"subgraph-${info.id}"),
          List(rect, labelSvg),
        )
      )
    end if
  end subgraphToSvg
end SubgraphRenderer
