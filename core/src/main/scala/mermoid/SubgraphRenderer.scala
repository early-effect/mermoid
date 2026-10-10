package mermoid

import mermoid.SvgNode.{leaf, textElem}
import mermoid.css.{PaintClass, WrapperClass}

object SubgraphRenderer:

  private val labelHeight = 20.0

  def subgraphToSvg(frame: PlacedFrame): SvgNode =
    val label = frame.label.getOrElse(frame.id)
    val rect  = leaf("rect")(
      "class"  -> PaintClass.SubgraphRect.cssName,
      "x"      -> frame.rect.x.f,
      "y"      -> frame.rect.y.f,
      "width"  -> frame.rect.w.f,
      "height" -> frame.rect.h.f,
      "rx"     -> "5",
      "ry"     -> "5",
    )
    val labelSvg = textElem("text")(
      "class" -> PaintClass.SubgraphLabel.cssName,
      "x"     -> (frame.rect.x + 8).f,
      "y"     -> (frame.rect.y + labelHeight - 4).f,
    )(label)
    val dividers = frame.dividers.map { y =>
      leaf("line")(
        "class"            -> PaintClass.FragmentDivider.cssName,
        "x1"               -> (frame.rect.x + 8).f,
        "y1"               -> y.f,
        "x2"               -> (frame.rect.x + frame.rect.w - 8).f,
        "y2"               -> y.f,
        "stroke-dasharray" -> "4 4",
      )
    }
    SvgNode.Element(
      "g",
      List("class" -> WrapperClass.Subgraph.cssName, "id" -> s"subgraph-${frame.id}"),
      rect :: labelSvg :: dividers,
    )
  end subgraphToSvg
end SubgraphRenderer
