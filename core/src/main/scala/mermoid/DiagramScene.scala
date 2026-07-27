package mermoid

import mermoid.css.CssRule

/** Available paint area for responsive layout. */
case class Viewport(
    maxWidth: Double,
    maxHeight: Option[Double] = None,
)

/** How [[DiagramLayout]] adapts a diagram to a [[Viewport]]. */
case class ResponsiveConfig(
    /** Compress hSpacing/vSpacing/padding to target the viewport. */
    compressSpacing: Boolean = true,
    /** When a viewport is set, re-orient relative to this width: below → prefer vertical (LR→TB); at/above → prefer
      * horizontal (TB→LR). `None` = keep author direction.
      */
    flipDirectionBelow: Option[Double] = Some(640.0),
    /** After layout, if scene.width > maxWidth, painters may apply uniform scale. */
    scaleToFit: Boolean = true,
    /** Floor for spacing compression so graphs stay readable. */
    minSpacingScale: Double = 0.45,
    /** Cap when expanding spacing to fill a wider viewport. */
    maxSpacingScale: Double = 1.75,
)

/** Mermaid `click` / tooltip / link metadata for a node or state. */
case class NodeInteraction(
    tooltip: Option[String] = None,
    href: Option[String] = None,
    linkTarget: Option[String] = None,
    callbackName: Option[String] = None,
)

/** Paint-ready diagram: geometry, styles, and interactions shared by SVG and ascent painters. */
case class DiagramScene(
    width: Double,
    height: Double,
    nodes: List[LayoutNode],
    edges: List[LayoutEdge],
    routes: Map[(String, String), List[Point]],
    subgraphs: List[StyleResolver.SubgraphInfo],
    notes: List[StateNote],
    interactions: Map[String, NodeInteraction],
    loopSide: SelfLoopSide,
    classDefRules: List[CssRule],
    config: RenderConfig,
    /** Effective direction after optional responsive flip. */
    direction: Direction,
):
  def visibleNodes: List[LayoutNode]          = nodes.filter(!_.dummy)
  def nodeMap: Map[String, LayoutNode]        = nodes.map(n => n.id -> n).toMap
  def visibleNodeMap: Map[String, LayoutNode] = visibleNodes.map(n => n.id -> n).toMap

  /** Uniform scale so the scene fits `maxWidth`, or 1.0 when scale-to-fit is off / not needed. */
  def fitScale(maxWidth: Double): Double =
    if !config.responsive.scaleToFit || width <= maxWidth || width <= 0 then 1.0
    else maxWidth / width
end DiagramScene
