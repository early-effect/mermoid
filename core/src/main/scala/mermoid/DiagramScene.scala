package mermoid

import mermoid.css.CssRule

/** Available paint area for responsive layout. */
case class Viewport(
    maxWidth: Double,
    maxHeight: Option[Double] = None,
)

/** How a painter fits a scene that is wider than its container. */
enum ContainerFit:
  /** Scene stays at scale 1. The parent scrolls. */
  case Off

  /** Scale down to the container width, never above 1, never below `floor`.
    *
    * `floor` is in (0, 1]. Below the floor the fit root scrolls horizontally.
    */
  case ToWidth(floor: Double)

object ContainerFit:
  /** A 16px label stays at least 8px, and a scene up to twice the column fits without a scrollbar. */
  val defaultFloor: Double  = 0.5
  val default: ContainerFit = ContainerFit.ToWidth(defaultFloor)

/** How [[DiagramLayout]] adapts a diagram to a [[Viewport]]. */
case class ResponsiveConfig(
    /** Compress hSpacing/vSpacing/padding to target the viewport. */
    compressSpacing: Boolean = true,
    /** Opt in to a direction flip. `None` keeps the authored direction. `Some(px)`: below prefers vertical (LR to TB),
      * at or above prefers horizontal (TB to LR).
      */
    flipDirectionBelow: Option[Double] = None,
    /** Container fit for painters. The default scales down to the column and stops at [[ContainerFit.defaultFloor]]. */
    fit: ContainerFit = ContainerFit.default,
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
end DiagramScene
