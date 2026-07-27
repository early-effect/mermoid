package mermoid

/** Result of laying out a diagram: placed nodes (including invisible dummies) and per-edge routes. */
case class LayoutResult(
    nodes: List[LayoutNode],
    /** Intermediate waypoint centers for each (from, to) pair, ordered from source toward target. */
    routes: Map[(String, String), List[Point]],
):
  def visibleNodes: List[LayoutNode]               = nodes.filter(!_.dummy)
  def nodeMap: Map[String, LayoutNode]             = nodes.map(n => n.id -> n).toMap
  def visibleNodeMap: Map[String, LayoutNode]      = visibleNodes.map(n => n.id -> n).toMap
end LayoutResult
