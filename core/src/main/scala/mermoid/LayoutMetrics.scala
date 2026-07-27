package mermoid

/** Pure geometric metrics over a laid-out graph, used by layout quality tests. */
object LayoutMetrics:

  /** True when two axis-aligned node boxes overlap (strict interior intersection). */
  def nodesOverlap(a: LayoutNode, b: LayoutNode, gap: Double = 0.0): Boolean =
    val dx = Math.abs(a.center.x - b.center.x)
    val dy = Math.abs(a.center.y - b.center.y)
    dx < (a.width + b.width) / 2 + gap && dy < (a.height + b.height) / 2 + gap

  def anyNodeOverlap(nodes: List[LayoutNode], gap: Double = 0.0): Boolean =
    nodes.iterator
      .filter(!_.dummy)
      .toList
      .combinations(2)
      .exists {
        case List(a, b) => nodesOverlap(a, b, gap)
        case _          => false
      }

  /** Count crossings among straight segments between consecutive layers.
    *
    * `pos` maps node id → cross-axis position within its layer. Only edges whose endpoints both appear in
    * `pos` are considered. Two edges (a→b) and (c→d) cross when the relative order of a,c differs from b,d.
    */
  private[mermoid] def countLayerCrossings(
      upper: List[String],
      lower: List[String],
      edges: List[(String, String)],
  ): Int =
    val uPos = upper.zipWithIndex.toMap
    val lPos = lower.zipWithIndex.toMap
    val segs = edges.collect {
      case (a, b) if uPos.contains(a) && lPos.contains(b) => (uPos(a), lPos(b))
      case (a, b) if uPos.contains(b) && lPos.contains(a) => (uPos(b), lPos(a))
    }
    segs.indices.foldLeft(0) { (acc, i) =>
      val (u1, l1) = segs(i)
      acc + segs.drop(i + 1).count { case (u2, l2) => (u1 < u2) != (l1 < l2) }
    }
  end countLayerCrossings

  /** Total crossings across all consecutive layer pairs for the given layering and undirected edge ends. */
  private[mermoid] def totalCrossings(layers: List[List[String]], edges: List[(String, String)]): Int =
    layers
      .sliding(2)
      .collect { case List(upper, lower) => countLayerCrossings(upper, lower, edges) }
      .sum

  /** Geometric edge-edge crossings from laid-out centers (straight center-to-center segments). */
  def edgeCrossings(nodes: List[LayoutNode], edges: List[Edge]): Int =
    val pos = nodes.filter(!_.dummy).map(n => n.id -> n.center).toMap
    val segs = edges
      .filter(e => e.from != e.to)
      .flatMap(e => pos.get(e.from).zip(pos.get(e.to)).map((a, b) => (e.from, e.to, a, b)))
    segs.indices.foldLeft(0) { (acc, i) =>
      val (f1, t1, a, b) = segs(i)
      acc + segs.drop(i + 1).count { case (f2, t2, c, d) =>
        val shareEndpoint = f1 == f2 || f1 == t2 || t1 == f2 || t1 == t2
        !shareEndpoint && segmentsCross(a, b, c, d)
      }
    }
  end edgeCrossings

  private def segmentsCross(p1: Point, p2: Point, p3: Point, p4: Point): Boolean =
    val d1 = cross(p3, p4, p1)
    val d2 = cross(p3, p4, p2)
    val d3 = cross(p1, p2, p3)
    val d4 = cross(p1, p2, p4)
    ((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0))

  private def cross(a: Point, b: Point, c: Point): Double =
    (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
end LayoutMetrics
