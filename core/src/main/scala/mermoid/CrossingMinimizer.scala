package mermoid

/** Barycenter heuristic for within-layer ordering (Sugiyama phase 2). */
object CrossingMinimizer:

  /** Reorder nodes within each layer to reduce edge crossings.
    *
    * Alternating forward/backward sweeps place each node at the median (here: mean barycenter) of its neighbors in the
    * adjacent layer, then break ties by prior order. Runs `iterations` full round-trips.
    */
  private[mermoid] def orderLayers(
      layers: List[List[String]],
      edges: List[(String, String)],
      iterations: Int,
  ): List[List[String]] =
    if layers.size < 2 then layers
    else
      val adj = undirectedAdj(edges)
      (0 until iterations).foldLeft(layers) { (current, _) =>
        val down = sweepForward(current, adj)
        sweepBackward(down, adj)
      }
  end orderLayers

  private def undirectedAdj(edges: List[(String, String)]): Map[String, List[String]] =
    edges
      .filter((a, b) => a != b)
      .foldLeft(Map.empty[String, List[String]]) { case (acc, (a, b)) =>
        acc
          .updated(a, (b :: acc.getOrElse(a, Nil)).distinct)
          .updated(b, (a :: acc.getOrElse(b, Nil)).distinct)
      }

  private def sweepForward(
      layers: List[List[String]],
      adj: Map[String, List[String]],
  ): List[List[String]] =
    layers.zipWithIndex.foldLeft(List.empty[List[String]]) { case (acc, (layer, idx)) =>
      if idx == 0 then acc :+ layer
      else
        val prev = acc.last
        val pos  = prev.zipWithIndex.toMap
        acc :+ sortByBarycenter(layer, pos, adj)
    }

  private def sweepBackward(
      layers: List[List[String]],
      adj: Map[String, List[String]],
  ): List[List[String]] =
    val indexed = layers.zipWithIndex
    indexed.foldRight(List.empty[List[String]]) { case ((layer, idx), acc) =>
      if idx == layers.size - 1 then layer :: acc
      else
        val next = acc.head
        val pos  = next.zipWithIndex.toMap
        sortByBarycenter(layer, pos, adj) :: acc
    }
  end sweepBackward

  /** Stable sort by mean neighbor position; nodes with no neighbors keep relative order via index. */
  private def sortByBarycenter(
      layer: List[String],
      neighborPos: Map[String, Int],
      adj: Map[String, List[String]],
  ): List[String] =
    layer.zipWithIndex
      .map { case (id, idx) =>
        val neighbors = adj.getOrElse(id, Nil).flatMap(neighborPos.get)
        val bary      = if neighbors.isEmpty then idx.toDouble else neighbors.sum.toDouble / neighbors.size
        (id, bary, idx)
      }
      .sortBy { case (_, bary, idx) => (bary, idx) }
      .map(_._1)
  end sortByBarycenter
end CrossingMinimizer
