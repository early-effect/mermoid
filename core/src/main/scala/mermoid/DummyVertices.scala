package mermoid

/** Inserts invisible dummy nodes so every edge spans exactly one layer (Sugiyama phase 3 prep). */
object DummyVertices:

  private[mermoid] case class Expanded(
      layers: List[List[String]],
      /** Dummy node stubs keyed by id (no geometry yet). */
      dummies: Map[String, NodeDef],
      /** Intermediate dummy ids for each (from, to), ordered source → target. */
      routes: Map[(String, String), List[String]],
  )

  /** Expand `layers` with dummies for every multi-layer edge in `edges`. */
  private[mermoid] def expand(
      layers: List[List[String]],
      edges: List[Edge],
  ): Expanded =
    val layerOf = layers.zipWithIndex.flatMap { case (ids, i) => ids.map(_ -> i) }.toMap
    val nonSelf = edges.filter(e => e.from != e.to)

    val initial = Expanded(layers, Map.empty, Map.empty)
    nonSelf.foldLeft(initial) { (acc, edge) =>
      val fromL = layerOf.getOrElse(edge.from, 0)
      val toL   = layerOf.getOrElse(edge.to, 0)
      val span  = Math.abs(toL - fromL)
      if span <= 1 then acc
      else
        val step      = if toL > fromL then 1 else -1
        val midLayers = (fromL + step) until toL by step
        val dummyIds  = midLayers.toList.map(l => dummyId(edge.from, edge.to, l))
        val withNodes = dummyIds.zip(midLayers).foldLeft(acc) { case (a, (id, layerIdx)) =>
          if a.dummies.contains(id) then a
          else
            val stub    = NodeDef(id, Some(""), NodeShape.Rect)
            val updated = a.layers.zipWithIndex.map { case (layer, i) =>
              if i == layerIdx && !layer.contains(id) then layer :+ id else layer
            }
            a.copy(layers = updated, dummies = a.dummies.updated(id, stub))
        }
        withNodes.copy(routes = withNodes.routes.updated((edge.from, edge.to), dummyIds))
      end if
    }
  end expand

  private[mermoid] def dummyId(from: String, to: String, layer: Int): String =
    s"__dummy_${from}_${to}_$layer"
end DummyVertices
