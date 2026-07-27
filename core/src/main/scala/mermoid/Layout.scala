package mermoid

object Layout:

  private[mermoid] def layout(
      config: LayoutConfig,
      direction: Direction,
      nodes: Map[String, NodeDef],
      edges: List[Edge],
  ): LayoutResult =
    val isVertical = direction match
      case Direction.TB | Direction.TD | Direction.BT => true
      case Direction.LR | Direction.RL                => false

    val layoutEdges = edges.filter(e => e.from != e.to)
    val reverseAdj  = layoutEdges.groupBy(_.to).map((k, v) => k -> v.map(_.from).distinct)
    val nodeIds     = nodes.keys.toList
    val layerOf     = longestPathLayers(nodeIds, reverseAdj)

    val maxLayer = layerOf.values.maxOption.getOrElse(0)
    val layers   = (0 to maxLayer)
      .map(l => nodeIds.filter(id => layerOf.getOrElse(id, 0) == l))
      .toList
      .filter(_.nonEmpty)

    val reverseMain   = direction == Direction.BT || direction == Direction.RL
    val orderedLayers = if reverseMain then layers.reverse else layers

    val pairEdges = layoutEdges.map(e => (e.from, e.to))
    val minimized = CrossingMinimizer.orderLayers(orderedLayers, pairEdges, config.barycenterIterations)

    val expanded    = DummyVertices.expand(minimized, edges)
    val allNodeDefs = nodes ++ expanded.dummies
    val dummyIds    = expanded.dummies.keySet
    val finalLayers = CrossingMinimizer.orderLayers(
      expanded.layers,
      pairEdges ++ expanded.routes.toList.flatMap { case ((from, to), dummies) =>
        val chain = from :: dummies ::: List(to)
        chain.zip(chain.tail)
      },
      Math.max(1, config.barycenterIterations / 2),
    )

    val nodeSizes = allNodeDefs.map { case (id, nd) =>
      if dummyIds.contains(id) then id -> (0.0, 0.0)
      else
        val label = nd.label.getOrElse(id)
        id -> SvgUtil.computeNodeSize(label, nd.shape, config)
    }

    val selfEdges     = edges.filter(e => e.from == e.to)
    val selfLoopExtra = selfEdges.groupBy(_.from).map { case (id, selfEs) =>
      val (nodeW, nodeH) = nodeSizes(id)
      val nodeRadius     = Math.max(nodeW, nodeH) / 2
      val loopSize       = nodeRadius * 0.8 + config.selfLoopSize
      val maxLabelW      = selfEs
        .flatMap(_.label)
        .map(l => SvgUtil.estimateTextWidth(l, config) + config.selfLoopLabelPadding)
        .maxOption
        .getOrElse(0.0)
      val labelCount         = selfEs.size
      val stackedLabelHeight = labelCount * (config.edgeLabelFontSize + 12)
      if isVertical then
        val extraRight = loopSize + Math.max(0.0, maxLabelW / 2) + config.selfLoopLabelPadding
        id -> (extraRight, 0.0)
      else
        val extraRight = Math.max(0.0, maxLabelW / 2 - nodeW / 2)
        val extraUp    = loopSize + stackedLabelHeight + config.selfLoopLabelPadding
        id -> (extraRight, extraUp)
    }

    val layerSets = finalLayers.zipWithIndex.flatMap { case (ids, idx) =>
      ids.map(id => id -> idx)
    }.toMap
    val chainEdges = pairEdges ++ expanded.routes.toList.flatMap { case ((from, to), dummies) =>
      val chain = from :: dummies ::: List(to)
      chain.zip(chain.tail)
    }
    val gapSpacing = (0 until finalLayers.size - 1).map { gapIdx =>
      val maxLabelWidth = layoutEdges
        .flatMap { e =>
          val fromLayer  = layerSets.getOrElse(e.from, -1)
          val toLayer    = layerSets.getOrElse(e.to, -1)
          val crossesGap =
            (fromLayer <= gapIdx && toLayer >= gapIdx + 1) ||
              (toLayer <= gapIdx && fromLayer >= gapIdx + 1)
          if crossesGap then e.label.map(l => SvgUtil.estimateTextWidth(l, config) + config.nodePaddingH)
          else None
        }
        .maxOption
        .getOrElse(0.0)
      Math.max(config.hSpacing, maxLabelWidth)
    }.toList

    val placed =
      if isVertical then
        layoutVertical(config, finalLayers, allNodeDefs, nodeSizes, selfLoopExtra, gapSpacing, chainEdges, dummyIds)
      else
        layoutHorizontal(config, finalLayers, allNodeDefs, nodeSizes, selfLoopExtra, gapSpacing, chainEdges, dummyIds)

    val routePoints = expanded.routes.map { case (pair, dummyIdsList) =>
      val centers = dummyIdsList.flatMap(id => placed.find(_.id == id).map(_.center))
      pair -> centers
    }

    LayoutResult(placed, routePoints)
  end layout

  /** Longest-path layering: a node's layer is one past its deepest predecessor.
    *
    * Memoized in an immutable map threaded through the traversal. `onPath` breaks cycles — a node already being
    * resolved contributes nothing to its own depth, so a cyclic graph layers rather than recursing forever.
    */
  private[mermoid] def longestPathLayers(
      nodeIds: List[String],
      reverseAdj: Map[String, List[String]],
  ): Map[String, Int] =
    def visit(id: String, memo: Map[String, Int], onPath: Set[String]): Map[String, Int] =
      if memo.contains(id) || onPath.contains(id) then memo
      else
        val parents  = reverseAdj.getOrElse(id, Nil)
        val resolved = parents.foldLeft(memo)((m, p) => visit(p, m, onPath + id))
        val layer    = parents.flatMap(resolved.get).map(_ + 1).maxOption.getOrElse(0)
        resolved.updated(id, layer)

    nodeIds.foldLeft(Map.empty[String, Int])((memo, id) => visit(id, memo, Set.empty))
  end longestPathLayers

  /** Main-axis gap after layer `layerIdx`; the last layer falls back to the default spacing. */
  private def gapAfter(layerIdx: Int, gapSpacing: List[Double], fallback: Double): Double =
    gapSpacing.lift(layerIdx).getOrElse(fallback)

  private def layoutVertical(
      config: LayoutConfig,
      orderedLayers: List[List[String]],
      nodes: Map[String, NodeDef],
      nodeSizes: Map[String, (Double, Double)],
      selfLoopExtra: Map[String, (Double, Double)],
      gapSpacing: List[Double],
      chainEdges: List[(String, String)],
      dummyIds: Set[String],
  ): List[LayoutNode] =
    val adj                = neighborPositions(chainEdges)
    val (mainPositions, _) =
      layerMainAxis(orderedLayers, nodeSizes, selfLoopExtra, gapSpacing, config, vertical = true)

    val crossPositions = medianCrossPositions(
      orderedLayers,
      nodeSizes,
      selfLoopExtra,
      adj,
      config.hSpacing,
      config.padding,
      vertical = true,
      config.coordinateIterations,
    )

    orderedLayers.zipWithIndex.flatMap { case (layer, layerIdx) =>
      val main = mainPositions(layerIdx)
      layer.map { id =>
        val (w, h)  = nodeSizes(id)
        val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
        val nd      = nodes(id)
        val cross   = crossPositions(id)
        val isDummy = dummyIds.contains(id)
        LayoutNode(
          id,
          nd.label.getOrElse(id),
          nd.shape,
          Point(cross, main + extraUp + (if isDummy then 0.0 else h / 2)),
          w,
          h,
          Map.empty,
          dummy = isDummy,
        )
      }
    }
  end layoutVertical

  private def layoutHorizontal(
      config: LayoutConfig,
      orderedLayers: List[List[String]],
      nodes: Map[String, NodeDef],
      nodeSizes: Map[String, (Double, Double)],
      selfLoopExtra: Map[String, (Double, Double)],
      gapSpacing: List[Double],
      chainEdges: List[(String, String)],
      dummyIds: Set[String],
  ): List[LayoutNode] =
    val adj                = neighborPositions(chainEdges)
    val (mainPositions, _) =
      layerMainAxis(orderedLayers, nodeSizes, selfLoopExtra, gapSpacing, config, vertical = false)

    val crossPositions = medianCrossPositions(
      orderedLayers,
      nodeSizes,
      selfLoopExtra,
      adj,
      config.vSpacing,
      config.padding,
      vertical = false,
      config.coordinateIterations,
    )

    orderedLayers.zipWithIndex.flatMap { case (layer, layerIdx) =>
      val main      = mainPositions(layerIdx)
      val layerMaxW = layer
        .map { id =>
          val (w, _)     = nodeSizes(id)
          val extraRight = selfLoopExtra.get(id).map(_._1).getOrElse(0.0)
          w + extraRight
        }
        .maxOption
        .getOrElse(0.0)
      layer.map { id =>
        val (w, h)  = nodeSizes(id)
        val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
        val nd      = nodes(id)
        val cross   = crossPositions(id)
        val isDummy = dummyIds.contains(id)
        LayoutNode(
          id,
          nd.label.getOrElse(id),
          nd.shape,
          Point(main + layerMaxW / 2, cross + extraUp + (if isDummy then 0.0 else h / 2)),
          w,
          h,
          Map.empty,
          dummy = isDummy,
        )
      }
    }
  end layoutHorizontal

  /** Main-axis offset of each layer's leading edge, and per-layer thickness. */
  private def layerMainAxis(
      orderedLayers: List[List[String]],
      nodeSizes: Map[String, (Double, Double)],
      selfLoopExtra: Map[String, (Double, Double)],
      gapSpacing: List[Double],
      config: LayoutConfig,
      vertical: Boolean,
  ): (List[Double], List[Double]) =
    val fallback = if vertical then config.vSpacing else config.hSpacing
    orderedLayers.zipWithIndex
      .foldLeft((config.padding, List.empty[Double], List.empty[Double])) {
        case ((offset, mains, thicknesses), (layer, layerIdx)) =>
          val thickness =
            if vertical then
              layer
                .map { id =>
                  val (_, h)  = nodeSizes(id)
                  val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
                  h + extraUp
                }
                .maxOption
                .getOrElse(0.0)
            else
              layer
                .map { id =>
                  val (w, _)     = nodeSizes(id)
                  val extraRight = selfLoopExtra.get(id).map(_._1).getOrElse(0.0)
                  w + extraRight
                }
                .maxOption
                .getOrElse(0.0)
          val next = offset + thickness + gapAfter(layerIdx, gapSpacing, fallback)
          (next, mains :+ offset, thicknesses :+ thickness)
      } match
      case (_, mains, thicknesses) => (mains, thicknesses)
    end match
  end layerMainAxis

  private def neighborPositions(edges: List[(String, String)]): Map[String, List[String]] =
    edges.foldLeft(Map.empty[String, List[String]]) { case (acc, (a, b)) =>
      if a == b then acc
      else
        acc
          .updated(a, (b :: acc.getOrElse(a, Nil)).distinct)
          .updated(b, (a :: acc.getOrElse(b, Nil)).distinct)
    }

  /** Cross-axis positions via iterative median-of-neighbors with min-separation packing. */
  private def medianCrossPositions(
      layers: List[List[String]],
      nodeSizes: Map[String, (Double, Double)],
      selfLoopExtra: Map[String, (Double, Double)],
      adj: Map[String, List[String]],
      spacing: Double,
      padding: Double,
      vertical: Boolean,
      iterations: Int,
  ): Map[String, Double] =
    def halfExtent(id: String): Double =
      val (w, h) = nodeSizes(id)
      if vertical then
        val extraRight = selfLoopExtra.get(id).map(_._1).getOrElse(0.0)
        (w + extraRight) / 2
      else
        val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
        (h + extraUp) / 2

    def packLayer(layer: List[String], desired: Map[String, Double]): Map[String, Double] =
      if layer.isEmpty then Map.empty
      else
        val sorted = layer.sortBy(id => desired.getOrElse(id, 0.0))
        // Pack from the origin with min separation, then slide the block so its center matches
        // the mean desired position. Using Math.max(cursor, target) alone walks the whole layer
        // to the right when every node shares a large median (hub / fan layouts).
        val packed = sorted
          .foldLeft((0.0, List.empty[(String, Double, Double)])) { case ((cursor, acc), id) =>
            val half = halfExtent(id)
            val x    = cursor + half
            (x + half + spacing, (id, x, half) :: acc)
          }
          ._2
          .reverse
        val left   = packed.head._2 - packed.head._3
        val right  = packed.last._2 + packed.last._3
        val center = (left + right) / 2
        val want   =
          val ds = sorted.flatMap(id => desired.get(id))
          if ds.isEmpty then center else ds.sum / ds.size
        val shift0 = want - center
        val minX   = packed.map((_, x, half) => x - half + shift0).min
        val shift  = if minX < padding then shift0 + (padding - minX) else shift0
        packed.map((id, x, _) => id -> (x + shift)).toMap
    end packLayer

    def initialPack: Map[String, Double] =
      layers.foldLeft(Map.empty[String, Double]) { (acc, layer) =>
        acc ++ packLayer(layer, layer.map(id => id -> 0.0).toMap)
      }

    def medianOf(id: String, pos: Map[String, Double]): Option[Double] =
      val ns = adj.getOrElse(id, Nil).flatMap(pos.get).sorted
      if ns.isEmpty then None
      else
        val mid = ns.size / 2
        Some(if ns.size % 2 == 1 then ns(mid) else (ns(mid - 1) + ns(mid)) / 2)

    (0 until iterations)
      .foldLeft(initialPack) { (pos, iter) =>
        val forward = iter % 2 == 0
        val order   = if forward then layers.zipWithIndex else layers.zipWithIndex.reverse
        order.foldLeft(pos) { case (current, (layer, _)) =>
          val desired = layer.map { id =>
            id -> medianOf(id, current).getOrElse(current.getOrElse(id, padding))
          }.toMap
          current ++ packLayer(layer, desired)
        }
      }
  end medianCrossPositions
end Layout
