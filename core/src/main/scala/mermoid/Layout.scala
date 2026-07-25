package mermoid

object Layout:

  private[mermoid] def layout(
      config: LayoutConfig,
      direction: Direction,
      nodes: Map[String, NodeDef],
      edges: List[Edge],
  ): List[LayoutNode] =
    val nodeIds = nodes.keys.toList

    val nodeSizes = nodes.map { case (id, nd) =>
      val label = nd.label.getOrElse(id)
      id -> SvgUtil.computeNodeSize(label, nd.shape, config)
    }

    val isVertical = direction match
      case Direction.TB | Direction.TD | Direction.BT => true
      case Direction.LR | Direction.RL                => false

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

    val layoutEdges = edges.filter(e => e.from != e.to)
    val reverseAdj  = layoutEdges.groupBy(_.to).map((k, v) => k -> v.map(_.from).distinct)

    val layerOf = longestPathLayers(nodeIds, reverseAdj)

    val maxLayer = layerOf.values.maxOption.getOrElse(0)
    val layers   = (0 to maxLayer)
      .map { l =>
        nodeIds.filter(id => layerOf.getOrElse(id, 0) == l)
      }
      .toList
      .filter(_.nonEmpty)

    val reverseMain   = direction == Direction.BT || direction == Direction.RL
    val orderedLayers = if reverseMain then layers.reverse else layers

    val layerSets = orderedLayers.zipWithIndex.flatMap { case (ids, idx) =>
      ids.map(id => id -> idx)
    }.toMap
    val gapSpacing = (0 until orderedLayers.size - 1).map { gapIdx =>
      val maxLabelWidth = layoutEdges
        .flatMap { e =>
          val fromLayer  = layerSets.getOrElse(e.from, -1)
          val toLayer    = layerSets.getOrElse(e.to, -1)
          val crossesGap = (fromLayer == gapIdx && toLayer == gapIdx + 1) ||
            (toLayer == gapIdx && fromLayer == gapIdx + 1)
          if crossesGap then e.label.map(l => SvgUtil.estimateTextWidth(l, config) + config.nodePaddingH)
          else None
        }
        .maxOption
        .getOrElse(0.0)
      Math.max(config.hSpacing, maxLabelWidth)
    }.toList

    if isVertical then layoutVertical(config, orderedLayers, nodes, nodeSizes, selfLoopExtra, gapSpacing)
    else layoutHorizontal(config, orderedLayers, nodes, nodeSizes, selfLoopExtra, gapSpacing)
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

  /** Places a layer's nodes along the cross axis. `place` returns the node and how far to advance for the next one. */
  private def placeLayer(
      layer: List[String],
      config: LayoutConfig,
      place: (String, Double) => (LayoutNode, Double),
  ): List[LayoutNode] =
    layer
      .foldLeft((config.padding, List.empty[LayoutNode])) { case ((crossOffset, acc), id) =>
        val (node, advance) = place(id, crossOffset)
        (crossOffset + advance, node :: acc)
      }
      ._2
      .reverse

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
  ): List[LayoutNode] =
    orderedLayers.zipWithIndex
      .foldLeft((config.padding, List.empty[List[LayoutNode]])) { case ((mainOffset, acc), (layer, layerIdx)) =>
        val layerMaxH = layer.map { id =>
          val (_, h)  = nodeSizes(id)
          val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
          h + extraUp
        }.max
        val placed = placeLayer(
          layer,
          config,
          (id, crossOffset) =>
            val (w, h)     = nodeSizes(id)
            val extraRight = selfLoopExtra.get(id).map(_._1).getOrElse(0.0)
            val extraUp    = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
            val nd         = nodes(id)
            val node       = LayoutNode(
              id,
              nd.label.getOrElse(id),
              nd.shape,
              Point(crossOffset + w / 2, mainOffset + extraUp + h / 2),
              w,
              h,
              Map.empty,
            )
            (node, w + extraRight + config.hSpacing),
        )
        (mainOffset + layerMaxH + gapAfter(layerIdx, gapSpacing, config.vSpacing), placed :: acc)
      }
      ._2
      .reverse
      .flatten

  private def layoutHorizontal(
      config: LayoutConfig,
      orderedLayers: List[List[String]],
      nodes: Map[String, NodeDef],
      nodeSizes: Map[String, (Double, Double)],
      selfLoopExtra: Map[String, (Double, Double)],
      gapSpacing: List[Double],
  ): List[LayoutNode] =
    orderedLayers.zipWithIndex
      .foldLeft((config.padding, List.empty[List[LayoutNode]])) { case ((mainOffset, acc), (layer, layerIdx)) =>
        val layerMaxW = layer.map { id =>
          val (w, _)     = nodeSizes(id)
          val extraRight = selfLoopExtra.get(id).map(_._1).getOrElse(0.0)
          w + extraRight
        }.max
        val placed = placeLayer(
          layer,
          config,
          (id, crossOffset) =>
            val (w, h)  = nodeSizes(id)
            val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
            val nd      = nodes(id)
            val node    = LayoutNode(
              id,
              nd.label.getOrElse(id),
              nd.shape,
              Point(mainOffset + layerMaxW / 2, crossOffset + extraUp + h / 2),
              w,
              h,
              Map.empty,
            )
            (node, h + extraUp + config.vSpacing),
        )
        (mainOffset + layerMaxW + gapAfter(layerIdx, gapSpacing, config.hSpacing), placed :: acc)
      }
      ._2
      .reverse
      .flatten
end Layout
