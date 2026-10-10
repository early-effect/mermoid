package mermoid

object Layout:

  private[mermoid] def layout(
      config: LayoutConfig,
      direction: Direction,
      nodes: Map[NodeId, NodeDef],
      edges: List[Edge],
      sizes: Map[NodeId, (Double, Double)] = Map.empty,
      measure: Option[TextMeasure] = None,
  ): LayoutResult =
    val usedMeasure = measure.getOrElse(TextMeasure.fromConfig(config))
    val isVertical  = direction match
      case Direction.TB | Direction.TD | Direction.BT => true
      case Direction.LR | Direction.RL                => false

    val layoutEdges = edges.filter(e => e.from != e.to)
    val nodeIds     = nodes.keys.toList
    val forward     = layoutEdges.map(e => (e.from, e.to))
    // Reverse only the feedback set. Drawing, dummies, and barycenter keep the original direction.
    val feedback   = feedbackEdges(nodeIds, forward)
    val ranking    = forward.map { (from, to) => if feedback.contains((from, to)) then (to, from) else (from, to) }
    val reverseAdj = ranking.groupBy(_._2).map((to, es) => to -> es.map(_._1).distinct)
    val layerOf    = longestPathLayers(nodeIds, reverseAdj)

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
        chain.zip(chain.drop(1))
      },
      Math.max(1, config.barycenterIterations / 2),
    )

    val nodeSizes = allNodeDefs.map { case (id, nd) =>
      if dummyIds.contains(id) then id -> (0.0, 0.0)
      else
        sizes.get(id) match
          case Some(fixed) => id -> fixed
          case None        =>
            val label = nd.label.getOrElse(id.value)
            id -> SvgUtil.computeNodeSize(label, nd.shape, config, usedMeasure)
    }

    val selfEdges     = edges.filter(e => e.from == e.to)
    val selfLoopExtra = selfEdges.groupBy(_.from).flatMap { case (id, selfEs) =>
      nodeSizes.get(id).map { (nodeW, nodeH) =>
        val nodeRadius = Math.max(nodeW, nodeH) / 2
        val loopSize   = nodeRadius * 0.8 + config.selfLoopSize
        val maxLabelW  = selfEs
          .flatMap(_.label)
          .map(l => usedMeasure.width(l, config.fontSize.toDouble, config.fontFamily) + config.selfLoopLabelPadding)
          .maxOption
          .getOrElse(0.0)
        val labelCount         = selfEs.size
        val stackedLabelHeight = labelCount * (config.edgeLabelFontSize + 16)
        if isVertical then
          val extraRight = loopSize + Math.max(0.0, maxLabelW / 2) + config.selfLoopLabelPadding
          id -> (extraRight, 0.0)
        else
          val extraRight = Math.max(0.0, maxLabelW / 2 - nodeW / 2)
          val extraUp    = loopSize + stackedLabelHeight + config.selfLoopLabelPadding
          id -> (extraRight, extraUp)
      }
    }

    val layerSets = finalLayers.zipWithIndex.flatMap { case (ids, idx) =>
      ids.map(id => id -> idx)
    }.toMap
    val chainEdges = pairEdges ++ expanded.routes.toList.flatMap { case ((from, to), dummies) =>
      val chain = from :: dummies ::: List(to)
      chain.zip(chain.drop(1))
    }
    val gapSpacing = (0 until finalLayers.size - 1).map { gapIdx =>
      val maxLabelWidth = layoutEdges
        .flatMap { e =>
          val fromLayer  = layerSets.getOrElse(e.from, -1)
          val toLayer    = layerSets.getOrElse(e.to, -1)
          val crossesGap =
            (fromLayer <= gapIdx && toLayer >= gapIdx + 1) ||
              (toLayer <= gapIdx && fromLayer >= gapIdx + 1)
          if crossesGap then
            e.label.map(l => usedMeasure.width(l, config.fontSize.toDouble, config.fontFamily) + config.nodePaddingH)
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

  /** Edges to reverse so [[longestPathLayers]] sees a DAG.
    *
    * Depth-first search from the sources, following edges in the order they were written. An edge into a node still on
    * the stack is a back edge. An edge into a finished node is a back edge when that node can still reach the source,
    * which is how a restart (`Stopped --> JourneyRepublishing`) is told from a second forward parent (`Live -->
    * Archived`). A two-cycle keeps the first-written direction. Self-loops are ignored.
    *
    * A left/right degree peel is the wrong greedy choice here: it can reverse a forward pipeline edge that happens to
    * point at a node the peel placed early, and the fault leaves rank as roots again.
    */
  private[mermoid] def feedbackEdges(nodeIds: List[NodeId], edges: List[(NodeId, NodeId)]): Set[(NodeId, NodeId)] =
    val known = nodeIds.toSet
    val uniq  = edges.filter((a, b) => a != b && known.contains(a) && known.contains(b)).distinct
    if uniq.isEmpty || nodeIds.isEmpty then Set.empty
    else
      val succ = uniq.groupBy(_._1).map((from, es) => from -> es.map(_._2))

      def reaches(from: NodeId, to: NodeId): Boolean =
        if from == to then true
        else
          @annotation.tailrec
          def bfs(queue: List[NodeId], seen: Set[NodeId]): Boolean =
            queue match
              case Nil          => false
              case head :: tail =>
                if seen.contains(head) then bfs(tail, seen)
                else
                  val next = succ.getOrElse(head, Nil)
                  if next.contains(to) then true
                  else bfs(tail ++ next, seen + head)
          bfs(List(from), Set.empty)

      // 0 white, 1 on the stack, 2 finished. Local to this search.
      val color    = scala.collection.mutable.Map.empty[NodeId, Int]
      val feedback = scala.collection.mutable.Set.empty[(NodeId, NodeId)]
      nodeIds.foreach(id => color(id) = 0)

      def dfs(id: NodeId): Unit =
        color(id) = 1
        succ.getOrElse(id, Nil).foreach { next =>
          color.getOrElse(next, 0) match
            case 0 => dfs(next)
            case 1 => feedback += ((id, next))
            case _ =>
              if reaches(next, id) then feedback += ((id, next))
        }
        color(id) = 2
      end dfs

      val indeg = uniq.groupBy(_._2).map((to, es) => to -> es.size)
      nodeIds.foreach { id =>
        if indeg.getOrElse(id, 0) == 0 && color.getOrElse(id, 0) == 0 then dfs(id)
      }
      uniq.foreach { (from, to) =>
        if color.getOrElse(from, 0) == 0 then dfs(from)
        if color.getOrElse(to, 0) == 0 then dfs(to)
      }
      feedback.toSet
    end if
  end feedbackEdges

  /** Longest-path layering: a node's layer is one past its deepest predecessor.
    *
    * Memoized in an immutable map threaded through the traversal. `onPath` breaks cycles — a node already being
    * resolved contributes nothing to its own depth, so a cyclic graph layers rather than recursing forever.
    */
  private[mermoid] def longestPathLayers(
      nodeIds: List[NodeId],
      reverseAdj: Map[NodeId, List[NodeId]],
  ): Map[NodeId, Int] =
    def visit(id: NodeId, memo: Map[NodeId, Int], onPath: Set[NodeId]): Map[NodeId, Int] =
      if memo.contains(id) || onPath.contains(id) then memo
      else
        val parents  = reverseAdj.getOrElse(id, Nil)
        val resolved = parents.foldLeft(memo)((m, p) => visit(p, m, onPath + id))
        val layer    = parents.flatMap(resolved.get).map(_ + 1).maxOption.getOrElse(0)
        resolved.updated(id, layer)

    nodeIds.foldLeft(Map.empty[NodeId, Int])((memo, id) => visit(id, memo, Set.empty))
  end longestPathLayers

  /** Main-axis gap after layer `layerIdx`; the last layer falls back to the default spacing. */
  private def gapAfter(layerIdx: Int, gapSpacing: List[Double], fallback: Double): Double =
    gapSpacing.lift(layerIdx).getOrElse(fallback)

  private def layoutVertical(
      config: LayoutConfig,
      orderedLayers: List[List[NodeId]],
      nodes: Map[NodeId, NodeDef],
      nodeSizes: Map[NodeId, (Double, Double)],
      selfLoopExtra: Map[NodeId, (Double, Double)],
      gapSpacing: List[Double],
      chainEdges: List[(NodeId, NodeId)],
      dummyIds: Set[NodeId],
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

    orderedLayers.zip(mainPositions).flatMap { (layer, main) =>
      for
        id     <- layer
        (w, h) <- nodeSizes.get(id)
        nd     <- nodes.get(id)
        cross  <- crossPositions.get(id)
      yield
        val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
        val isDummy = dummyIds.contains(id)
        LayoutNode(
          id,
          nd.label.getOrElse(id.value),
          nd.shape,
          Point(cross, main + extraUp + (if isDummy then 0.0 else h / 2)),
          w,
          h,
          Map.empty,
          dummy = isDummy,
        )
      end for
    }
  end layoutVertical

  private def layoutHorizontal(
      config: LayoutConfig,
      orderedLayers: List[List[NodeId]],
      nodes: Map[NodeId, NodeDef],
      nodeSizes: Map[NodeId, (Double, Double)],
      selfLoopExtra: Map[NodeId, (Double, Double)],
      gapSpacing: List[Double],
      chainEdges: List[(NodeId, NodeId)],
      dummyIds: Set[NodeId],
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

    orderedLayers.zip(mainPositions).flatMap { (layer, main) =>
      val layerMaxW = layer
        .flatMap { id =>
          nodeSizes.get(id).map((w, _) => w + selfLoopExtra.get(id).map(_._1).getOrElse(0.0))
        }
        .maxOption
        .getOrElse(0.0)
      for
        id     <- layer
        (w, h) <- nodeSizes.get(id)
        nd     <- nodes.get(id)
        cross  <- crossPositions.get(id)
      yield
        val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
        val isDummy = dummyIds.contains(id)
        LayoutNode(
          id,
          nd.label.getOrElse(id.value),
          nd.shape,
          Point(main + layerMaxW / 2, cross + extraUp + (if isDummy then 0.0 else h / 2)),
          w,
          h,
          Map.empty,
          dummy = isDummy,
        )
      end for
    }
  end layoutHorizontal

  /** Main-axis offset of each layer's leading edge, and per-layer thickness. */
  private def layerMainAxis(
      orderedLayers: List[List[NodeId]],
      nodeSizes: Map[NodeId, (Double, Double)],
      selfLoopExtra: Map[NodeId, (Double, Double)],
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
                .flatMap { id =>
                  nodeSizes.get(id).map((_, h) => h + selfLoopExtra.get(id).map(_._2).getOrElse(0.0))
                }
                .maxOption
                .getOrElse(0.0)
            else
              layer
                .flatMap { id =>
                  nodeSizes.get(id).map((w, _) => w + selfLoopExtra.get(id).map(_._1).getOrElse(0.0))
                }
                .maxOption
                .getOrElse(0.0)
          val next = offset + thickness + gapAfter(layerIdx, gapSpacing, fallback)
          (next, mains :+ offset, thicknesses :+ thickness)
      } match
      case (_, mains, thicknesses) => (mains, thicknesses)
    end match
  end layerMainAxis

  private def neighborPositions(edges: List[(NodeId, NodeId)]): Map[NodeId, List[NodeId]] =
    edges.foldLeft(Map.empty[NodeId, List[NodeId]]) { case (acc, (a, b)) =>
      if a == b then acc
      else
        acc
          .updated(a, (b :: acc.getOrElse(a, Nil)).distinct)
          .updated(b, (a :: acc.getOrElse(b, Nil)).distinct)
    }

  /** Cross-axis positions via iterative median-of-neighbors with min-separation packing. */
  private def medianCrossPositions(
      layers: List[List[NodeId]],
      nodeSizes: Map[NodeId, (Double, Double)],
      selfLoopExtra: Map[NodeId, (Double, Double)],
      adj: Map[NodeId, List[NodeId]],
      spacing: Double,
      padding: Double,
      vertical: Boolean,
      iterations: Int,
  ): Map[NodeId, Double] =
    def halfExtent(id: NodeId): Double =
      val (w, h) = nodeSizes.getOrElse(id, (0.0, 0.0))
      if vertical then
        val extraRight = selfLoopExtra.get(id).map(_._1).getOrElse(0.0)
        (w + extraRight) / 2
      else
        val extraUp = selfLoopExtra.get(id).map(_._2).getOrElse(0.0)
        (h + extraUp) / 2

    def packLayer(layer: List[NodeId], desired: Map[NodeId, Double]): Map[NodeId, Double] =
      if layer.isEmpty then Map.empty
      else
        val sorted = layer.sortBy(id => desired.getOrElse(id, 0.0))
        // Pack from the origin with min separation, then slide the block so its center matches
        // the mean desired position. Using Math.max(cursor, target) alone walks the whole layer
        // to the right when every node shares a large median (hub / fan layouts).
        val packed = sorted
          .foldLeft((0.0, List.empty[(NodeId, Double, Double)])) { case ((cursor, acc), id) =>
            val half = halfExtent(id)
            val x    = cursor + half
            (x + half + spacing, (id, x, half) :: acc)
          }
          ._2
          .reverse
        packed match
          case Nil                         => Map.empty
          case (_, firstX, firstHalf) :: _ =>
            val (lastX, lastHalf) = packed.lastOption.fold((firstX, firstHalf))((_, x, half) => (x, half))
            val left              = firstX - firstHalf
            val right             = lastX + lastHalf
            val center            = (left + right) / 2
            val want              =
              val ds = sorted.flatMap(id => desired.get(id))
              if ds.isEmpty then center else ds.sum / ds.size
            // Padding is a margin around the whole scene, applied once after alignment.
            // Clamping each rank here pins a wide node to the left edge and shoves it off the column.
            val shift = want - center
            packed.map((id, x, _) => id -> (x + shift)).toMap
        end match
    end packLayer

    def initialPack: Map[NodeId, Double] =
      layers.foldLeft(Map.empty[NodeId, Double]) { (acc, layer) =>
        acc ++ packLayer(layer, layer.map(id => id -> 0.0).toMap)
      }

    def medianOf(id: NodeId, pos: Map[NodeId, Double]): Option[Double] =
      val ns  = adj.getOrElse(id, Nil).flatMap(pos.get).sorted
      val mid = ns.size / 2
      if ns.size % 2 == 1 then ns.lift(mid)
      else ns.lift(mid - 1).zip(ns.lift(mid)).map((a, b) => (a + b) / 2)

    val aligned = (0 until iterations)
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
    val minLeft = aligned.map((id, x) => x - halfExtent(id)).minOption.getOrElse(0.0)
    val margin  = padding - minLeft
    if margin == 0.0 then aligned else aligned.map((id, x) => id -> (x + margin))
  end medianCrossPositions
end Layout
