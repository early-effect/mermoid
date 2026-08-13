package mermoid

import mermoid.css.*

object SvgRenderer:

  /** Assigns each item a 0-based index within its key group, preserving input order. */
  private[mermoid] def indexByKey[A, K](items: List[A])(key: A => K): List[(A, Int)] =
    items
      .foldLeft((Map.empty[K, Int], List.empty[(A, Int)])) { case ((counts, acc), item) =>
        val k = key(item)
        val i = counts.getOrElse(k, 0)
        (counts.updated(k, i + 1), (item, i) :: acc)
      }
      ._2
      .reverse

  private[mermoid] def buildLayoutEdges(edges: List[Edge]): List[LayoutEdge] =
    val pairCounts = edges.groupBy(e => (e.from, e.to)).map((pair, es) => pair -> es.size)
    indexByKey(edges)(e => (e.from, e.to)).map { (e, idx) =>
      LayoutEdge(
        e.from,
        e.to,
        e.style,
        e.label,
        selfLoopIndex = if e.from == e.to then idx else 0,
        alias = e.alias,
        edgeIndex = idx,
        edgeCount = pairCounts((e.from, e.to)),
      )
    }
  end buildLayoutEdges

  private def arrowheadDefs(config: RenderConfig): SvgNode =
    val lc = config.layout
    val w  = lc.arrowSize
    val h  = lc.arrowSize * 0.85
    SvgNode.elem("defs")()(
      SvgNode.elem("marker")(
        "id"           -> PaintClass.Arrowhead.cssName,
        "markerWidth"  -> w.f,
        "markerHeight" -> h.f,
        "refX"         -> w.f,
        "refY"         -> (h / 2).f,
        "orient"       -> "auto",
        "markerUnits"  -> "userSpaceOnUse",
      )(
        SvgNode.leaf("polygon")(
          "class"  -> PaintClass.Arrowhead.cssName,
          "points" -> s"0 0, ${w.f} ${(h / 2).f}, 0 ${h.f}",
        )
      )
    )
  end arrowheadDefs

  /** `None` when the resolved stylesheet renders empty. */
  private def styleBlock(config: RenderConfig, classDefRules: List[css.CssRule]): Option[SvgNode] =
    val base          = RenderConfig.resolvedStylesheet(config)
    val withClassDefs =
      if classDefRules.isEmpty then base
      else base.copy(rules = base.rules ++ classDefRules)
    val css = CssRenderer.render(withClassDefs, config.resolveVariables)
    Option.when(css.nonEmpty)(SvgNode.elem("style")()(SvgNode.Raw(s"\n$css\n")))

  private def svgRoot(width: Double, height: Double, children: List[SvgNode]): SvgNode =
    SvgNode.Element(
      "svg",
      List(
        "xmlns"   -> "http://www.w3.org/2000/svg",
        "width"   -> width.f,
        "height"  -> height.f,
        "viewBox" -> s"0 0 ${width.f} ${height.f}",
      ),
      children,
    )

  def render(
      diagram: Diagram,
      config: RenderConfig = RenderConfig(),
      viewport: Option[Viewport] = None,
  ): String =
    SvgSerializer.render(renderTree(diagram, config, viewport))

  /** The diagram as a tree — the integration point for consumers that build their own markup or UI. */
  def renderTree(
      diagram: Diagram,
      config: RenderConfig = RenderConfig(),
      viewport: Option[Viewport] = None,
  ): SvgNode =
    paint(DiagramLayout.scene(diagram, config, viewport))

  /** Paint a pre-built scene (SVG backend). */
  def paint(scene: DiagramScene): SvgNode =
    val config  = scene.config
    val nodeMap = scene.nodeMap
    val visible = scene.visibleNodes

    val subgraphSvg =
      scene.subgraphs.flatMap(sg => SubgraphRenderer.subgraphToSvg(sg, nodeMap.filter(!_._2.dummy)))

    val edgeSvg = scene.edges
      .filter(e => nodeMap.contains(e.from) && nodeMap.contains(e.to))
      .map(e =>
        EdgeRenderer.edgeToSvg(
          config,
          e,
          nodeMap,
          scene.loopSide,
          scene.routes.getOrElse((e.from, e.to), Nil),
        )
      )

    val nodeSvg = visible.map { n =>
      val interaction = scene.interactions.get(n.id)
      ShapeRenderer.nodeToSvg(
        n,
        config,
        includeLabel = !n.cssClasses.contains(PaintClass.StartEnd.cssName),
        interaction = interaction,
      )
    }

    val selfLoopCounts  = scene.edges.filter(e => e.from == e.to).groupBy(_.from).map((id, es) => id -> es.size)
    val selfLoopExtents = selfLoopCounts.flatMap { case (id, count) =>
      nodeMap.get(id).map(node => id -> NoteRenderer.selfLoopBottomExtent(config, node, count))
    }
    val noteSvg =
      scene.notes.flatMap(note => NoteRenderer.noteToSvg(config, note, nodeMap, selfLoopExtents))

    // Opaque canvas so dark hosts (docs panels, dark mode) do not swallow #333 edges.
    val background = SvgNode.leaf("rect")(
      "class"  -> PaintClass.DiagramBg.cssName,
      "x"      -> "0",
      "y"      -> "0",
      "width"  -> scene.width.f,
      "height" -> scene.height.f,
    )

    svgRoot(
      scene.width,
      scene.height,
      arrowheadDefs(config) :: styleBlock(
        config,
        scene.classDefRules,
      ).toList ++ (background :: subgraphSvg ++ edgeSvg ++ nodeSvg ++ noteSvg),
    )
  end paint
end SvgRenderer
