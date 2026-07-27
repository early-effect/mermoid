package mermoid

/** Builds a paint-ready [[DiagramScene]] from a parsed [[Diagram]]. */
object DiagramLayout:

  def scene(
      diagram: Diagram,
      config: RenderConfig = RenderConfig(),
      viewport: Option[Viewport] = None,
  ): DiagramScene =
    diagram match
      case Diagram.Flowchart(dir, stmts) => flowchartScene(dir, stmts, config, viewport)
      case Diagram.StateDiagram(stmts)   => stateScene(stmts, config, viewport)

  private[mermoid] def effectiveDirection(
      author: Direction,
      responsive: ResponsiveConfig,
      viewport: Option[Viewport],
  ): Direction =
    (viewport, responsive.flipDirectionBelow) match
      case (Some(vp), Some(threshold)) =>
        if vp.maxWidth < threshold then preferVertical(author) else preferHorizontal(author)
      case _ => author

  /** Narrow viewports: stack top-to-bottom so content gets height. */
  private def preferVertical(dir: Direction): Direction = dir match
    case Direction.LR => Direction.TB
    case Direction.RL => Direction.BT
    case other        => other

  /** Wide viewports: use horizontal run so content gets width. */
  private def preferHorizontal(dir: Direction): Direction = dir match
    case Direction.TB | Direction.TD => Direction.LR
    case Direction.BT                => Direction.RL
    case other                       => other

  private[mermoid] def compressLayout(
      layout: LayoutConfig,
      responsive: ResponsiveConfig,
      viewport: Option[Viewport],
      nodeCount: Int,
      direction: Direction,
  ): LayoutConfig =
    if !responsive.compressSpacing then return layout
    viewport match
      case None     => layout
      case Some(vp) =>
        val isVertical = direction match
          case Direction.TB | Direction.TD | Direction.BT => true
          case Direction.LR | Direction.RL                => false
        // Naive estimate: nodes in a chain along the main axis with default spacing.
        val along   = Math.max(1, nodeCount)
        val rawSpan =
          if isVertical then along * layout.nodeHeight + (along - 1) * layout.vSpacing + 2 * layout.padding
          else along * layout.minNodeWidth + (along - 1) * layout.hSpacing + 2 * layout.padding
        val target =
          if isVertical then vp.maxHeight.getOrElse(vp.maxWidth * 1.5)
          else vp.maxWidth
        val scale =
          if rawSpan <= 0 then 1.0
          else
            val raw = target / rawSpan
            Math.max(responsive.minSpacingScale, Math.min(responsive.maxSpacingScale, raw))
        if Math.abs(scale - 1.0) < 0.04 then layout
        else
          layout.copy(
            hSpacing = layout.hSpacing * scale,
            vSpacing = layout.vSpacing * scale,
            padding = Math.max(12.0, layout.padding * scale),
            parallelEdgeSpacing = layout.parallelEdgeSpacing * scale,
          )
    end match
  end compressLayout

  private def flowchartScene(
      authorDir: Direction,
      stmts: List[FlowStatement],
      config: RenderConfig,
      viewport: Option[Viewport],
  ): DiagramScene =
    val dir           = effectiveDirection(authorDir, config.responsive, viewport)
    val nodeDefs      = StyleResolver.collectNodes(stmts)
    val edges         = StyleResolver.collectEdges(stmts)
    val nodeClasses   = StyleResolver.collectNodeClasses(stmts)
    val inlineStyles  = StyleResolver.collectInlineStyles(stmts)
    val classDefRules = StyleResolver.classDefsToRules(stmts)
    val interactions  = StyleResolver.collectInteractions(stmts)
    val subgraphs     = StyleResolver.collectSubgraphs(stmts)
    val lc            = compressLayout(config.layout, config.responsive, viewport, nodeDefs.size, dir)
    val cfg           = config.copy(layout = lc)
    val laid          = Layout.layout(lc, dir, nodeDefs, edges)
    val layoutNodes   = laid.nodes.map { n =>
      n.copy(
        cssClasses = nodeClasses.getOrElse(n.id, Nil),
        styles = inlineStyles.getOrElse(n.id, Map.empty),
      )
    }
    val layoutEdges = SvgRenderer.buildLayoutEdges(edges)
    val visible     = layoutNodes.filter(!_.dummy)
    val routes      = laid.routes

    val maxX         = visible.map(n => n.center.x + n.width / 2).maxOption.getOrElse(0.0) + lc.padding
    val maxY         = visible.map(n => n.center.y + n.height / 2).maxOption.getOrElse(0.0) + lc.padding
    val routeExtentX = routes.values.flatten.map(_.x).maxOption.getOrElse(0.0)
    val routeExtentY = routes.values.flatten.map(_.y).maxOption.getOrElse(0.0)
    val parallelPad  = layoutEdges
      .map(e => if e.edgeCount > 1 then lc.parallelEdgeSpacing * e.edgeCount else 0.0)
      .maxOption
      .getOrElse(0.0)
    val labelPad = if layoutEdges.exists(_.label.nonEmpty) then lc.edgeLabelFontSize + 24.0 else 0.0
    val canvasW  = Math.max(maxX, routeExtentX + lc.padding) + parallelPad + labelPad
    val canvasH  = Math.max(maxY, routeExtentY + lc.padding) + parallelPad + labelPad

    val loopSide = dir match
      case Direction.TB | Direction.TD | Direction.BT => SelfLoopSide.Right
      case Direction.LR | Direction.RL                => SelfLoopSide.Top

    DiagramScene(
      width = canvasW,
      height = canvasH,
      nodes = layoutNodes,
      edges = layoutEdges,
      routes = routes,
      subgraphs = subgraphs,
      notes = Nil,
      interactions = interactions,
      loopSide = loopSide,
      classDefRules = classDefRules,
      config = cfg,
      direction = dir,
    )
  end flowchartScene

  private def stateScene(
      stmts: List[StateStatement],
      config: RenderConfig,
      viewport: Option[Viewport],
  ): DiagramScene =
    val authorDir   = Direction.TB
    val dir         = effectiveDirection(authorDir, config.responsive, viewport)
    val transitions = stmts.collect { case StateStatement.TransitionSt(t) => t }
    val stateStyles = stmts.collect { case StateStatement.StyleSt(id, style) => id -> style }.toMap
    val noteStmts   = stmts.collect { case n: StateStatement.NoteSt => n }
    val notes       = SvgRenderer.indexByKey(noteStmts)(_.stateId).map { (n, idx) =>
      val align = stateStyles.get(n.stateId).flatMap(_.noteAlign).getOrElse(NoteTextAlign.Left)
      StateNote(n.position, n.stateId, n.text, align, n.alias, idx)
    }

    val stateIds = (transitions.map(_.from) ++ transitions.map(_.to)).distinct
    val nodeDefs = stateIds.map { id =>
      if id == "[*]" then id -> NodeDef(id, Some(""), NodeShape.Circle)
      else id                -> NodeDef(id, Some(id), NodeShape.Round)
    }.toMap

    val edges       = transitions.map(t => Edge(t.from, t.to, EdgeStyle.Arrow, t.label))
    val lc          = compressLayout(config.layout, config.responsive, viewport, nodeDefs.size, dir)
    val cfg         = config.copy(layout = lc)
    val laid        = Layout.layout(lc, dir, nodeDefs, edges)
    val layoutNodes = laid.nodes.map { n =>
      if n.id == "[*]" then n.copy(width = 16, height = 16, cssClasses = List("start-end"))
      else n
    }
    val routes      = laid.routes
    val layoutEdges = SvgRenderer.buildLayoutEdges(edges)
    val nodeMap     = layoutNodes.map(n => n.id -> n).toMap
    val visible     = layoutNodes.filter(!_.dummy)

    val selfLoopCounts    = edges.filter(e => e.from == e.to).groupBy(_.from).map((id, es) => id -> es.size)
    val bbSelfLoopExtents = selfLoopCounts.flatMap { case (id, count) =>
      nodeMap.get(id).map(node => id -> NoteRenderer.selfLoopBottomExtent(cfg, node, count))
    }

    val noteBoxes = notes.flatMap { note =>
      nodeMap.get(note.stateId).map(node => NoteRenderer.placeNote(cfg, note, node, visible, bbSelfLoopExtents))
    }

    val noteMinX = noteBoxes.map(_.x).minOption.getOrElse(lc.padding)
    val noteMinY = noteBoxes.map(_.y).minOption.getOrElse(lc.padding)
    val shiftX   = if noteMinX < lc.padding then lc.padding - noteMinX else 0.0
    val shiftY   = if noteMinY < lc.padding then lc.padding - noteMinY else 0.0

    val maxX = Math.max(
      visible.map(n => n.center.x + n.width / 2).maxOption.getOrElse(0.0) + shiftX,
      noteBoxes.map(_.right).maxOption.getOrElse(0.0) + shiftX,
    ) + lc.padding
    val maxY = Math.max(
      visible.map(n => n.center.y + n.height / 2).maxOption.getOrElse(0.0) + shiftY,
      noteBoxes.map(_.bottom).maxOption.getOrElse(0.0) + shiftY,
    ) + lc.padding

    val shiftedNodes =
      if shiftX == 0 && shiftY == 0 then layoutNodes
      else layoutNodes.map(n => n.copy(center = Point(n.center.x + shiftX, n.center.y + shiftY)))
    val shiftedRoutes =
      if shiftX == 0 && shiftY == 0 then routes
      else routes.map((k, pts) => k -> pts.map(p => Point(p.x + shiftX, p.y + shiftY)))

    val loopSide = dir match
      case Direction.TB | Direction.TD | Direction.BT => SelfLoopSide.Right
      case Direction.LR | Direction.RL                => SelfLoopSide.Top

    DiagramScene(
      width = maxX,
      height = maxY,
      nodes = shiftedNodes,
      edges = layoutEdges,
      routes = shiftedRoutes,
      subgraphs = Nil,
      notes = notes,
      interactions = Map.empty,
      loopSide = loopSide,
      classDefRules = Nil,
      config = cfg,
      direction = dir,
    )
  end stateScene
end DiagramLayout
