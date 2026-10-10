package mermoid

import mermoid.css.PaintClass

/** Builds a paint-ready [[Scene]] from a parsed [[Diagram]]. Sequence diagrams are a timeline, not a ranked graph. */
object DiagramLayout:

  def scene(
      diagram: Diagram,
      config: RenderConfig = RenderConfig(),
      viewport: Option[Viewport] = None,
  ): Scene =
    diagram match
      case Diagram.Flowchart(dir, raw) =>
        val stmts = LinkStyle.decorateFlow(raw)
        val scene =
          if FlowLayout.containsSubgraph(stmts) then FlowLayout.scene(dir, stmts, config, viewport)
          else flowchartScene(dir, stmts, config, viewport)
        val (title, descr) = LinkStyle.flowAccess(stmts)
        Scene.Ranked(scene.copy(accTitle = title, accDescr = descr))
      case Diagram.StateDiagram(dir, raw) =>
        val stmts = LinkStyle.decorateState(raw)
        val scene = StateModel.resolve(Diagram.StateDiagram(dir, stmts)) match
          case Right(machine) if !StateModel.isLegacy(machine) =>
            StateLayout.scene(machine, config, viewport)
          case Right(machine) =>
            stateScene(dir, stmts, config, viewport).copy(accTitle = machine.accTitle, accDescr = machine.accDescr)
          case Left(_) =>
            stateScene(dir, stmts, config, viewport)
        Scene.Ranked(scene)
      case Diagram.Sequence(stmts)       => Scene.Sequence(SequenceLayout.place(stmts, config, viewport))
      case diagram: Diagram.ClassDiagram =>
        val scene = ClassModel.resolve(diagram) match
          case Right(resolved) => ClassModel.scene(resolved, config, viewport)
          case Left(_)         =>
            ClassModel.scene(ResolvedClass(Direction.TB, Nil, Nil, Nil, false, Nil, None, None), config, viewport)
        Scene.Ranked(scene)
      case diagram: Diagram.ErDiagram =>
        val scene = ErModel.resolve(diagram) match
          case Right(resolved) => ErModel.scene(resolved, config, viewport)
          case Left(_)         =>
            ErModel.scene(ErModel.ResolvedEr(Nil, Nil, Map.empty, Map.empty, Nil, None, None), config, viewport)
        Scene.Ranked(scene)

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
    if !responsive.compressSpacing then layout
    else
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
    val lc            = compressLayout(config.layout, config.responsive, viewport, nodeDefs.size, dir)
    val cfg           = config.copy(layout = lc)
    val laid          = Layout.layout(lc, dir, nodeDefs, edges, measure = Some(TextMeasure.resolve(cfg)))
    val layoutNodes   = laid.nodes.map { n =>
      n.copy(
        cssClasses = nodeClasses.getOrElse(n.id, Nil),
        styles = inlineStyles.getOrElse(n.id, Map.empty),
      )
    }
    val layoutEdges = SvgRenderer.buildLayoutEdges(edges)
    val visible     = layoutNodes.filter(!_.dummy)
    val routes      = laid.routes
    val nodeMap     = layoutNodes.map(n => n.id -> n).toMap

    val loopSide = dir match
      case Direction.TB | Direction.TD | Direction.BT => SelfLoopSide.Right
      case Direction.LR | Direction.RL                => SelfLoopSide.Top

    val ink =
      visible.map(InkBox.fromNode) ++
        routes.values.flatten.map(InkBox.fromPoint(_)) ++
        layoutEdges.flatMap(e => EdgeRenderer.edgeInk(cfg, e, nodeMap, routes.getOrElse((e.from, e.to), Nil), loopSide))

    val fitted       = LayoutBounds.fit(lc.padding, ink)
    val shiftedNodes =
      if fitted.shiftX == 0 && fitted.shiftY == 0 then layoutNodes
      else layoutNodes.map(n => n.copy(center = Point(n.center.x + fitted.shiftX, n.center.y + fitted.shiftY)))
    val shiftedRoutes =
      if fitted.shiftX == 0 && fitted.shiftY == 0 then routes
      else routes.map((k, pts) => k -> pts.map(p => Point(p.x + fitted.shiftX, p.y + fitted.shiftY)))

    DiagramScene(
      width = fitted.width,
      height = fitted.height,
      nodes = shiftedNodes,
      edges = layoutEdges,
      routes = shiftedRoutes,
      subgraphs = Nil,
      notes = Nil,
      interactions = interactions,
      loopSide = loopSide,
      classDefRules = classDefRules,
      config = cfg,
      direction = dir,
    )
  end flowchartScene

  private def stateScene(
      authorDir: Direction,
      stmts: List[StateStatement],
      config: RenderConfig,
      viewport: Option[Viewport],
  ): DiagramScene =
    val dir         = effectiveDirection(authorDir, config.responsive, viewport)
    val transitions = stmts.collect { case StateStatement.TransitionSt(t) => t }
    val stateStyles = stmts.collect { case StateStatement.StyleSt(id, style) => id -> style }.toMap
    val noteStmts   = stmts.collect { case n: StateStatement.NoteSt => n }
    val notes       = SvgRenderer.indexByKey(noteStmts)(_.stateId).map { (n, idx) =>
      val align = stateStyles.get(n.stateId).flatMap(_.noteAlign).getOrElse(NoteTextAlign.Left)
      StateNote(n.position, n.stateId, n.text, align, n.alias, idx)
    }

    // Mermaid draws start and end as separate markers. A single shared `[*]` node makes a cycle through
    // the marker and longest-path ranking flips the diagram. Split when both roles appear.
    val endId             = NodeId.stateEnd
    val hasStart          = transitions.exists(_.from == NodeId.stateMarker)
    val hasEnd            = transitions.exists(_.to == NodeId.stateMarker)
    val splitStartEnd     = hasStart && hasEnd
    val nodeClasses       = StyleResolver.collectStateClasses(stmts, splitStartEnd, endId)
    val inlineStyles      = StyleResolver.collectStateInlineStyles(stmts)
    val classDefRules     = StyleResolver.stateClassDefsToRules(stmts)
    val edges: List[Edge] =
      transitions.map { t =>
        val to = if splitStartEnd && t.to == NodeId.stateMarker then endId else t.to
        Edge(t.from, to, EdgeStyle.Arrow, t.label, inline = t.inline)
      }
    val stateIds = (edges.map(_.from) ++ edges.map(_.to)).distinct
    val nodeDefs = stateIds.map { id =>
      if id == NodeId.stateMarker || id == endId then id -> NodeDef(id, Some(""), NodeShape.Circle)
      else id                                            -> NodeDef(id, Some(id.value), NodeShape.Round)
    }.toMap

    val lc          = compressLayout(config.layout, config.responsive, viewport, nodeDefs.size, dir)
    val cfg         = config.copy(layout = lc)
    val laid        = Layout.layout(lc, dir, nodeDefs, edges, measure = Some(TextMeasure.resolve(cfg)))
    val layoutNodes = laid.nodes.map { n =>
      val user   = nodeClasses.getOrElse(n.id, Nil)
      val styles = inlineStyles.getOrElse(n.id, Map.empty)
      if n.id == NodeId.stateMarker || n.id == endId then
        val marker =
          if n.id == endId then List(PaintClass.StartEnd.cssName, PaintClass.StateEnd.cssName)
          else List(PaintClass.StartEnd.cssName)
        n.copy(
          width = 16,
          height = 16,
          cssClasses = marker ++ user,
          styles = styles,
        )
      else n.copy(cssClasses = user, styles = styles)
      end if
    }
    val routes      = laid.routes
    val layoutEdges = SvgRenderer.buildLayoutEdges(edges)
    val nodeMap     = layoutNodes.map(n => n.id -> n).toMap
    val visible     = layoutNodes.filter(!_.dummy)

    val loopSide = dir match
      case Direction.TB | Direction.TD | Direction.BT => SelfLoopSide.Right
      case Direction.LR | Direction.RL                => SelfLoopSide.Top

    val selfLoopCounts    = edges.filter(e => e.from == e.to).groupBy(_.from).map((id, es) => id -> es.size)
    val bbSelfLoopExtents = selfLoopCounts.flatMap { case (id, count) =>
      nodeMap.get(id).map(node => id -> NoteRenderer.selfLoopBottomExtent(cfg, node, count))
    }

    val noteBoxes = notes.flatMap { note =>
      nodeMap.get(note.stateId).map(node => NoteRenderer.placeNote(cfg, note, node, visible, bbSelfLoopExtents))
    }

    val ink =
      visible.map(InkBox.fromNode) ++
        routes.values.flatten.map(InkBox.fromPoint(_)) ++
        layoutEdges.flatMap(e =>
          EdgeRenderer.edgeInk(cfg, e, nodeMap, routes.getOrElse((e.from, e.to), Nil), loopSide)
        ) ++
        noteBoxes.map(b => InkBox(b.x, b.y, b.w, b.h)) ++
        selfLoopCounts.flatMap { case (id, count) =>
          nodeMap.get(id).toList.flatMap { node =>
            val bottom = NoteRenderer.selfLoopBottomExtent(cfg, node, count)
            List(InkBox.fromPoint(Point(node.center.x, bottom)))
          }
        }

    val fitted       = LayoutBounds.fit(lc.padding, ink)
    val shiftedNodes =
      if fitted.shiftX == 0 && fitted.shiftY == 0 then layoutNodes
      else layoutNodes.map(n => n.copy(center = Point(n.center.x + fitted.shiftX, n.center.y + fitted.shiftY)))
    val shiftedRoutes =
      if fitted.shiftX == 0 && fitted.shiftY == 0 then routes
      else routes.map((k, pts) => k -> pts.map(p => Point(p.x + fitted.shiftX, p.y + fitted.shiftY)))

    DiagramScene(
      width = fitted.width,
      height = fitted.height,
      nodes = shiftedNodes,
      edges = layoutEdges,
      routes = shiftedRoutes,
      subgraphs = Nil,
      notes = notes,
      interactions = Map.empty,
      loopSide = loopSide,
      classDefRules = classDefRules,
      config = cfg,
      direction = dir,
    )
  end stateScene
end DiagramLayout
