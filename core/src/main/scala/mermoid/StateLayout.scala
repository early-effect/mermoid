package mermoid

import mermoid.css.PaintClass

/** Lays a resolved state machine out as nested regions. A flat machine stays on the legacy ranker. */
private[mermoid] object StateLayout:

  def scene(
      machine: StateMachine,
      config: RenderConfig,
      viewport: Option[Viewport],
  ): DiagramScene =
    val dir     = DiagramLayout.effectiveDirection(machine.direction, config.responsive, viewport)
    val counted = machine.regions.map(count).sum
    val lc      = DiagramLayout.compressLayout(config.layout, config.responsive, viewport, Math.max(counted, 1), dir)
    val first   = lay(machine, config.copy(layout = lc), viewport, dir, lc)
    machine.scaleWidth match
      case Some(px) if viewport.isEmpty && first.width > px && px > 0 =>
        val scale  = Math.max(config.responsive.minSpacingScale, px / first.width)
        val shrunk = lc.copy(
          hSpacing = lc.hSpacing * scale,
          vSpacing = lc.vSpacing * scale,
          padding = Math.max(12.0, lc.padding * scale),
        )
        lay(machine, config.copy(layout = shrunk), viewport, dir, shrunk)
      case _ => first
    end match
  end scene

  private def lay(
      machine: StateMachine,
      config: RenderConfig,
      viewport: Option[Viewport],
      direction: Direction,
      layout: LayoutConfig,
  ): DiagramScene =
    val measure = TextMeasure.resolve(config)
    val regions = machine.regions.map(region => convert(region, config, viewport))
    val placed  = CompoundLayout.place(direction, regions, layout, measure)
    val notes   = machine.regions.flatMap(collectNotes)
    val floats  = machine.regions.flatMap(collectFloating)
    finish(placed, notes, floats, interactions(machine), machine, config, direction)
  end lay

  private def convert(
      region: StateRegion,
      config: RenderConfig,
      viewport: Option[Viewport],
  ): CompoundLayout.Region =
    val items = region.nodes.map {
      case StateNode.Atom(id, label, form, classes, styles, _) =>
        val marker                  = classes.contains(PaintClass.StartEnd.cssName)
        val bar                     = form == StateForm.Fork || form == StateForm.Join
        val history                 = form == StateForm.ShallowHistory || form == StateForm.DeepHistory
        val (shape, fixed, painted) =
          if marker then (NodeShape.Circle, Some((16.0, 16.0)), classes)
          else if bar then (NodeShape.Rect, Some((48.0, barThick)), PaintClass.StateBar.cssName :: classes)
          else if history then (NodeShape.Circle, Some((28.0, 28.0)), classes)
          else if form == StateForm.Choice then (NodeShape.Rhombus, None, classes)
          else (NodeShape.Round, None, classes)
        CompoundLayout.Item.Leaf(NodeDef(id, Some(label), shape), painted, styles, fixed)
      case StateNode.Composite(id, title, dir, inner, classes, styles, _) =>
        val flipped = DiagramLayout.effectiveDirection(dir, config.responsive, viewport)
        CompoundLayout.Item.Group(
          id,
          title,
          flipped,
          inner.map(region => convert(region, config, viewport)),
          classes,
          styles,
        )
    }
    CompoundLayout.Region(items, region.edges)
  end convert

  private val barThick = 8.0

  private def count(region: StateRegion): Int =
    region.nodes.map {
      case StateNode.Atom(_, _, _, _, _, _)             => 1
      case StateNode.Composite(_, _, _, inner, _, _, _) => inner.map(count).sum
    }.sum

  private def collectNotes(region: StateRegion): List[StateStatement.NoteSt] =
    region.notes ++ region.nodes.collect { case StateNode.Composite(_, _, _, inner, _, _, _) =>
      inner.flatMap(collectNotes)
    }.flatten

  private def collectFloating(region: StateRegion): List[StateStatement.FloatingNote] =
    region.floating ++ region.nodes.collect { case StateNode.Composite(_, _, _, inner, _, _, _) =>
      inner.flatMap(collectFloating)
    }.flatten

  private def interactions(machine: StateMachine): Map[NodeId, NodeInteraction] =
    machine.regions.foldLeft(Map.empty[NodeId, NodeInteraction])((acc, region) => acc ++ interactions(region))

  private def interactions(region: StateRegion): Map[NodeId, NodeInteraction] =
    region.nodes.foldLeft(Map.empty[NodeId, NodeInteraction]) {
      case (acc, StateNode.Atom(id, _, _, _, _, Some(hit))) =>
        acc + (id -> hit)
      case (acc, StateNode.Composite(id, _, _, inner, _, _, hit)) =>
        val withOwn = hit.fold(acc)(value => acc + (id -> value))
        inner.foldLeft(withOwn)((a, child) => a ++ interactions(child))
      case (acc, _) => acc
    }

  private def finish(
      placed: CompoundLayout.Placed,
      noteStmts: List[StateStatement.NoteSt],
      floating: List[StateStatement.FloatingNote],
      interactions: Map[NodeId, NodeInteraction],
      machine: StateMachine,
      config: RenderConfig,
      direction: Direction,
  ): DiagramScene =
    val loopSide = direction match
      case Direction.TB | Direction.TD | Direction.BT => SelfLoopSide.Right
      case Direction.LR | Direction.RL                => SelfLoopSide.Top
    val nodeMap = placed.nodes.map(n => n.id -> n).toMap
    val visible = placed.nodes.filter(!_.dummy)
    val edges   = SvgRenderer.buildLayoutEdges(placed.edges)
    val notes   = SvgRenderer.indexByKey(noteStmts)(_.stateId).map { (n, idx) =>
      StateNote(n.position, n.stateId, n.text, NoteTextAlign.Left, n.alias, idx)
    }
    val noteBoxes = notes.flatMap { note =>
      nodeMap.get(note.stateId).map(node => NoteRenderer.placeNote(config, note, node, visible, Map.empty))
    }
    val floatBoxes = placeFloating(floating, visible, config)
    val ink        =
      visible.map(InkBox.fromNode) ++
        placed.routes.values.flatten.map(InkBox.fromPoint(_)) ++
        placed.frames.map(f => InkBox(f.rect.x, f.rect.y, f.rect.w, f.rect.h)) ++
        edges.flatMap(e =>
          EdgeRenderer.edgeInk(config, e, nodeMap, placed.routes.getOrElse((e.from, e.to), Nil), loopSide)
        ) ++
        noteBoxes.map(b => InkBox(b.x, b.y, b.w, b.h)) ++
        floatBoxes.map(b => InkBox(b.rect.x, b.rect.y, b.rect.w, b.rect.h))
    val fitted                      = LayoutBounds.fit(config.layout.padding, ink)
    def shiftPoint(p: Point): Point = Point(p.x + fitted.shiftX, p.y + fitted.shiftY)
    val nodes                       =
      if fitted.shiftX == 0 && fitted.shiftY == 0 then placed.nodes
      else placed.nodes.map(n => n.copy(center = shiftPoint(n.center)))
    val routes =
      if fitted.shiftX == 0 && fitted.shiftY == 0 then placed.routes
      else placed.routes.map((k, pts) => k -> pts.map(shiftPoint))
    val frames =
      placed.frames.map { frame =>
        frame.copy(
          rect = frame.rect.copy(x = frame.rect.x + fitted.shiftX, y = frame.rect.y + fitted.shiftY),
          dividers = frame.dividers.map(_ + fitted.shiftY),
        )
      }
    val movedFloats = floatBoxes.map { box =>
      box.copy(rect = box.rect.copy(x = box.rect.x + fitted.shiftX, y = box.rect.y + fitted.shiftY))
    }
    DiagramScene(
      width = fitted.width,
      height = fitted.height,
      nodes = nodes,
      edges = edges,
      routes = routes,
      subgraphs = frames,
      notes = notes,
      interactions = interactions,
      loopSide = loopSide,
      classDefRules = machine.classDefRules,
      config = config,
      direction = direction,
      accTitle = machine.accTitle,
      accDescr = machine.accDescr,
      floatingNotes = movedFloats,
    )
  end finish

  private def placeFloating(
      notes: List[StateStatement.FloatingNote],
      nodes: List[LayoutNode],
      config: RenderConfig,
  ): List[FloatingNoteBox] =
    val right = nodes.map(n => n.center.x + n.width / 2).maxOption.getOrElse(0.0) + 24.0
    notes.zipWithIndex.map { case (note, index) =>
      val textW = TextMeasure.resolve(config).width(note.text, 12, config.layout.fontFamily)
      val w     = Math.max(80.0, textW + 16)
      val h     = 28.0
      FloatingNoteBox(note.text, note.alias, Rect(right, 12.0 + index * (h + 8), w, h))
    }
  end placeFloating
end StateLayout
