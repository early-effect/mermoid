package mermoid

/** Flowchart subgraphs as compound nodes. A diagram with no subgraph keeps the flat ranker. */
private[mermoid] object FlowLayout:

  def containsSubgraph(stmts: List[FlowStatement]): Boolean =
    stmts.exists {
      case FlowStatement.SubgraphSt(_, _, _, _) => true
      case _                                    => false
    } || stmts.exists {
      case FlowStatement.SubgraphSt(_, _, _, inner) => containsSubgraph(inner)
      case _                                        => false
    }

  def scene(
      authorDir: Direction,
      stmts: List[FlowStatement],
      config: RenderConfig,
      viewport: Option[Viewport],
  ): DiagramScene =
    val dir     = DiagramLayout.effectiveDirection(authorDir, config.responsive, viewport)
    val count   = StyleResolver.collectNodes(stmts).size
    val lc      = DiagramLayout.compressLayout(config.layout, config.responsive, viewport, Math.max(count, 1), dir)
    val cfg     = config.copy(layout = lc)
    val region  = regionOf(stmts, dir, cfg, viewport)
    val placed  = CompoundLayout.place(dir, List(region), lc, TextMeasure.resolve(cfg))
    val classes = StyleResolver.collectNodeClasses(stmts)
    val styles  = StyleResolver.collectInlineStyles(stmts)
    val nodes   = placed.nodes.map { node =>
      node.copy(
        cssClasses = classes.getOrElse(node.id, Nil),
        styles = styles.getOrElse(node.id, Map.empty),
      )
    }
    val loopSide = dir match
      case Direction.TB | Direction.TD | Direction.BT => SelfLoopSide.Right
      case Direction.LR | Direction.RL                => SelfLoopSide.Top
    val nodeMap = nodes.map(n => n.id -> n).toMap
    val visible = nodes.filter(!_.dummy)
    val edges   = SvgRenderer.buildLayoutEdges(placed.edges)
    val ink     =
      visible.map(InkBox.fromNode) ++
        placed.routes.values.flatten.map(InkBox.fromPoint(_)) ++
        placed.frames.map(f => InkBox(f.rect.x, f.rect.y, f.rect.w, f.rect.h)) ++
        edges.flatMap(e =>
          EdgeRenderer.edgeInk(cfg, e, nodeMap, placed.routes.getOrElse((e.from, e.to), Nil), loopSide)
        )
    val fitted                = LayoutBounds.fit(lc.padding, ink)
    def move(p: Point): Point = Point(p.x + fitted.shiftX, p.y + fitted.shiftY)
    DiagramScene(
      width = fitted.width,
      height = fitted.height,
      nodes =
        if fitted.shiftX == 0 && fitted.shiftY == 0 then nodes else nodes.map(n => n.copy(center = move(n.center))),
      edges = edges,
      routes =
        if fitted.shiftX == 0 && fitted.shiftY == 0 then placed.routes
        else placed.routes.map((k, pts) => k -> pts.map(move)),
      subgraphs = placed.frames.map { frame =>
        frame.copy(
          rect = frame.rect.copy(x = frame.rect.x + fitted.shiftX, y = frame.rect.y + fitted.shiftY),
          dividers = frame.dividers.map(_ + fitted.shiftY),
        )
      },
      notes = Nil,
      interactions = StyleResolver.collectInteractions(stmts),
      loopSide = loopSide,
      classDefRules = StyleResolver.classDefsToRules(stmts),
      config = cfg,
      direction = dir,
    )
  end scene

  private def regionOf(
      stmts: List[FlowStatement],
      parentDir: Direction,
      config: RenderConfig,
      viewport: Option[Viewport],
  ): CompoundLayout.Region =
    val groups = stmts.collect { case FlowStatement.SubgraphSt(id, label, innerDir, inner) =>
      val dir = innerDir match
        case Some(authored) => DiagramLayout.effectiveDirection(authored, config.responsive, viewport)
        case None           => parentDir
      val child = regionOf(inner, dir, config, viewport)
      CompoundLayout.Item.Group(NodeId.trusted(id), label.orElse(Some(id)), dir, List(child), Nil, Map.empty)
    }
    val members = groups.flatMap {
      case CompoundLayout.Item.Group(_, _, _, regions, _, _) => regions.flatMap(idsOf)
      case _                                                 => Set.empty
    }.toSet
    val edges   = stmts.collect { case FlowStatement.EdgeSt(edge, _, _) => edge }
    val defs    = StyleResolver.collectNodes(stmts)
    val leafIds =
      (stmts.collect { case FlowStatement.NodeSt(node) => node.id } ++ edges.flatMap(e => List(e.from, e.to)))
        .filterNot(members.contains)
        .distinct
    val leaves = leafIds.map { id =>
      val defn = defs.getOrElse(id, NodeDef(id, Some(id.value), NodeShape.Rect))
      CompoundLayout.Item.Leaf(defn, Nil, Map.empty, None)
    }
    CompoundLayout.Region(leaves ++ groups, edges)
  end regionOf

  private def idsOf(region: CompoundLayout.Region): Set[NodeId] =
    region.items.flatMap {
      case CompoundLayout.Item.Leaf(defn, _, _, _)          => Set(defn.id)
      case CompoundLayout.Item.Group(id, _, _, inner, _, _) => inner.flatMap(idsOf).toSet + id
    }.toSet
end FlowLayout
