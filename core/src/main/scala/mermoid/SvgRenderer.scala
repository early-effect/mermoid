package mermoid

import mermoid.css.*

case class RenderConfig(
    layout: LayoutConfig = LayoutConfig(),
    theme: ThemeName = ThemeName.Default,
    customStylesheet: Option[Stylesheet] = None,
    resolveVariables: Boolean = true,
)

object RenderConfig:
  def themeColors(config: RenderConfig): ThemeColors = Theme.colors(config.theme)

  def resolvedStylesheet(config: RenderConfig): Stylesheet =
    val base = Theme.toStylesheet(config.theme)
    config.customStylesheet match
      case Some(custom) => Stylesheet.merge(base, custom)
      case None         => base

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
        // A self-loop's index among its node's loops is its index within the (from, to) pair,
        // since a self-loop is the only kind of edge whose endpoints are the same node.
        selfLoopIndex = if e.from == e.to then idx else 0,
        alias = e.alias,
        edgeIndex = idx,
        edgeCount = pairCounts((e.from, e.to)),
      )
    }
  end buildLayoutEdges

  private def arrowheadDefs(config: RenderConfig): SvgNode =
    val lc = config.layout
    SvgNode.elem("defs")()(
      SvgNode.elem("marker")(
        "id"           -> "arrowhead",
        "markerWidth"  -> lc.arrowSize.f,
        "markerHeight" -> lc.arrowSize.f,
        "refX"         -> lc.arrowSize.f,
        "refY"         -> (lc.arrowSize / 2).f,
        "orient"       -> "auto",
      )(
        SvgNode.leaf("polygon")(
          "class"  -> "arrowhead",
          "points" -> s"0 0, ${lc.arrowSize.f} ${(lc.arrowSize / 2).f}, 0 ${lc.arrowSize.f}",
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

  def render(diagram: Diagram, config: RenderConfig = RenderConfig()): String =
    SvgSerializer.render(renderTree(diagram, config))

  /** The diagram as a tree — the integration point for consumers that build their own markup or UI. */
  def renderTree(diagram: Diagram, config: RenderConfig = RenderConfig()): SvgNode = diagram match
    case Diagram.StateDiagram(stmts)   => renderStateDiagram(stmts, config)
    case Diagram.Flowchart(dir, stmts) => renderFlowchart(dir, stmts, config)

  private def renderStateDiagram(stmts: List[StateStatement], config: RenderConfig): SvgNode =
    val lc          = config.layout
    val transitions = stmts.collect { case StateStatement.TransitionSt(t) => t }
    val stateStyles = stmts.collect { case StateStatement.StyleSt(id, style) => id -> style }.toMap
    val noteStmts   = stmts.collect { case n: StateStatement.NoteSt => n }
    val notes       = indexByKey(noteStmts)(_.stateId).map { (n, idx) =>
      val align = stateStyles.get(n.stateId).flatMap(_.noteAlign).getOrElse(NoteTextAlign.Left)
      StateNote(n.position, n.stateId, n.text, align, n.alias, idx)
    }

    val stateIds = (transitions.map(_.from) ++ transitions.map(_.to)).distinct
    val nodeDefs = stateIds.map { id =>
      if id == "[*]" then id -> NodeDef(id, Some(""), NodeShape.Circle)
      else id                -> NodeDef(id, Some(id), NodeShape.Round)
    }.toMap

    val edges       = transitions.map(t => Edge(t.from, t.to, EdgeStyle.Arrow, t.label))
    val layoutNodes = Layout.layout(lc, Direction.TB, nodeDefs, edges).map { n =>
      if n.id == "[*]" then n.copy(width = 16, height = 16, cssClasses = List("start-end"))
      else n
    }

    val layoutEdges = buildLayoutEdges(edges)
    val nodeMap     = layoutNodes.map(n => n.id -> n).toMap

    // Compute self-loop extents for bounding box
    val selfLoopCounts    = edges.filter(e => e.from == e.to).groupBy(_.from).map((id, es) => id -> es.size)
    val bbSelfLoopExtents = selfLoopCounts.flatMap { case (id, count) =>
      nodeMap.get(id).map(node => id -> NoteRenderer.selfLoopBottomExtent(config, node, count))
    }

    // Account for notes in bounding box
    val noteExtents = notes.flatMap { note =>
      nodeMap.get(note.stateId).map { node =>
        val maxLineWidth = note.text.split("\n").map(l => SvgUtil.estimateTextWidth(l, lc)).maxOption.getOrElse(60.0)
        val noteW        = maxLineWidth + 20
        val noteH        = note.text.split("\n").length * (lc.edgeLabelFontSize + 4) + 12
        val noteTopY     = bbSelfLoopExtents.get(note.stateId) match
          case Some(bottomY) => bottomY + 5
          case None          => node.center.y - noteH / 2
        val noteBottomY = noteTopY + noteH
        note.position match
          case NotePosition.RightOf =>
            val nx = node.center.x + node.width / 2 + 15
            (nx, nx + noteW, noteBottomY)
          case NotePosition.LeftOf =>
            val nx = node.center.x - node.width / 2 - 15 - noteW
            (nx, nx + noteW, noteBottomY)
      }
    }

    val noteMinX = noteExtents.map(_._1).minOption.getOrElse(lc.padding)
    val shiftX   = if noteMinX < lc.padding then lc.padding - noteMinX else 0.0

    val maxX = Math.max(
      layoutNodes.map(n => n.center.x + n.width / 2).maxOption.getOrElse(0.0) + shiftX,
      noteExtents.map(_._2).maxOption.getOrElse(0.0) + shiftX,
    ) + lc.padding
    val maxY = Math.max(
      layoutNodes.map(n => n.center.y + n.height / 2).maxOption.getOrElse(0.0),
      noteExtents.map(_._3).maxOption.getOrElse(0.0),
    ) + lc.padding

    val shiftedNodes =
      if shiftX > 0 then layoutNodes.map(n => n.copy(center = Point(n.center.x + shiftX, n.center.y)))
      else layoutNodes
    val shiftedNodeMap = shiftedNodes.map(n => n.id -> n).toMap

    val edgeSvg = layoutEdges
      .filter(e => shiftedNodeMap.contains(e.from) && shiftedNodeMap.contains(e.to))
      .map(e => EdgeRenderer.edgeToSvg(config, e, shiftedNodeMap, SelfLoopSide.Right))

    val nodeSvg = shiftedNodes.map(n => ShapeRenderer.nodeToSvg(n, config, includeLabel = n.id != "[*]"))

    // Recompute self-loop extents with shifted nodes
    val selfLoopExtents = selfLoopCounts.flatMap { case (id, count) =>
      shiftedNodeMap.get(id).map(node => id -> NoteRenderer.selfLoopBottomExtent(config, node, count))
    }
    val noteSvg = notes.flatMap(note => NoteRenderer.noteToSvg(config, note, shiftedNodeMap, selfLoopExtents))

    svgRoot(maxX, maxY, arrowheadDefs(config) :: styleBlock(config, Nil).toList ++ edgeSvg ++ nodeSvg ++ noteSvg)
  end renderStateDiagram

  private def renderFlowchart(dir: Direction, stmts: List[FlowStatement], config: RenderConfig): SvgNode =
    val lc            = config.layout
    val nodeDefs      = StyleResolver.collectNodes(stmts)
    val edges         = StyleResolver.collectEdges(stmts)
    val nodeClasses   = StyleResolver.collectNodeClasses(stmts)
    val inlineStyles  = StyleResolver.collectInlineStyles(stmts)
    val classDefRules = StyleResolver.classDefsToRules(stmts)
    val layoutNodes   = Layout.layout(lc, dir, nodeDefs, edges).map { n =>
      n.copy(
        cssClasses = nodeClasses.getOrElse(n.id, Nil),
        styles = inlineStyles.getOrElse(n.id, Map.empty),
      )
    }

    val layoutEdges = buildLayoutEdges(edges)
    val subgraphs   = StyleResolver.collectSubgraphs(stmts)
    val nodeMap     = layoutNodes.map(n => n.id -> n).toMap

    val maxX = layoutNodes.map(n => n.center.x + n.width / 2).maxOption.getOrElse(0.0) + lc.padding
    val maxY = layoutNodes.map(n => n.center.y + n.height / 2).maxOption.getOrElse(0.0) + lc.padding

    // Subgraph backgrounds render first, behind edges and nodes
    val subgraphSvg = subgraphs.flatMap(sg => SubgraphRenderer.subgraphToSvg(sg, nodeMap, config))

    val flowLoopSide = dir match
      case Direction.TB | Direction.TD | Direction.BT => SelfLoopSide.Right
      case Direction.LR | Direction.RL                => SelfLoopSide.Top

    val edgeSvg = layoutEdges
      .filter(e => nodeMap.contains(e.from) && nodeMap.contains(e.to))
      .map(e => EdgeRenderer.edgeToSvg(config, e, nodeMap, flowLoopSide))

    val nodeSvg = layoutNodes.map(n => ShapeRenderer.nodeToSvg(n, config))

    svgRoot(
      maxX,
      maxY,
      arrowheadDefs(config) :: styleBlock(config, classDefRules).toList ++ subgraphSvg ++ edgeSvg ++ nodeSvg,
    )
  end renderFlowchart
end SvgRenderer
