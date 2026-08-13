package mermoid.ascent

import ascent.ast.{AscentEvent, Attr, UI}
import ascent.domtypes.{AttrValue, Events}
import ascent.dsl.*
import mermoid.*
import mermoid.css.{CssHybrid, CssRenderer, PaintClass, WrapperClass}
import zio.*

/** Paints a [[DiagramScene]] as hybrid HTML nodes + SVG edges. */
private[ascent] object HybridPainter:

  def paint(
      scene: DiagramScene,
      selected: Option[String],
      onSelect: String => UIO[Unit],
      scale: Double = 1.0,
      cssFit: Boolean = false,
  ): UI[Any] =
    val cfg      = scene.config
    val styleCss =
      val base          = RenderConfig.resolvedStylesheet(cfg)
      val withClassDefs =
        if scene.classDefRules.isEmpty then base
        else base.copy(rules = base.rules ++ scene.classDefRules)
      val htmlSheet = CssHybrid.htmlCompat(withClassDefs)
      val theme     = CssRenderer.render(htmlSheet, cfg.resolveVariables)
      theme + HybridChrome.css

    val edgeNodes = scene.edges
      .filter(e => scene.nodeMap.contains(e.from) && scene.nodeMap.contains(e.to))
      .map { e =>
        val raw = EdgeRenderer.edgeToSvg(
          cfg,
          e,
          scene.nodeMap,
          scene.loopSide,
          scene.routes.getOrElse((e.from, e.to), Nil),
        )
        markIncident(raw, e, selected)
      }

    val selfLoopCounts  = scene.edges.filter(e => e.from == e.to).groupBy(_.from).map((id, es) => id -> es.size)
    val selfLoopExtents = selfLoopCounts.flatMap { case (id, count) =>
      scene.nodeMap.get(id).map(node => id -> NoteRenderer.selfLoopBottomExtent(cfg, node, count))
    }
    val noteConnectors = scene.notes.flatMap { note =>
      NoteRenderer.noteToSvg(cfg, note, scene.nodeMap, selfLoopExtents).toList.flatMap(extractConnector)
    }
    val subgraphSvg =
      scene.subgraphs.flatMap(sg => SubgraphRenderer.subgraphToSvg(sg, scene.nodeMap.filter(!_._2.dummy)))

    val edgeSvg = SvgNode.Element(
      "svg",
      List(
        "class"   -> HybridClass.Edges.cssName,
        "xmlns"   -> "http://www.w3.org/2000/svg",
        "width"   -> scene.width.f,
        "height"  -> scene.height.f,
        "viewBox" -> s"0 0 ${scene.width.f} ${scene.height.f}",
      ),
      arrowheadDefs(cfg) :: subgraphSvg ++ edgeNodes ++ noteConnectors,
    )

    val htmlNodes = scene.visibleNodes.map(n => nodeButton(n, scene, selected, onSelect))
    val htmlNotes = scene.notes.flatMap(n => noteCard(n, scene, selfLoopExtents, selected, onSelect))

    val scalerStyle =
      s"width:${scene.width.f}px;height:${scene.height.f}px;transform:scale(${Num.format(scale)})"
    val wrapStyle =
      if cssFit then
        s"${HybridVar.SceneWidth.cssName}:${scene.width.f}px;${HybridVar.SceneHeight.cssName}:${scene.height.f}px;width:100%;max-width:100%"
      else s"width:${(scene.width * scale).f}px;height:${(scene.height * scale).f}px"
    val rootClass =
      if cssFit then s"${HybridClass.Root.cssName} ${HybridClass.Fit.cssName}"
      else HybridClass.Root.cssName

    val styleEl =
      if SvgBridge.cssIsEntitySafe(styleCss) then UI.Element("style", Vector.empty, Vector(UI.Text(styleCss)))
      else UI.Empty

    UI.Element(
      "div",
      Vector(
        Attr.StaticAttr("class", AttrValue.Str(rootClass)),
        Attr.StaticAttr("style", AttrValue.Str(wrapStyle)),
      ),
      Vector(
        styleEl,
        UI.Element(
          "div",
          Vector(
            Attr.StaticAttr(
              "class",
              AttrValue.Str(s"${HybridClass.Diagram.cssName} ${HybridClass.Scaler.cssName}"),
            ),
            Attr.StaticAttr("style", AttrValue.Str(scalerStyle)),
            Attr.StaticAttr("data-mermoid-width", AttrValue.Str(scene.width.f)),
            Attr.StaticAttr("data-mermoid-height", AttrValue.Str(scene.height.f)),
            Attr.StaticAttr("data-mermoid-direction", AttrValue.Str(scene.direction.toString)),
          ),
          Vector(SvgBridge.toUi(edgeSvg)) ++ htmlNodes ++ htmlNotes,
        ),
      ),
    )
  end paint

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

  private def markIncident(node: SvgNode, edge: LayoutEdge, selected: Option[String]): SvgNode =
    val incident = selected.exists(id => edge.from == id || edge.to == id)
    if !incident then node
    else
      node match
        case SvgNode.Element(tag, attrs, kids) =>
          val cls = attrs.collectFirst { case ("class", v) => v }.getOrElse(WrapperClass.Edge.cssName)
          SvgNode.Element(
            tag,
            attrs.filter(_._1 != "class") :+ ("class" -> s"$cls ${HybridClass.IsIncident.cssName}"),
            kids,
          )
        case other => other
    end if
  end markIncident

  /** Keep only the connector line from a note group for the SVG layer. */
  private def extractConnector(noteGroup: SvgNode): List[SvgNode] = noteGroup match
    case SvgNode.Element(_, _, kids) =>
      kids.collect {
        case line @ SvgNode.Element("line", attrs, _)
            if attrs.exists(_ == ("class" -> PaintClass.NoteConnector.cssName)) =>
          line
      }
    case _ => Nil

  private def nodeButton(
      node: LayoutNode,
      scene: DiagramScene,
      selected: Option[String],
      onSelect: String => UIO[Unit],
  ): UI[Any] =
    val interaction = scene.interactions.get(node.id)
    val left        = node.center.x - node.width / 2
    val top         = node.center.y - node.height / 2
    val isSel       = selected.contains(node.id)
    val classes     =
      List(
        HybridClass.Node.cssName,
        node.shape.wrapperClass,
      ) ++ node.cssClasses ++ Option.when(isSel)(PaintClass.IsSelected.cssName) ++
        Option.when(node.cssClasses.contains(PaintClass.StartEnd.cssName))(PaintClass.StartEnd.cssName)
    val style =
      s"left:${left.f}px;top:${top.f}px;width:${node.width.f}px;height:${node.height.f}px"
    val shapeStyle = CssHybrid.htmlInline(node.styles)
    val shapeAttrs =
      Vector(Attr.StaticAttr("class", AttrValue.Str(PaintClass.NodeShape.cssName))) ++
        ShapeRenderer.inlineStyle(shapeStyle).map(s => Attr.StaticAttr("style", AttrValue.Str(s)))
    val shapeEl: UI[Any] = UI.Element("span", shapeAttrs, Vector.empty)
    val label: UI[Any]   =
      if node.id == "[*]" then UI.Empty
      else
        UI.Element(
          "span",
          Vector(Attr.StaticAttr("class", AttrValue.Str(PaintClass.HybridNodeLabel.cssName))),
          Vector(UI.Text(node.label)),
        )
    val tip: Option[UI[Any]] = interaction.flatMap(_.tooltip).map { t =>
      UI.Element(
        "span",
        Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.Tooltip.cssName))),
        Vector(UI.Text(t)),
      )
    }
    // Match SVG rhombus polygon (midpoints of the AABB). Do not rotate a rect; that skews
    // non-square nodes and disagrees with EdgeRenderer ports.
    val body: UI[Any] = node.shape match
      case NodeShape.Rhombus if label != UI.Empty =>
        UI.Element(
          "span",
          Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.DiamondFill.cssName))),
          Vector(label),
        )
      case _ => label
    val click: Attr[Any] = Events.onClick { (_: AscentEvent) =>
      onSelect(node.id)
    }
    val kids: Vector[UI[Any]] =
      Vector[UI[Any]](shapeEl) ++
        (if body == UI.Empty then Vector.empty[UI[Any]] else Vector(body)) ++
        tip.toList
    val attrs = Vector(
      Attr.StaticAttr("class", AttrValue.Str(classes.mkString(" "))),
      Attr.StaticAttr("style", AttrValue.Str(style)),
      Attr.StaticAttr("type", AttrValue.Str("button")),
      Attr.StaticAttr("id", AttrValue.Str(s"node-${node.id}")),
      Attr.StaticAttr("aria-label", AttrValue.Str(if node.label.nonEmpty then node.label else node.id)),
      Attr.StaticAttr("data-node-id", AttrValue.Str(node.id)),
    ) ++ interaction.flatMap(_.tooltip).map(t => Attr.StaticAttr("title", AttrValue.Str(t))) ++ Vector(click)

    interaction.flatMap(_.href) match
      case Some(url) =>
        val linkAttrs = Vector(
          Attr.StaticAttr("href", AttrValue.Str(url)),
          Attr.StaticAttr("class", AttrValue.Str(classes.mkString(" ") + " " + HybridClass.NodeLink.cssName)),
          Attr.StaticAttr("style", AttrValue.Str(style)),
          Attr.StaticAttr("id", AttrValue.Str(s"node-${node.id}")),
          Attr.StaticAttr("data-node-id", AttrValue.Str(node.id)),
        ) ++ interaction.flatMap(_.linkTarget).map(t => Attr.StaticAttr("target", AttrValue.Str(t))) ++
          interaction.flatMap(_.tooltip).map(t => Attr.StaticAttr("title", AttrValue.Str(t))) :+
          Events.onClick((_: AscentEvent) => onSelect(node.id))
        UI.Element("a", linkAttrs, kids)
      case None =>
        UI.Element("button", attrs, kids)
    end match
  end nodeButton

  private def noteCard(
      note: StateNote,
      scene: DiagramScene,
      selfLoopExtents: Map[String, Double],
      selected: Option[String],
      onSelect: String => UIO[Unit],
  ): Option[UI[Any]] =
    scene.nodeMap.get(note.stateId).map { node =>
      val box     = NoteRenderer.placeNote(scene.config, note, node, scene.visibleNodes, selfLoopExtents)
      val isSel   = selected.contains(note.stateId)
      val classes = List(HybridClass.Note.cssName) ++ Option.when(isSel)(PaintClass.IsSelected.cssName)
      val style   = s"left:${box.x.f}px;top:${box.y.f}px;width:${box.w.f}px;min-height:${box.h.f}px"
      UI.Element(
        "div",
        Vector(
          Attr.StaticAttr("class", AttrValue.Str(classes.mkString(" "))),
          Attr.StaticAttr("style", AttrValue.Str(style)),
          Attr.StaticAttr("role", AttrValue.Str("note")),
          Attr.StaticAttr("aria-label", AttrValue.Str(s"Note for ${note.stateId}")),
          Events.onClick((_: AscentEvent) => onSelect(note.stateId)),
        ),
        Vector(UI.Text(note.text)),
      )
    }
end HybridPainter
