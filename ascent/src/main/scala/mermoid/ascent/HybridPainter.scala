package mermoid.ascent

import ascent.ast.{AscentEvent, Attr, UI}
import ascent.domtypes.{AttrValue, Events}
import ascent.dsl.*
import mermoid.*
import mermoid.css.CssRenderer
import zio.*

/** Paints a [[DiagramScene]] as hybrid HTML nodes + SVG edges. */
private[ascent] object HybridPainter:

  private val hybridCss: String =
    """
      |.mermoid-diagram{position:relative;font-family:var(--mermoid-font-family,sans-serif);font-size:var(--mermoid-font-size,14px);color:var(--mermoid-text,#333);background:var(--mermoid-background,#ffffff);overflow:visible}
      |.mermoid-diagram-scaler{position:relative;transform-origin:top left}
      |.mermoid-edges{position:absolute;inset:0;width:100%;height:100%;overflow:visible;pointer-events:none}
      |.mermoid-edges .edge.is-incident path,.mermoid-edges .edge.is-incident line{stroke-width:3;opacity:1}
      |.mermoid-node{position:absolute;display:flex;align-items:center;justify-content:center;box-sizing:border-box;margin:0;padding:4px 8px;cursor:pointer;border:2px solid var(--mermoid-node-border,#9370db);background:var(--mermoid-main-bkg,#ececff);color:var(--mermoid-text,#333);font:inherit;line-height:1.2;text-align:center;z-index:2}
      |.mermoid-node:hover{filter:brightness(1.05)}
      |.mermoid-node.is-selected{outline:3px solid var(--mermoid-line,#333);outline-offset:2px;z-index:3}
      |.mermoid-node.node-round{border-radius:15px}
      |.mermoid-node.node-stadium{border-radius:999px}
      |.mermoid-node.node-circle,.mermoid-node.node-double-circle{border-radius:50%}
      |.mermoid-node.node-rhombus{border:none;padding:0;background:var(--mermoid-node-border,#9370db);clip-path:polygon(50% 0%,100% 50%,50% 100%,0% 50%)}
      |.mermoid-node.node-rhombus .mermoid-node-diamond-fill{box-sizing:border-box;display:flex;align-items:center;justify-content:center;width:86%;height:86%;background:var(--mermoid-main-bkg,#ececff);color:inherit;clip-path:polygon(50% 0%,100% 50%,50% 100%,0% 50%);padding:2px 6px;line-height:1.2;text-align:center}
      |.mermoid-node.node-rhombus.is-selected{outline:none;filter:drop-shadow(0 0 1.5px var(--mermoid-line,#333)) drop-shadow(0 0 1.5px var(--mermoid-line,#333))}
      |.mermoid-node.node-rhombus.is-selected:hover{filter:brightness(1.05) drop-shadow(0 0 1.5px var(--mermoid-line,#333)) drop-shadow(0 0 1.5px var(--mermoid-line,#333))}
      |.mermoid-node.start-end{padding:0;border-radius:50%;background:var(--mermoid-primary-border,#333)}
      |.mermoid-note{position:absolute;box-sizing:border-box;padding:6px 10px;border:1px solid var(--mermoid-note-border,#333);background:var(--mermoid-note-bg,#ffc);color:var(--mermoid-note-text,#333);font-size:12px;white-space:pre-wrap;z-index:2;border-radius:3px}
      |.mermoid-note.is-selected{outline:2px solid var(--mermoid-line,#333)}
      |.mermoid-tooltip{display:none;position:absolute;left:50%;bottom:calc(100% + 8px);transform:translateX(-50%);background:#1c1d1f;color:#e8e6dc;padding:6px 10px;border-radius:4px;font-size:12px;white-space:nowrap;pointer-events:none;z-index:5;box-shadow:0 2px 8px rgba(0,0,0,.25)}
      |.mermoid-node:hover .mermoid-tooltip,.mermoid-node:focus-visible .mermoid-tooltip{display:block}
      |.mermoid-controls{display:flex;gap:8px;flex-wrap:wrap;margin-bottom:8px;align-items:center}
      |.mermoid-controls button{font:inherit;padding:4px 10px;cursor:pointer}
      |.mermoid-width-label{font-size:12px;opacity:.8}
      |""".stripMargin.replace("\n", "")

  def paint(
      scene: DiagramScene,
      selected: Option[String],
      onSelect: String => UIO[Unit],
      scale: Double = 1.0,
  ): UI[Any] =
    val cfg      = scene.config
    val styleCss =
      val base          = RenderConfig.resolvedStylesheet(cfg)
      val withClassDefs =
        if scene.classDefRules.isEmpty then base
        else base.copy(rules = base.rules ++ scene.classDefRules)
      val theme = CssRenderer.render(withClassDefs, cfg.resolveVariables)
      theme + hybridCss

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

    val edgeSvg = SvgNode.Element(
      "svg",
      List(
        "class"   -> "mermoid-edges",
        "xmlns"   -> "http://www.w3.org/2000/svg",
        "width"   -> scene.width.f,
        "height"  -> scene.height.f,
        "viewBox" -> s"0 0 ${scene.width.f} ${scene.height.f}",
      ),
      arrowheadDefs(cfg) :: edgeNodes ++ noteConnectors,
    )

    val htmlNodes = scene.visibleNodes.map(n => nodeButton(n, scene, selected, onSelect))
    val htmlNotes = scene.notes.flatMap(n => noteCard(n, scene, selfLoopExtents, selected, onSelect))

    val scalerStyle =
      s"width:${scene.width.f}px;height:${scene.height.f}px;transform:scale(${Num.format(scale)})"
    val wrapStyle =
      s"width:${(scene.width * scale).f}px;height:${(scene.height * scale).f}px"

    val styleEl =
      if SvgBridge.cssIsEntitySafe(styleCss) then UI.Element("style", Vector.empty, Vector(UI.Text(styleCss)))
      else UI.Empty

    UI.Element(
      "div",
      Vector(
        Attr.StaticAttr("class", AttrValue.Str("mermoid-root")),
        Attr.StaticAttr("style", AttrValue.Str(wrapStyle)),
      ),
      Vector(
        styleEl,
        UI.Element(
          "div",
          Vector(
            Attr.StaticAttr("class", AttrValue.Str("mermoid-diagram mermoid-diagram-scaler")),
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
        "id"           -> "arrowhead",
        "markerWidth"  -> w.f,
        "markerHeight" -> h.f,
        "refX"         -> w.f,
        "refY"         -> (h / 2).f,
        "orient"       -> "auto",
        "markerUnits"  -> "userSpaceOnUse",
      )(
        SvgNode.leaf("polygon")(
          "class"  -> "arrowhead",
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
          val cls = attrs.collectFirst { case ("class", v) => v }.getOrElse("edge")
          SvgNode.Element(tag, attrs.filter(_._1 != "class") :+ ("class" -> s"$cls is-incident"), kids)
        case other => other

  /** Keep only the connector line from a note group for the SVG layer. */
  private def extractConnector(noteGroup: SvgNode): List[SvgNode] = noteGroup match
    case SvgNode.Element(_, _, kids) =>
      kids.collect {
        case line @ SvgNode.Element("line", attrs, _) if attrs.exists(_ == ("class" -> "note-connector")) =>
          line
      }
    case _ => Nil

  private def shapeClass(shape: NodeShape): String = ShapeRenderer.shapeCssClass(shape)

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
        "mermoid-node",
        s"node-${shapeClass(node.shape)}",
      ) ++ node.cssClasses ++ Option.when(isSel)("is-selected") ++
        Option.when(node.cssClasses.contains("start-end"))("start-end")
    val style =
      s"left:${left.f}px;top:${top.f}px;width:${node.width.f}px;height:${node.height.f}px" +
        ShapeRenderer.inlineStyle(node.styles).map(s => s";$s").getOrElse("")
    val label: UI[Any] =
      if node.id == "[*]" then UI.Empty
      else
        UI.Element(
          "span",
          Vector(Attr.StaticAttr("class", AttrValue.Str("mermoid-node-label"))),
          Vector(UI.Text(node.label)),
        )
    val tip: Option[UI[Any]] = interaction.flatMap(_.tooltip).map { t =>
      UI.Element("span", Vector(Attr.StaticAttr("class", AttrValue.Str("mermoid-tooltip"))), Vector(UI.Text(t)))
    }
    // Match SVG rhombus polygon (midpoints of the AABB). Do not rotate a rect; that skews
    // non-square nodes and disagrees with EdgeRenderer ports.
    val body: UI[Any] = node.shape match
      case NodeShape.Rhombus if label != UI.Empty =>
        UI.Element(
          "span",
          Vector(Attr.StaticAttr("class", AttrValue.Str("mermoid-node-diamond-fill"))),
          Vector(label),
        )
      case _ => label
    val click: Attr[Any] = Events.onClick { (_: AscentEvent) =>
      onSelect(node.id)
    }
    val kids: Vector[UI[Any]] =
      (if body == UI.Empty then Vector.empty else Vector(body)) ++ tip.toList
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
          Attr.StaticAttr("class", AttrValue.Str(classes.mkString(" ") + " mermoid-node-link")),
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
      val classes = List("mermoid-note") ++ Option.when(isSel)("is-selected")
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
