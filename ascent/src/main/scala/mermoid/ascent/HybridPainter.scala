package mermoid.ascent

import ascent.ast.{AscentEvent, Attr, UI}
import ascent.domtypes.{AttrValue, Events}
import ascent.dsl.*
import mermoid.*
import mermoid.css.{CssHybrid, CssProperty, CssRenderer, CssRule, CssSelector, PaintClass, Stylesheet, WrapperClass}
import zio.*

/** Paints a [[Scene]]. Ranked diagrams keep HTML nodes. Sequence headers are HTML buttons; lifelines and messages stay
  * SVG.
  */
private[ascent] object HybridPainter:

  def paint(
      scene: Scene,
      selected: Option[NodeId],
      onSelect: NodeId => UIO[Unit],
      scale: Double = 1.0,
      cssFit: Boolean = false,
  ): UI[Any] =
    scene match
      case Scene.Ranked(ranked) => paintRanked(ranked, selected, onSelect, scale, cssFit)
      case Scene.Sequence(seq)  => paintSequence(seq, selected, onSelect, scale, cssFit)

  private def paintRanked(
      scene: DiagramScene,
      selected: Option[NodeId],
      onSelect: NodeId => UIO[Unit],
      scale: Double,
      cssFit: Boolean,
  ): UI[Any] =
    val cfg      = scene.config
    val styleCss =
      val base          = RenderConfig.resolvedStylesheet(cfg)
      val withClassDefs =
        if scene.classDefRules.isEmpty then base
        else base.copy(rules = base.rules ++ scene.classDefRules)
      val htmlSheet = boostClassDefs(CssHybrid.htmlCompat(withClassDefs), scene.classDefRules)
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
      scene.subgraphs.map(SubgraphRenderer.subgraphToSvg)

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

    shell(
      scene.config,
      scene.width,
      scene.height,
      scale,
      cssFit,
      styleCss,
      Some(scene.direction.toString),
      AccessibleName.of(scene.accTitle, scene.accDescr),
      SvgBridge.toUi(edgeSvg),
      htmlNodes ++ htmlNotes,
    )
  end paintRanked

  private def paintSequence(
      scene: SequenceScene,
      selected: Option[NodeId],
      onSelect: NodeId => UIO[Unit],
      scale: Double,
      cssFit: Boolean,
  ): UI[Any] =
    val cfg      = scene.config
    val styleCss =
      val theme = CssRenderer.render(CssHybrid.htmlCompat(RenderConfig.resolvedStylesheet(cfg)), cfg.resolveVariables)
      theme + HybridChrome.css
    val edgeSvg = SvgNode.Element(
      "svg",
      List(
        "class"   -> HybridClass.Edges.cssName,
        "xmlns"   -> "http://www.w3.org/2000/svg",
        "width"   -> scene.width.f,
        "height"  -> scene.height.f,
        "viewBox" -> s"0 0 ${scene.width.f} ${scene.height.f}",
      ),
      SequenceRenderer.chrome(scene),
    )
    val actors = scene.participants.map(actorButton(_, selected, onSelect))
    val notes  = scene.notes.map(sequenceNote)
    shell(
      cfg,
      scene.width,
      scene.height,
      scale,
      cssFit,
      styleCss,
      None,
      AccessibleName.of(scene.accTitle, scene.accDescr),
      SvgBridge.toUi(edgeSvg),
      actors ++ notes,
    )
  end paintSequence

  private def shell(
      config: RenderConfig,
      width: Double,
      height: Double,
      scale: Double,
      cssFit: Boolean,
      styleCss: String,
      direction: Option[String],
      name: Option[String],
      layer: UI[Any],
      html: Seq[UI[Any]],
  ): UI[Any] =
    val box = s"width:${width.f}px;height:${height.f}px"
    // An inline transform beats the fit class. CSS fit owns the transform when cssFit is set.
    val scalerStyle =
      if cssFit then box else s"$box;transform:scale(${Num.format(scale)})"
    val floorDecl = config.responsive.fit match
      case ContainerFit.ToWidth(floor) => s";${HybridVar.ScaleFloor.cssName}:${Num.format(floor)}"
      case ContainerFit.Off            => ""
    val wrapStyle =
      if cssFit then
        s"${HybridVar.SceneWidth.cssName}:${width.f}px;${HybridVar.SceneHeight.cssName}:${height.f}px$floorDecl;width:100%;max-width:100%"
      else s"width:${(width * scale).f}px;height:${(height * scale).f}px"
    val rootClass =
      if cssFit then s"${HybridClass.Root.cssName} ${HybridClass.Fit.cssName}"
      else HybridClass.Root.cssName
    val directionAttr = direction.toList.map(d => Attr.StaticAttr("data-mermoid-direction", AttrValue.Str(d)))
    val nameAttr      = name.toList.flatMap { text =>
      List(
        Attr.StaticAttr("role", AttrValue.Str("group")),
        Attr.StaticAttr("aria-label", AttrValue.Str(text)),
      )
    }
    val styleEl =
      if SvgBridge.cssIsEntitySafe(styleCss) then UI.Element("style", Vector.empty, Vector(UI.Text(styleCss)))
      else UI.Empty

    UI.Element(
      "div",
      Vector(
        Attr.StaticAttr("class", AttrValue.Str(rootClass)),
        Attr.StaticAttr("style", AttrValue.Str(wrapStyle)),
      ) ++ nameAttr,
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
            Attr.StaticAttr("data-mermoid-width", AttrValue.Str(width.f)),
            Attr.StaticAttr("data-mermoid-height", AttrValue.Str(height.f)),
          ) ++ directionAttr,
          Vector(layer) ++ html,
        ),
      ),
    )
  end shell

  private def actorButton(
      person: PlacedParticipant,
      selected: Option[NodeId],
      onSelect: NodeId => UIO[Unit],
  ): UI[Any] =
    val box    = person.box
    val isSel  = selected.contains(person.id)
    val figure = person.stereotype match
      case Some(SequenceStereotype.Actor) => true
      case Some(_)                        => false
      case None                           => person.kind == ParticipantKind.Actor
    val kind =
      if figure || person.stereotype.isDefined then HybridClass.ActorPerson.cssName
      else HybridClass.ActorBox.cssName
    val classes =
      List(HybridClass.Actor.cssName, kind) ++ person.cssClasses ++ Option.when(isSel)(PaintClass.IsSelected.cssName)
    val paint          = ShapeRenderer.inlineStyle(person.styles).map(extra => s";$extra").getOrElse("")
    val style          = s"left:${box.x.f}px;top:${box.y.f}px;width:${box.w.f}px;height:${box.h.f}px$paint"
    val label: UI[Any] = UI.Element(
      "span",
      Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.ActorLabel.cssName))),
      Vector(UI.Text(person.label)),
    )
    val leg: Int => UI[Any] = degrees =>
      UI.Element(
        "span",
        Vector(
          Attr.StaticAttr("class", AttrValue.Str(HybridClass.ActorLeg.cssName)),
          Attr.StaticAttr("style", AttrValue.Str(s"transform:rotate(${degrees}deg)")),
        ),
        Vector.empty,
      )
    val stick: Vector[UI[Any]] = Vector(
      UI.Element(
        "span",
        Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.ActorHead.cssName))),
        Vector.empty,
      ),
      UI.Element(
        "span",
        Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.ActorArms.cssName))),
        Vector.empty,
      ),
      UI.Element(
        "span",
        Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.ActorStem.cssName))),
        Vector.empty,
      ),
      UI.Element(
        "span",
        Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.ActorLegs.cssName))),
        Vector(leg(24), leg(-24)),
      ),
      label,
    )
    val kids: Vector[UI[Any]] =
      if figure then stick
      else
        person.stereotype match
          case Some(stereo) => Vector(SvgBridge.toUi(SequenceRenderer.icon(stereo)), label)
          case None         => Vector(label)
    UI.Element(
      "button",
      Vector(
        Attr.StaticAttr("type", AttrValue.Str("button")),
        Attr.StaticAttr("id", AttrValue.Str(s"actor-${person.id.value}")),
        Attr.StaticAttr("class", AttrValue.Str(classes.mkString(" "))),
        Attr.StaticAttr("style", AttrValue.Str(style)),
        Attr.StaticAttr("aria-label", AttrValue.Str(person.label)),
        Events.onClick((_: AscentEvent) => onSelect(person.id)),
      ),
      kids,
    )
  end actorButton

  private def sequenceNote(note: PlacedNote): UI[Any] =
    val box   = note.box
    val style = s"left:${box.x.f}px;top:${box.y.f}px;width:${box.w.f}px;min-height:${box.h.f}px"
    UI.Element(
      "div",
      Vector(
        Attr.StaticAttr("id", AttrValue.Str(s"note-${note.index}")),
        Attr.StaticAttr("class", AttrValue.Str(HybridClass.Note.cssName)),
        Attr.StaticAttr("style", AttrValue.Str(style)),
        Attr.StaticAttr("role", AttrValue.Str("note")),
      ),
      Vector(
        UI.Element(
          "span",
          Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.NoteFold.cssName))),
          Vector.empty,
        ),
        UI.Text(note.lines.mkString("\n")),
      ),
    )
  end sequenceNote

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

  private def markIncident(node: SvgNode, edge: LayoutEdge, selected: Option[NodeId]): SvgNode =
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
      selected: Option[NodeId],
      onSelect: NodeId => UIO[Unit],
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
    val painted    = CssHybrid.htmlInline(node.styles)
    val labelColor = painted.get(CssProperty.Color).map(c => s";color: $c").getOrElse("")
    val style      =
      s"left:${left.f}px;top:${top.f}px;width:${node.width.f}px;height:${node.height.f}px$labelColor"
    // `color` belongs on the button: the label is a sibling of the shape, so a color on the shape never reaches it.
    val shapeStyle = painted - CssProperty.Color
    val shapeAttrs =
      Vector(Attr.StaticAttr("class", AttrValue.Str(PaintClass.NodeShape.cssName))) ++
        ShapeRenderer.inlineStyle(shapeStyle).map(s => Attr.StaticAttr("style", AttrValue.Str(s)))
    val shapeEl: UI[Any] = UI.Element("span", shapeAttrs, Vector.empty)
    val label: UI[Any]   =
      if node.cssClasses.contains(PaintClass.StartEnd.cssName) then UI.Empty
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
      Attr.StaticAttr("aria-label", AttrValue.Str(if node.label.nonEmpty then node.label else node.id.value)),
      Attr.StaticAttr("data-node-id", AttrValue.Str(node.id.value)),
    ) ++ interaction.flatMap(_.tooltip).map(t => Attr.StaticAttr("title", AttrValue.Str(t))) ++ Vector(click)

    interaction.flatMap(_.href) match
      case Some(url) =>
        val linkAttrs = Vector(
          Attr.StaticAttr("href", AttrValue.Str(url)),
          Attr.StaticAttr("class", AttrValue.Str(classes.mkString(" ") + " " + HybridClass.NodeLink.cssName)),
          Attr.StaticAttr("style", AttrValue.Str(style)),
          Attr.StaticAttr("id", AttrValue.Str(s"node-${node.id}")),
          Attr.StaticAttr("data-node-id", AttrValue.Str(node.id.value)),
        ) ++ interaction.flatMap(_.linkTarget).map(t => Attr.StaticAttr("target", AttrValue.Str(t))) ++
          interaction.flatMap(_.tooltip).map(t => Attr.StaticAttr("title", AttrValue.Str(t))) :+
          Events.onClick((_: AscentEvent) => onSelect(node.id))
        UI.Element("a", linkAttrs, kids)
      case None =>
        UI.Element("button", attrs, kids)
    end match
  end nodeButton

  /** Copy classDef rules onto `.mermoid-node.<name>` so they outrank `.mermoid-node .node-shape` (0, 2, 0). */
  private def boostClassDefs(sheet: Stylesheet, classDefRules: List[CssRule]): Stylesheet =
    val names = classDefRules.collect { case CssRule(CssSelector.Class(name), _) => name }.toSet
    if names.isEmpty then sheet
    else
      val extra = sheet.rules.flatMap { rule =>
        boostedSelectors(rule.selector, names).map(sel => rule.copy(selector = sel))
      }
      sheet.copy(rules = sheet.rules ++ extra)

  private def boostedSelectors(sel: CssSelector, names: Set[String]): List[CssSelector] =
    val node = CssSelector.Class(HybridClass.Node.cssName)
    sel match
      case CssSelector.Class(name) if names.contains(name) =>
        List(CssSelector.Compound(List(node, CssSelector.Class(name))))
      case CssSelector.Descendant(CssSelector.Class(name), child) if names.contains(name) =>
        val compound = CssSelector.Compound(List(node, CssSelector.Class(name)))
        val shaped   = CssSelector.Descendant(compound, child)
        val diamond  =
          if child == PaintClass.NodeShape.selector then
            List(CssSelector.Descendant(compound, CssSelector.Class(HybridClass.DiamondFill.cssName)))
          else Nil
        shaped :: diamond
      case _ => Nil
    end match
  end boostedSelectors

  private def noteCard(
      note: StateNote,
      scene: DiagramScene,
      selfLoopExtents: Map[NodeId, Double],
      selected: Option[NodeId],
      onSelect: NodeId => UIO[Unit],
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
