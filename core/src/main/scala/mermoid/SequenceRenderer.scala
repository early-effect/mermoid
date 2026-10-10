package mermoid

import mermoid.css.PaintClass

/** Paints a [[SequenceScene]]. Full SVG includes the stylesheet, background, notes, and headers. Hybrid keeps
  * [[chrome]]: markers, frames, lifelines, activations, and messages.
  */
private[mermoid] object SequenceRenderer:

  def paint(scene: SequenceScene): SvgNode =
    val config = scene.config
    val meta   =
      scene.accTitle.toList.map(text => SvgNode.elem("title")()(SvgNode.Text(text))) ++
        scene.accDescr.toList.map(text => SvgNode.elem("desc")()(SvgNode.Text(text)))
    SvgRenderer.svgRoot(
      scene.width,
      scene.height,
      meta ++ (markers :: SvgRenderer
        .styleBlock(config, scene.classDefRules)
        .toList ++ (background(scene) :: bands(scene) ++ frames(scene) ++ lifelines(scene) ++ activations(
        scene
      ) ++ messages(scene) ++ notes(scene) ++ headers(scene))),
    )
  end paint

  /** SVG layer for hybrid paint. Headers and notes are HTML. No background and no second stylesheet. */
  def chrome(scene: SequenceScene): List[SvgNode] =
    markers :: bands(scene) ++ frames(scene) ++ lifelines(scene) ++ activations(scene) ++ messages(scene)

  /** A 26px stereotype icon, origin at the top-left. Hybrid headers embed this above the name. */
  def icon(stereotype: SequenceStereotype): SvgNode =
    SvgNode.elem("svg")(
      "width"   -> StereotypeIcon.size.f,
      "height"  -> StereotypeIcon.size.f,
      "viewBox" -> s"0 0 ${StereotypeIcon.size.f} ${StereotypeIcon.size.f}",
    )(iconShapes(stereotype, StereotypeIcon.size / 2, StereotypeIcon.size / 2, StereotypeIcon.size)*)

  private def markers: SvgNode =
    val w = 9.0
    val h = 6.0
    SvgNode.elem("defs")()(
      filledMarker("seq-head", w, h, "auto"),
      filledMarker("seq-tail", w, h, "auto-start-reverse"),
      openMarker(w, h),
      crossMarker(w, h),
    )

  private def filledMarker(id: String, w: Double, h: Double, orient: String): SvgNode =
    SvgNode.elem("marker")(
      "id"           -> id,
      "markerWidth"  -> w.f,
      "markerHeight" -> h.f,
      "refX"         -> w.f,
      "refY"         -> (h / 2).f,
      "orient"       -> orient,
      "markerUnits"  -> "userSpaceOnUse",
    )(
      SvgNode.leaf("polygon")(
        "class"  -> PaintClass.Arrowhead.cssName,
        "points" -> s"0 0, ${w.f} ${(h / 2).f}, 0 ${h.f}",
      )
    )

  private def openMarker(w: Double, h: Double): SvgNode =
    SvgNode.elem("marker")(
      "id"           -> "seq-open",
      "markerWidth"  -> w.f,
      "markerHeight" -> h.f,
      "refX"         -> w.f,
      "refY"         -> (h / 2).f,
      "orient"       -> "auto",
      "markerUnits"  -> "userSpaceOnUse",
    )(
      SvgNode.leaf("polyline")(
        "class"  -> PaintClass.ArrowOpen.cssName,
        "points" -> s"0 0, ${w.f} ${(h / 2).f}, 0 ${h.f}",
        "fill"   -> "none",
      )
    )

  private def crossMarker(w: Double, h: Double): SvgNode =
    SvgNode.elem("marker")(
      "id"           -> "seq-cross",
      "markerWidth"  -> w.f,
      "markerHeight" -> h.f,
      "refX"         -> w.f,
      "refY"         -> (h / 2).f,
      "orient"       -> "auto",
      "markerUnits"  -> "userSpaceOnUse",
    )(
      SvgNode.leaf("path")(
        "class" -> PaintClass.ArrowCross.cssName,
        "d"     -> s"M 0 0 L ${w.f} ${h.f} M 0 ${h.f} L ${w.f} 0",
        "fill"  -> "none",
      )
    )

  private def background(scene: SequenceScene): SvgNode =
    SvgNode.leaf("rect")(
      "class"  -> PaintClass.DiagramBg.cssName,
      "x"      -> "0",
      "y"      -> "0",
      "width"  -> scene.width.f,
      "height" -> scene.height.f,
    )

  private def frames(scene: SequenceScene): List[SvgNode] =
    scene.groups.map(frame(scene, _))

  private def frame(scene: SequenceScene, group: PlacedGroup): SvgNode =
    val lineH     = scene.config.layout.lineHeight
    val box       = group.frame
    val rectAttrs = group.kind match
      case GroupKind.Highlight(color) =>
        val opacity = color.alpha.getOrElse(0.18)
        List(
          "style" -> s"fill: rgb(${color.r}, ${color.g}, ${color.b}); fill-opacity: ${Num.format(opacity)}"
        )
      case _ => Nil
    val rect = SvgNode.leaf("rect")(
      (
        List(
          "class"  -> PaintClass.FragmentFrame.cssName,
          "x"      -> box.x.f,
          "y"      -> box.y.f,
          "width"  -> box.w.f,
          "height" -> box.h.f,
          "rx"     -> "4",
        ) ++ rectAttrs
      )*
    )
    val tabPlate = group.tabBox.toList.map { tab =>
      SvgNode.leaf("rect")(
        "class"  -> PaintClass.FragmentTab.cssName,
        "x"      -> tab.x.f,
        "y"      -> tab.y.f,
        "width"  -> tab.w.f,
        "height" -> tab.h.f,
      )
    }
    val tab = group.tab.toList.map { text =>
      SvgNode.textElem("text")(
        "class"       -> PaintClass.FragmentLabel.cssName,
        "x"           -> (box.x + 8).f,
        "y"           -> (box.y + lineH).f,
        "text-anchor" -> "start",
      )(text)
    }
    val dividers = group.dividers.flatMap { divider =>
      val plate =
        if divider.tabWidth <= 0 then Nil
        else
          List(
            SvgNode.leaf("rect")(
              "class"  -> PaintClass.FragmentTab.cssName,
              "x"      -> divider.x0.f,
              "y"      -> (divider.y - lineH - 2).f,
              "width"  -> divider.tabWidth.f,
              "height" -> (lineH + 4).f,
            )
          )
      plate ++ List(
        SvgNode.leaf("line")(
          "class" -> PaintClass.FragmentDivider.cssName,
          "x1"    -> divider.x0.f,
          "y1"    -> divider.y.f,
          "x2"    -> divider.x1.f,
          "y2"    -> divider.y.f,
        ),
        SvgNode.textElem("text")(
          "class"       -> PaintClass.FragmentLabel.cssName,
          "x"           -> (divider.x0 + 8).f,
          "y"           -> (divider.y - 4).f,
          "text-anchor" -> "start",
        )(divider.label),
      )
    }
    SvgNode.elem("g")(
      "id"    -> s"fragment-${group.index}",
      "class" -> "fragment",
    )((rect :: tabPlate ++ tab ++ dividers)*)
  end frame

  private def bands(scene: SequenceScene): List[SvgNode] =
    scene.bands.zipWithIndex.map { (band, index) =>
      val title = band.title.toList.map { text =>
        SvgNode.textElem("text")(
          "class"       -> PaintClass.FragmentLabel.cssName,
          "x"           -> (band.rect.x + 8).f,
          "y"           -> (band.rect.y + 14).f,
          "text-anchor" -> "start",
        )(text)
      }
      SvgNode.elem("g")(
        "id"    -> s"band-$index",
        "class" -> "band",
      )(
        (SvgNode.leaf("rect")(
          "x"      -> band.rect.x.f,
          "y"      -> band.rect.y.f,
          "width"  -> band.rect.w.f,
          "height" -> band.rect.h.f,
          "style"  -> s"fill: ${band.fill}; fill-opacity: 0.35; stroke: none",
        ) :: title)*
      )
    }

  private def lifelines(scene: SequenceScene): List[SvgNode] =
    scene.lifelines.map { line =>
      val shaft = SvgNode.leaf("line")(
        "id"    -> s"lifeline-${line.id.value}",
        "class" -> PaintClass.Lifeline.cssName,
        "x1"    -> line.x.f,
        "y1"    -> line.y0.f,
        "x2"    -> line.x.f,
        "y2"    -> line.y1.f,
      )
      val cross =
        if !line.destroyed then Nil
        else
          val s = 6.0
          List(
            SvgNode.leaf("line")(
              "class" -> PaintClass.ActorFigure.cssName,
              "x1"    -> (line.x - s).f,
              "y1"    -> (line.y1 - s).f,
              "x2"    -> (line.x + s).f,
              "y2"    -> (line.y1 + s).f,
            ),
            SvgNode.leaf("line")(
              "class" -> PaintClass.ActorFigure.cssName,
              "x1"    -> (line.x - s).f,
              "y1"    -> (line.y1 + s).f,
              "x2"    -> (line.x + s).f,
              "y2"    -> (line.y1 - s).f,
            ),
          )
      if cross.isEmpty then shaft
      else SvgNode.elem("g")()((shaft :: cross)*)
    }

  private def activations(scene: SequenceScene): List[SvgNode] =
    scene.activations.zipWithIndex.map { (bar, index) =>
      SvgNode.leaf("rect")(
        "id"     -> s"activation-$index",
        "class"  -> PaintClass.Activation.cssName,
        "x"      -> bar.rect.x.f,
        "y"      -> bar.rect.y.f,
        "width"  -> bar.rect.w.f,
        "height" -> bar.rect.h.f,
      )
    }

  private def messages(scene: SequenceScene): List[SvgNode] =
    scene.messages.map(message(scene, _))

  private def message(scene: SequenceScene, message: PlacedMessage): SvgNode =
    val lineH  = scene.config.layout.lineHeight
    val anchor = message.path match
      case MessagePath.Straight(_, _)   => "middle"
      case MessagePath.Hook(_, _, _, _) => "start"
    val dash = message.arrow.line match
      case SequenceLine.Solid  => "message-solid"
      case SequenceLine.Dashed => "message-dashed"
    val shaft = SvgNode.leaf("path")(
      (List(
        "class" -> PaintClass.MessageLine.cssName,
        "d"     -> pathD(message.path),
        "fill"  -> "none",
      ) ++ markerAttrs(message.arrow))*
    )
    val labels = message.shown.zipWithIndex.map { (line, i) =>
      SvgNode.textElem("text")(
        "class"       -> PaintClass.MessageLabel.cssName,
        "x"           -> message.labelAt.x.f,
        "y"           -> (message.labelAt.y + i * lineH).f,
        "text-anchor" -> anchor,
      )(line)
    }
    SvgNode.elem("g")(
      "id"    -> s"message-${message.index}",
      "class" -> s"message $dash",
    )((shaft :: labels)*)
  end message

  private def markerAttrs(arrow: SequenceArrow): List[(String, String)] =
    val head = arrow.head match
      case SequenceHead.None   => Nil
      case SequenceHead.Filled => List("marker-end" -> "url(#seq-head)")
      case SequenceHead.Open   => List("marker-end" -> "url(#seq-open)")
      case SequenceHead.Cross  => List("marker-end" -> "url(#seq-cross)")
    val tail = arrow.tail match
      case SequenceHead.Filled => List("marker-start" -> "url(#seq-tail)")
      case _                   => Nil
    head ++ tail
  end markerAttrs

  private def pathD(path: MessagePath): String = path match
    case MessagePath.Straight(from, to) =>
      s"M ${from.x.f} ${from.y.f} L ${to.x.f} ${to.y.f}"
    case MessagePath.Hook(out, down, back, head) =>
      s"M ${out.x.f} ${out.y.f} L ${down.x.f} ${down.y.f} L ${back.x.f} ${back.y.f} L ${head.x.f} ${head.y.f}"

  private def notes(scene: SequenceScene): List[SvgNode] =
    val lineH = scene.config.layout.lineHeight
    scene.notes.map { note =>
      val lines = note.lines.zipWithIndex.map { (line, i) =>
        SvgNode.textElem("text")(
          "class"       -> PaintClass.NoteText.cssName,
          "x"           -> (note.box.x + 8).f,
          "y"           -> (note.box.y + 16 + i * lineH).f,
          "text-anchor" -> "start",
        )(line)
      }
      val fold = math.min(10.0, math.min(note.box.w, note.box.h) / 2)
      val x    = note.box.x
      val y    = note.box.y
      val w    = note.box.w
      val h    = note.box.h
      SvgNode.elem("g")(
        "id"    -> s"note-${note.index}",
        "class" -> "note",
      )(
        (SvgNode.leaf("path")(
          "class" -> PaintClass.NoteRect.cssName,
          "d"     ->
            s"M ${x.f} ${y.f} L ${(x + w - fold).f} ${y.f} L ${(x + w).f} ${(y + fold).f} L ${(x + w).f} ${(y + h).f} L ${x.f} ${(y + h).f} Z",
        ) :: SvgNode.leaf("path")(
          "class" -> PaintClass.NoteRect.cssName,
          "style" -> "fill: none",
          "d"     ->
            s"M ${(x + w - fold).f} ${y.f} L ${(x + w - fold).f} ${(y + fold).f} L ${(x + w).f} ${(y + fold).f}",
        ) :: lines)*
      )
    }
  end notes

  private def headers(scene: SequenceScene): List[SvgNode] =
    val lineH = scene.config.layout.lineHeight
    scene.participants.map { person =>
      val style     = ShapeRenderer.inlineStyle(person.styles).toList.map("style" -> _)
      val body      = headerBody(person, lineH, style)
      val kindClass = (person.stereotype, person.kind) match
        case (Some(SequenceStereotype.Actor), _) | (None, ParticipantKind.Actor) => "actor actor-person"
        case (Some(_), _)                                                        => "actor actor-stereotype"
        case (None, ParticipantKind.Participant)                                 => "actor"
      val user  = person.cssClasses.map(name => s" $name").mkString
      val tip   = headerTitle(person)
      val group = SvgNode.elem("g")(
        "id"    -> s"actor-${person.id.value}",
        "class" -> s"$kindClass$user",
      )((tip ++ body)*)
      person.links match
        case (_, href) :: Nil => SvgNode.Element("a", List("href" -> href), List(group))
        case _                => group
    }
  end headers

  private def headerTitle(person: PlacedParticipant): List[SvgNode] =
    val fromLinks = person.links.map((label, href) => s"$label ($href)")
    val lines     = person.tooltip.toList ++ fromLinks
    lines match
      case Nil => Nil
      case _   => List(SvgNode.elem("title")()(SvgNode.Text(lines.mkString("\n"))))

  private def headerBody(
      person: PlacedParticipant,
      lineH: Double,
      style: List[(String, String)],
  ): List[SvgNode] =
    val box     = person.box
    val labelAt = (y: Double) =>
      SvgNode.textElem("text")(
        "class"             -> PaintClass.ActorLabel.cssName,
        "x"                 -> person.centerX.f,
        "y"                 -> y.f,
        "text-anchor"       -> "middle",
        "dominant-baseline" -> "middle",
      )(person.label)
    person.stereotype match
      case Some(SequenceStereotype.Actor) =>
        stick(person).map(paintStyle(_, style)) ++ List(labelAt(box.y + ActorMetrics.height(lineH) - lineH / 2))
      case Some(stereo) =>
        val cx = person.centerX
        val cy = box.y + StereotypeIcon.size / 2
        iconShapes(stereo, cx, cy, StereotypeIcon.size).map(paintStyle(_, style)) ++ List(
          labelAt(box.y + StereotypeIcon.height(lineH) - lineH / 2)
        )
      case None =>
        person.kind match
          case ParticipantKind.Participant =>
            List(
              SvgNode.leaf("rect")(
                (
                  List(
                    "class"  -> PaintClass.ActorBox.cssName,
                    "x"      -> box.x.f,
                    "y"      -> box.y.f,
                    "width"  -> box.w.f,
                    "height" -> box.h.f,
                    "rx"     -> "8",
                  ) ++ style
                )*
              ),
              labelAt(box.y + box.h / 2),
            )
          case ParticipantKind.Actor =>
            stick(person).map(paintStyle(_, style)) ++ List(
              labelAt(box.y + ActorMetrics.height(lineH) - lineH / 2)
            )
    end match
  end headerBody

  private def paintStyle(node: SvgNode, style: List[(String, String)]): SvgNode =
    if style.isEmpty then node
    else
      node match
        case SvgNode.Element(name, attrs, children) => SvgNode.Element(name, attrs ++ style, children)
        case other                                  => other

  private def stick(person: PlacedParticipant): List[SvgNode] =
    val cx    = person.centerX
    val headY = person.box.y + ActorMetrics.headRadius
    val neckY = headY + ActorMetrics.headRadius
    val hipY  = neckY + ActorMetrics.neck + ActorMetrics.torso
    val armY  = neckY + ActorMetrics.neck + ActorMetrics.torso * 0.35
    List(
      SvgNode.leaf("circle")(
        "class" -> PaintClass.ActorFigure.cssName,
        "cx"    -> cx.f,
        "cy"    -> headY.f,
        "r"     -> ActorMetrics.headRadius.f,
      ),
      line(cx, neckY, cx, hipY),
      line(cx - ActorMetrics.arm, armY, cx + ActorMetrics.arm, armY),
      line(cx, hipY, cx - ActorMetrics.legSpread, hipY + ActorMetrics.leg),
      line(cx, hipY, cx + ActorMetrics.legSpread, hipY + ActorMetrics.leg),
    )
  end stick

  private def line(x1: Double, y1: Double, x2: Double, y2: Double): SvgNode =
    SvgNode.leaf("line")(
      "class" -> PaintClass.ActorFigure.cssName,
      "x1"    -> x1.f,
      "y1"    -> y1.f,
      "x2"    -> x2.f,
      "y2"    -> y2.f,
    )

  private def iconShapes(stereotype: SequenceStereotype, cx: Double, cy: Double, size: Double): List[SvgNode] =
    val s = size / 2
    stereotype match
      case SequenceStereotype.Actor =>
        Nil
      case SequenceStereotype.Boundary =>
        List(
          line(cx - s * 0.7, cy - s * 0.7, cx - s * 0.7, cy + s * 0.7),
          line(cx - s * 0.7, cy, cx - s * 0.15, cy),
          SvgNode.leaf("circle")(
            "class" -> PaintClass.ActorFigure.cssName,
            "cx"    -> (cx + s * 0.25).f,
            "cy"    -> cy.f,
            "r"     -> (s * 0.5).f,
          ),
        )
      case SequenceStereotype.Control =>
        List(
          SvgNode.leaf("circle")(
            "class" -> PaintClass.ActorFigure.cssName,
            "cx"    -> cx.f,
            "cy"    -> (cy + s * 0.1).f,
            "r"     -> (s * 0.55).f,
          ),
          SvgNode.leaf("polygon")(
            "class" -> PaintClass.ActorFigure.cssName,
            "points" -> s"${cx.f},${(cy - s).f} ${(cx + s * 0.35).f},${(cy - s * 0.35).f} ${(cx - s * 0.15).f},${(cy - s * 0.35).f}",
          ),
        )
      case SequenceStereotype.Entity =>
        List(
          SvgNode.leaf("circle")(
            "class" -> PaintClass.ActorFigure.cssName,
            "cx"    -> cx.f,
            "cy"    -> (cy - s * 0.2).f,
            "r"     -> (s * 0.45).f,
          ),
          line(cx - s * 0.7, cy + s * 0.7, cx + s * 0.7, cy + s * 0.7),
        )
      case SequenceStereotype.Database =>
        val top = cy - s * 0.55
        val bot = cy + s * 0.55
        val rx  = s * 0.7
        val ry  = s * 0.22
        List(
          SvgNode.leaf("path")(
            "class" -> PaintClass.ActorBox.cssName,
            "d"     ->
              s"M ${(cx - rx).f} ${top.f} L ${(cx - rx).f} ${bot.f} A ${rx.f} ${ry.f} 0 0 0 ${(cx + rx).f} ${bot.f} L ${(cx + rx).f} ${top.f}",
          ),
          SvgNode.leaf("ellipse")(
            "class" -> PaintClass.ActorBox.cssName,
            "cx"    -> cx.f,
            "cy"    -> top.f,
            "rx"    -> rx.f,
            "ry"    -> ry.f,
          ),
        )
      case SequenceStereotype.Collections =>
        List(
          SvgNode.leaf("rect")(
            "class"  -> PaintClass.ActorBox.cssName,
            "x"      -> (cx - s * 0.15).f,
            "y"      -> (cy - s * 0.75).f,
            "width"  -> (s * 1.1).f,
            "height" -> (s * 1.15).f,
          ),
          SvgNode.leaf("rect")(
            "class"  -> PaintClass.ActorBox.cssName,
            "x"      -> (cx - s * 0.7).f,
            "y"      -> (cy - s * 0.35).f,
            "width"  -> (s * 1.1).f,
            "height" -> (s * 1.15).f,
          ),
        )
      case SequenceStereotype.Queue =>
        SvgNode.leaf("polygon")(
          "class"  -> PaintClass.ActorBox.cssName,
          "points" ->
            s"${(cx - s * 0.8).f},${(cy - s * 0.35).f} ${(cx + s * 0.55).f},${(cy - s * 0.7).f} ${(cx + s * 0.8).f},${(cy + s * 0.35).f} ${(cx - s * 0.55).f},${(cy + s * 0.7).f}",
        ) :: Nil
    end match
  end iconShapes
end SequenceRenderer
