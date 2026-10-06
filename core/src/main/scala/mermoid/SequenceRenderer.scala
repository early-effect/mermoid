package mermoid

import mermoid.css.PaintClass

/** Paints a [[SequenceScene]]. Full SVG includes the stylesheet, background, notes, and headers. Hybrid keeps
  * [[chrome]]: markers, frames, lifelines, activations, and messages.
  */
private[mermoid] object SequenceRenderer:

  def paint(scene: SequenceScene): SvgNode =
    val config = scene.config
    SvgRenderer.svgRoot(
      scene.width,
      scene.height,
      markers(config) :: SvgRenderer
        .styleBlock(config, Nil)
        .toList ++ (background(scene) :: frames(scene) ++ lifelines(scene) ++ activations(scene) ++ messages(
        scene
      ) ++ notes(scene) ++ headers(scene)),
    )
  end paint

  /** SVG layer for hybrid paint. Headers and notes are HTML. No background and no second stylesheet. */
  def chrome(scene: SequenceScene): List[SvgNode] =
    markers(scene.config) :: frames(scene) ++ lifelines(scene) ++ activations(scene) ++ messages(scene)

  private def markers(config: RenderConfig): SvgNode =
    val w = config.layout.arrowSize
    val h = config.layout.arrowSize * 0.85
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
    val tab = group.tab.toList.map { text =>
      SvgNode.textElem("text")(
        "class"       -> PaintClass.FragmentLabel.cssName,
        "x"           -> (box.x + 8).f,
        "y"           -> (box.y + lineH).f,
        "text-anchor" -> "start",
      )(text)
    }
    val dividers = group.dividers.flatMap { divider =>
      List(
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
    )((rect :: tab ++ dividers)*)
  end frame

  private def lifelines(scene: SequenceScene): List[SvgNode] =
    scene.lifelines.map { line =>
      SvgNode.leaf("line")(
        "id"    -> s"lifeline-${line.id.value}",
        "class" -> PaintClass.Lifeline.cssName,
        "x1"    -> line.x.f,
        "y1"    -> line.y0.f,
        "x2"    -> line.x.f,
        "y2"    -> line.y1.f,
      )
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
      SvgNode.elem("g")(
        "id"    -> s"note-${note.index}",
        "class" -> "note",
      )(
        (SvgNode.leaf("rect")(
          "class"  -> PaintClass.NoteRect.cssName,
          "x"      -> note.box.x.f,
          "y"      -> note.box.y.f,
          "width"  -> note.box.w.f,
          "height" -> note.box.h.f,
          "rx"     -> "3",
        ) :: lines)*
      )
    }
  end notes

  private def headers(scene: SequenceScene): List[SvgNode] =
    val lineH = scene.config.layout.lineHeight
    scene.participants.map { person =>
      val box  = person.box
      val body = person.kind match
        case ParticipantKind.Participant =>
          List(
            SvgNode.leaf("rect")(
              "class"  -> PaintClass.ActorBox.cssName,
              "x"      -> box.x.f,
              "y"      -> box.y.f,
              "width"  -> box.w.f,
              "height" -> box.h.f,
              "rx"     -> "8",
            ),
            SvgNode.textElem("text")(
              "class"             -> PaintClass.ActorLabel.cssName,
              "x"                 -> person.centerX.f,
              "y"                 -> (box.y + box.h / 2).f,
              "text-anchor"       -> "middle",
              "dominant-baseline" -> "middle",
            )(person.label),
          )
        case ParticipantKind.Actor =>
          val cx = person.centerX
          val cy = box.y + ActorMetrics.headRadius
          List(
            SvgNode.leaf("circle")(
              "class" -> PaintClass.ActorFigure.cssName,
              "cx"    -> cx.f,
              "cy"    -> cy.f,
              "r"     -> ActorMetrics.headRadius.f,
            ),
            SvgNode.leaf("line")(
              "class" -> PaintClass.ActorFigure.cssName,
              "x1"    -> cx.f,
              "y1"    -> (cy + ActorMetrics.headRadius + ActorMetrics.gap).f,
              "x2"    -> cx.f,
              "y2"    -> (cy + ActorMetrics.headRadius + ActorMetrics.gap + ActorMetrics.stem).f,
            ),
            SvgNode.textElem("text")(
              "class"             -> PaintClass.ActorLabel.cssName,
              "x"                 -> cx.f,
              "y"                 -> (box.y + ActorMetrics.height(lineH) - lineH / 2).f,
              "text-anchor"       -> "middle",
              "dominant-baseline" -> "middle",
            )(person.label),
          )
      val kindClass = person.kind match
        case ParticipantKind.Participant => "actor"
        case ParticipantKind.Actor       => "actor actor-person"
      SvgNode.elem("g")(
        "id"    -> s"actor-${person.id.value}",
        "class" -> kindClass,
      )(body*)
    }
  end headers
end SequenceRenderer
