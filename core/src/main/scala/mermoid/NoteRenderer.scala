package mermoid

import mermoid.SvgNode.{leaf, textElem}

object NoteRenderer:

  /** Axis-aligned note box in scene coordinates. */
  final case class NoteBox(x: Double, y: Double, w: Double, h: Double):
    def right: Double  = x + w
    def bottom: Double = y + h

    def overlaps(node: LayoutNode, gap: Double): Boolean =
      val left   = node.center.x - node.width / 2 - gap
      val rightN = node.center.x + node.width / 2 + gap
      val top    = node.center.y - node.height / 2 - gap
      val bot    = node.center.y + node.height / 2 + gap
      x < rightN && right > left && y < bot && bottom > top
  end NoteBox

  def selfLoopBottomExtent(
      config: RenderConfig,
      node: LayoutNode,
      selfLoopCount: Int,
  ): Double =
    val hw       = node.width / 2
    val hh       = node.height / 2
    val cy       = node.center.y
    val lc       = config.layout
    val loopSize = Math.max(hw, hh) * 0.8 + lc.selfLoopSize
    val apexY    = cy + hh + loopSize * 0.5
    val startY   = node.shape match
      case NodeShape.Circle | NodeShape.DoubleCircle =>
        val r = Math.max(hw, hh)
        cy - r * Math.sin(Math.toRadians(-20))
      case _ =>
        cy + hh * 0.15
    val labelBaseY       = (startY + apexY) / 2 - (lc.edgeLabelFontSize + 16) * 0.5
    val lastLabelBottomY =
      labelBaseY + (selfLoopCount - 1) * (lc.edgeLabelFontSize + 16) + (lc.edgeLabelFontSize + 8) / 2
    Math.max(apexY, lastLabelBottomY)
  end selfLoopBottomExtent

  def noteSize(config: RenderConfig, note: StateNote): (Double, Double) =
    val lc           = config.layout
    val lines        = note.text.split("\n")
    val maxLineWidth = lines.map(l => SvgUtil.estimateTextWidth(l, lc)).maxOption.getOrElse(60.0)
    val noteW        = maxLineWidth + 20
    val lineHeight   = lc.edgeLabelFontSize + 4
    val noteH        = lines.length * lineHeight + 12
    (noteW, noteH)

  /** Place a note beside its state, dodging other nodes when the side slot is occupied. */
  def placeNote(
      config: RenderConfig,
      note: StateNote,
      node: LayoutNode,
      obstacles: Iterable[LayoutNode],
      selfLoopBottomExtents: Map[String, Double] = Map.empty,
  ): NoteBox =
    val (noteW, noteH) = noteSize(config, note)
    val gap            = 10.0
    val others         = obstacles.filter(n => n.id != node.id && !n.dummy).toList

    def clear(box: NoteBox): Boolean =
      !others.exists(n => box.overlaps(n, gap))

    val sideX = note.position match
      case NotePosition.RightOf => node.center.x + node.width / 2 + 15
      case NotePosition.LeftOf  => node.center.x - node.width / 2 - 15 - noteW

    val midY = selfLoopBottomExtents.get(note.stateId) match
      case Some(bottomY) => bottomY + 5
      case None          => node.center.y - noteH / 2

    val preferred = NoteBox(sideX, midY, noteW, noteH)
    if clear(preferred) then preferred
    else
      val aboveY = node.center.y - node.height / 2 - noteH - 12
      val belowY = node.center.y + node.height / 2 + 12
      val pushX  = note.position match
        case NotePosition.RightOf =>
          val far = others.map(n => n.center.x + n.width / 2).maxOption.getOrElse(sideX)
          Math.max(sideX, far + 15)
        case NotePosition.LeftOf =>
          val near = others.map(n => n.center.x - n.width / 2).minOption.getOrElse(sideX + noteW)
          Math.min(sideX, near - 15 - noteW)
      val candidates = List(
        NoteBox(node.center.x - noteW / 2, aboveY, noteW, noteH),
        NoteBox(sideX, aboveY, noteW, noteH),
        NoteBox(node.center.x - noteW / 2, belowY, noteW, noteH),
        NoteBox(sideX, belowY, noteW, noteH),
        NoteBox(pushX, midY, noteW, noteH),
        NoteBox(pushX, aboveY, noteW, noteH),
        NoteBox(pushX, belowY, noteW, noteH),
      )
      candidates.find(clear).getOrElse(candidates.head)
    end if
  end placeNote

  private def noteId(note: StateNote): String =
    note.alias.getOrElse(s"${note.stateId}-${note.noteIndex}")

  private def connector(node: LayoutNode, box: NoteBox): SvgNode =
    val nx               = node.center.x
    val ny               = node.center.y
    val cx               = box.x + box.w / 2
    val cy               = box.y + box.h / 2
    val (x1, y1, x2, y2) =
      if Math.abs(cx - nx) >= Math.abs(cy - ny) then
        if cx >= nx then (nx + node.width / 2, ny, box.x, cy)
        else (nx - node.width / 2, ny, box.right, cy)
      else if cy >= ny then (nx, ny + node.height / 2, cx, box.y)
      else (nx, ny - node.height / 2, cx, box.bottom)
    leaf("line")(
      "class" -> "note-connector",
      "x1"    -> x1.f,
      "y1"    -> y1.f,
      "x2"    -> x2.f,
      "y2"    -> y2.f,
    )
  end connector

  /** `None` when the note's state has no laid-out node to attach to. */
  def noteToSvg(
      config: RenderConfig,
      note: StateNote,
      nodeMap: Map[String, LayoutNode],
      selfLoopBottomExtents: Map[String, Double] = Map.empty,
  ): Option[SvgNode] =
    nodeMap.get(note.stateId).map { node =>
      val lc                  = config.layout
      val lines               = note.text.split("\n")
      val (noteW, noteH)      = noteSize(config, note)
      val box                 = placeNote(config, note, node, nodeMap.values, selfLoopBottomExtents)
      val (textAnchor, textX) = note.textAlign match
        case NoteTextAlign.Center => ("middle", box.x + noteW / 2)
        case NoteTextAlign.Right  => ("end", box.right - 10)
        case NoteTextAlign.Left   => ("start", box.x + 10)
      val lineHeight = lc.edgeLabelFontSize + 4
      val rect       = leaf("rect")(
        "class"  -> "note-rect",
        "x"      -> box.x.f,
        "y"      -> box.y.f,
        "width"  -> noteW.f,
        "height" -> noteH.f,
        "rx"     -> "3",
        "ry"     -> "3",
      )
      val textSvg = lines.toList.zipWithIndex.map { case (line, i) =>
        val ty = box.y + 10 + i * lineHeight + lineHeight / 2
        textElem("text")(
          "class"             -> "note-text",
          "x"                 -> textX.f,
          "y"                 -> ty.f,
          "text-anchor"       -> textAnchor,
          "dominant-baseline" -> "central",
        )(line)
      }
      SvgNode.Element(
        "g",
        List("class" -> "note", "id" -> s"note-${noteId(note)}"),
        connector(node, box) :: rect :: textSvg,
      )
    }
end NoteRenderer
