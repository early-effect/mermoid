package mermoid

import mermoid.SvgNode.{leaf, textElem}

object NoteRenderer:

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
    val labelBaseY       = (startY + apexY) / 2 - (lc.edgeLabelFontSize + 12) * 0.5
    val lastLabelBottomY =
      labelBaseY + (selfLoopCount - 1) * (lc.edgeLabelFontSize + 12) + (lc.edgeLabelFontSize + 8) / 2
    Math.max(apexY, lastLabelBottomY)
  end selfLoopBottomExtent

  private def noteId(note: StateNote): String =
    note.alias.getOrElse(s"${note.stateId}-${note.noteIndex}")

  /** `None` when the note's state has no laid-out node to attach to. */
  def noteToSvg(
      config: RenderConfig,
      note: StateNote,
      nodeMap: Map[String, LayoutNode],
      selfLoopBottomExtents: Map[String, Double] = Map.empty,
  ): Option[SvgNode] =
    nodeMap.get(note.stateId).map { node =>
      val lc           = config.layout
      val lines        = note.text.split("\n")
      val maxLineWidth = lines.map(l => SvgUtil.estimateTextWidth(l, lc)).maxOption.getOrElse(60.0)
      val noteW        = maxLineWidth + 20
      val lineHeight   = lc.edgeLabelFontSize + 4
      val noteH        = lines.length * lineHeight + 12
      val nx           = note.position match
        case NotePosition.RightOf => node.center.x + node.width / 2 + 15
        case NotePosition.LeftOf  => node.center.x - node.width / 2 - 15 - noteW
      val ny = selfLoopBottomExtents.get(note.stateId) match
        case Some(bottomY) => bottomY + 5
        case None          => node.center.y - noteH / 2
      val (textAnchor, textX) = note.textAlign match
        case NoteTextAlign.Center => ("middle", nx + noteW / 2)
        case NoteTextAlign.Right  => ("end", nx + noteW - 10)
        case NoteTextAlign.Left   => ("start", nx + 10)
      val noteMidY                         = ny + noteH / 2
      val (lineX1, lineY1, lineX2, lineY2) = note.position match
        case NotePosition.RightOf => (node.center.x + node.width / 2, node.center.y, nx, noteMidY)
        case NotePosition.LeftOf  => (node.center.x - node.width / 2, node.center.y, nx + noteW, noteMidY)
      val connector = leaf("line")(
        "class" -> "note-connector",
        "x1"    -> lineX1.f,
        "y1"    -> lineY1.f,
        "x2"    -> lineX2.f,
        "y2"    -> lineY2.f,
      )
      val rect = leaf("rect")(
        "class"  -> "note-rect",
        "x"      -> nx.f,
        "y"      -> ny.f,
        "width"  -> noteW.f,
        "height" -> noteH.f,
        "rx"     -> "3",
        "ry"     -> "3",
      )
      val textSvg = lines.toList.zipWithIndex.map { case (line, i) =>
        val ty = ny + 10 + i * lineHeight + lineHeight / 2
        textElem("text")(
          "class"             -> "note-text",
          "x"                 -> textX.f,
          "y"                 -> ty.f,
          "text-anchor"       -> textAnchor,
          "dominant-baseline" -> "central",
        )(line)
      }
      SvgNode.Element("g", List("class" -> "note", "id" -> s"note-${noteId(note)}"), connector :: rect :: textSvg)
    }
end NoteRenderer
