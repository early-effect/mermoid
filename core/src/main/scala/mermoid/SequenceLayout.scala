package mermoid

import scala.annotation.tailrec

/** Stick-figure header. Layout and paint share it so the lifeline starts under the label. */
private[mermoid] object ActorMetrics:
  val headRadius: Double = 8.0
  val stem: Double       = 18.0
  val gap: Double        = 4.0

  def height(lineHeight: Double): Double =
    headRadius * 2 + gap + stem + gap + lineHeight

/** Columns are participants in source order. Rows are statements in source order. No ranker. */
private[mermoid] object SequenceLayout:

  private val lineBreak = "(?i)<br\\s*/?>".r

  def place(
      statements: List[SequenceStatement],
      config: RenderConfig,
      viewport: Option[Viewport],
  ): SequenceScene =
    val declared = SequenceModel.declarations(statements)
    val seq      = config.sequence
    val lc       = config.layout
    val measured = measure(statements, declared, lc.charWidthEstimate)
    val natural  = columns(declared, measured, seq, lc, gapScale = 1.0)
    val scale    = spacingScale(natural.contentRight + lc.padding * 2, config, viewport)
    val laid     = if scale == 1.0 then natural else columns(declared, measured, seq, lc, gapScale = scale)
    val pitch    = seq.rowPitch * scale
    val env      = Env(laid, seq, lc, pitch)
    val cursor   = placeStatements(statements, Cursor.start(env.headerBand + seq.headerGap), env)
    finish(cursor, env, config)
  end place

  private def linesOf(text: Option[String]): List[String] =
    text match
      case None      => Nil
      case Some(raw) =>
        val parts = lineBreak.split(raw).toList.map(_.trim)
        if parts.forall(_.isEmpty) then Nil else parts

  private def textWidth(lines: List[String], charWidth: Double): Double =
    lines.map(_.length * charWidth).maxOption.getOrElse(0.0)

  private case class NumState(on: Boolean, next: Int, step: Int):
    def applyMode(mode: Numbering): NumState = mode match
      case Numbering.Off             => copy(on = false)
      case Numbering.On(start, step) =>
        NumState(on = true, next = start, step = if step == 0 then 1 else step)

    def take: (Option[Int], NumState) =
      if on then (Some(next), copy(next = next + step)) else (None, this)

  private object NumState:
    val initial: NumState = NumState(on = false, next = 1, step = 1)

  private case class SpanNeed(i: Int, j: Int, width: Double)
  private case class SelfNeed(index: Int, width: Double)
  private case class Measured(spans: List[SpanNeed], selves: List[SelfNeed])

  private def measure(
      statements: List[SequenceStatement],
      declared: List[DeclaredParticipant],
      charWidth: Double,
  ): Measured =
    val indexOf: Map[NodeId, Int] =
      declared.zipWithIndex.map((d, i) => d.id -> i).toMap

    def walk(stmts: List[SequenceStatement], state: NumState, acc: Measured): (NumState, Measured) =
      stmts.foldLeft((state, acc)) { case ((st, measured), stmt) =>
        stmt match
          case SequenceStatement.Autonumber(mode)              => (st.applyMode(mode), measured)
          case SequenceStatement.Message(from, to, _, text, _) =>
            val (number, next) = st.take
            val width          = textWidth(PlacedMessage.shown(linesOf(text), number), charWidth)
            (indexOf.get(from), indexOf.get(to)) match
              case (Some(i), Some(j)) if i == j =>
                (next, measured.copy(selves = SelfNeed(i, width) :: measured.selves))
              case (Some(i), Some(j)) =>
                val need = SpanNeed(math.min(i, j), math.max(i, j), width + 16)
                (next, measured.copy(spans = need :: measured.spans))
              case _ => (next, measured)
          case SequenceStatement.Group(_, sections) =>
            sections.foldLeft((st, measured)) { case ((s, m), section) => walk(section.body, s, m) }
          case _ => (st, measured)
      }

    val (_, measured) = walk(statements, NumState.initial, Measured(Nil, Nil))
    measured.copy(spans = measured.spans.reverse, selves = measured.selves.reverse)
  end measure

  private case class Column(
      id: NodeId,
      label: String,
      kind: ParticipantKind,
      boxW: Double,
      boxH: Double,
      center: Double,
      boxLeft: Double,
      boxTop: Double,
  )

  private case class ColumnLayout(columns: Vector[Column], contentRight: Double, headerBand: Double)

  private def kindHeight(kind: ParticipantKind, seq: SequenceConfig, lineHeight: Double): Double =
    kind match
      case ParticipantKind.Participant => seq.actorHeight
      case ParticipantKind.Actor       => ActorMetrics.height(lineHeight)

  private def columns(
      declared: List[DeclaredParticipant],
      measured: Measured,
      seq: SequenceConfig,
      lc: LayoutConfig,
      gapScale: Double,
  ): ColumnLayout =
    val charW = lc.charWidthEstimate
    val boxWs = declared.map { d =>
      math.max(seq.actorMinWidth, d.label.length * charW + seq.actorPadH * 2)
    }.toVector
    val n      = boxWs.length
    val selfAt = measured.selves.groupBy(_.index).view.mapValues(_.map(_.width).maxOption.getOrElse(0.0)).toMap
    val minGap = seq.columnGap * gapScale

    def overhang(index: Int): Double =
      selfAt.get(index) match
        case None         => 0.0
        case Some(labelW) =>
          val pastBox = seq.selfHookWidth + 8 + labelW - boxWs.lift(index).getOrElse(0.0) / 2
          math.max(seq.selfHookWidth, pastBox)

    val initGaps = Vector.tabulate(math.max(0, n - 1)) { i => math.max(minGap, overhang(i)) }
    // Shortest spans first, so an adjacent label settles before a wider span adds what is still missing.
    val ordered = measured.spans.sortBy(s => (s.j - s.i, s.i))
    val gaps    = solveGaps(initGaps, ordered, boxWs, ordered.size + 2)
    val centers = centersOf(boxWs, gaps)
    val extra   = if n == 0 then 0.0 else overhang(n - 1)
    val right   = centers.lastOption.zip(boxWs.lastOption).map((c, w) => c + w / 2 + extra).getOrElse(0.0)
    val heights = declared.map(d => kindHeight(d.kind, seq, lc.lineHeight))
    val band    = heights.maxOption.getOrElse(0.0)
    val cols    = declared
      .lazyZip(boxWs)
      .lazyZip(heights)
      .lazyZip(centers)
      .map { (d, w, h, cx) =>
        Column(d.id, d.label, d.kind, w, h, cx, cx - w / 2, band - h)
      }
      .toVector
    ColumnLayout(cols, right, band)
  end columns

  private def centersOf(boxWs: Vector[Double], gaps: Vector[Double]): Vector[Double] =
    boxWs.zipWithIndex
      .foldLeft((Vector.empty[Double], 0.0)) { case ((acc, x), (w, i)) =>
        val gap = gaps.lift(i).getOrElse(0.0)
        (acc :+ (x + w / 2), x + w + gap)
      }
      ._1

  /** Center distance between columns `i` and `j` (`i < j`). */
  private def spanOf(gaps: Vector[Double], boxWs: Vector[Double], i: Int, j: Int): Double =
    boxWs.slice(i, j).lazyZip(gaps.slice(i, j)).lazyZip(boxWs.slice(i + 1, j + 1)).foldLeft(0.0) {
      case (acc, (left, gap, right)) => acc + left / 2 + gap + right / 2
    }

  @tailrec
  private def solveGaps(
      gaps: Vector[Double],
      needs: List[SpanNeed],
      boxWs: Vector[Double],
      guard: Int,
  ): Vector[Double] =
    if guard <= 0 then gaps
    else
      val next = needs.foldLeft(gaps) { (g, need) =>
        val have    = spanOf(g, boxWs, need.i, need.j)
        val deficit = need.width - have
        val count   = need.j - need.i
        if deficit <= 0.01 || count <= 0 then g
        else
          val add = deficit / count
          g.zipWithIndex.map { (gap, idx) => if idx >= need.i && idx < need.j then gap + add else gap }
      }
      if next == gaps then gaps else solveGaps(next, needs, boxWs, guard - 1)

  private def spacingScale(naturalWidth: Double, config: RenderConfig, viewport: Option[Viewport]): Double =
    if !config.responsive.compressSpacing then 1.0
    else
      viewport match
        case None     => 1.0
        case Some(vp) =>
          if naturalWidth <= 0 then 1.0
          else
            val raw     = vp.maxWidth / naturalWidth
            val clamped =
              math.max(config.responsive.minSpacingScale, math.min(config.responsive.maxSpacingScale, raw))
            if math.abs(clamped - 1.0) < 0.04 then 1.0 else clamped

  private case class Env(
      layout: ColumnLayout,
      seq: SequenceConfig,
      lc: LayoutConfig,
      rowPitch: Double,
  ):
    val byId: Map[NodeId, Column] = layout.columns.map(c => c.id -> c).toMap
    def headerBand: Double        = layout.headerBand
    def lineH: Double             = lc.lineHeight
    def frameLeft: Double         = layout.columns.headOption.map(_.boxLeft - 8).getOrElse(0.0)
    def frameRight: Double        =
      layout.columns.lastOption.map(c => c.boxLeft + c.boxW + 8).getOrElse(180.0) max (frameLeft + 120)
  end Env

  private case class OpenBar(id: NodeId, depth: Int, y0: Double)

  private case class Cursor(
      y: Double,
      open: List[OpenBar],
      numbering: NumState,
      msgIndex: Int,
      noteIndex: Int,
      groupIndex: Int,
      messages: List[PlacedMessage],
      notes: List[PlacedNote],
      groups: List[PlacedGroup],
      bars: List[ActivationBar],
  )

  private object Cursor:
    def start(y: Double): Cursor =
      Cursor(y, Nil, NumState.initial, 0, 0, 0, Nil, Nil, Nil, Nil)

  private def placeStatements(stmts: List[SequenceStatement], cursor: Cursor, env: Env): Cursor =
    stmts.foldLeft(cursor)((c, stmt) => placeOne(c, stmt, env))

  private def placeOne(cursor: Cursor, stmt: SequenceStatement, env: Env): Cursor =
    stmt match
      case SequenceStatement.Autonumber(mode) => cursor.copy(numbering = cursor.numbering.applyMode(mode))
      case SequenceStatement.Declare(_, _, _) => cursor
      case SequenceStatement.Activate(id)     =>
        cursor.copy(open = pushBar(cursor.open, id, cursor.y))
      case SequenceStatement.Deactivate(id) =>
        val (closed, open) = popBar(cursor.open, id, cursor.y, env)
        cursor.copy(open = open, bars = cursor.bars ++ closed)
      case SequenceStatement.Message(from, to, arrow, text, control) =>
        placeMessage(cursor, from, to, arrow, text, control, env)
      case SequenceStatement.Note(place, text) =>
        placeNote(cursor, place, text, env)
      case SequenceStatement.Group(kind, sections) =>
        placeGroup(cursor, kind, sections, env)

  private def pushBar(open: List[OpenBar], id: NodeId, y0: Double): List[OpenBar] =
    OpenBar(id, open.count(_.id == id), y0) :: open

  private def popBar(
      open: List[OpenBar],
      id: NodeId,
      y1: Double,
      env: Env,
  ): (List[ActivationBar], List[OpenBar]) =
    def go(rest: List[OpenBar], kept: List[OpenBar]): (Option[OpenBar], List[OpenBar]) =
      rest match
        case Nil                         => (None, kept.reverse)
        case bar :: tail if bar.id == id => (Some(bar), kept.reverse ++ tail)
        case bar :: tail                 => go(tail, bar :: kept)
    val (found, rest) = go(open, Nil)
    found match
      case None      => (Nil, open)
      case Some(bar) => (List(barOf(bar, y1, env)), rest)
  end popBar

  private def barOf(bar: OpenBar, y1: Double, env: Env): ActivationBar =
    val col = env.byId.get(bar.id)
    val x   = col.map(_.center).getOrElse(0.0) - env.seq.activationWidth / 2 + bar.depth * (env.seq.activationWidth / 2)
    val top = math.min(bar.y0, y1)
    val bot = math.max(bar.y0, y1)
    ActivationBar(bar.id, bar.depth, Rect(x, top, env.seq.activationWidth, bot - top))

  private def placeMessage(
      cursor: Cursor,
      from: NodeId,
      to: NodeId,
      arrow: SequenceArrow,
      text: Option[String],
      control: MessageControl,
      env: Env,
  ): Cursor =
    (env.byId.get(from), env.byId.get(to)) match
      case (Some(src), Some(dst)) =>
        val (number, numbering) = cursor.numbering.take
        val raw                 = linesOf(text)
        val shown               = PlacedMessage.shown(raw, number)
        val labelH              = if shown.isEmpty then 0.0 else shown.size * env.lineH
        val self                = src.id == dst.id
        val drop                = if self then env.seq.selfHookDrop else 0.0
        val rowH                = math.max(env.rowPitch, labelH + 8 + drop)
        val shaftY              = cursor.y + (if shown.isEmpty then env.rowPitch * 0.45 else labelH + 4)
        val path                =
          if self then hook(src.center, shaftY, env.seq)
          else MessagePath.Straight(Point(src.center, shaftY), Point(dst.center, shaftY))
        val labelAt =
          if shown.isEmpty then Point((src.center + dst.center) / 2, shaftY)
          else if self then
            val y = shaftY + env.seq.selfHookDrop / 2 - (shown.size - 1) * env.lineH / 2
            Point(src.center + env.seq.selfHookWidth + 8, y)
          else
            val y = shaftY - 4 - (shown.size - 1) * env.lineH
            Point((src.center + dst.center) / 2, y)
        val message          = PlacedMessage(cursor.msgIndex, from, to, arrow, raw, number, path, labelAt)
        val (closed, pushed) = control match
          case MessageControl.None       => (Nil, cursor.open)
          case MessageControl.Activate   => (Nil, pushBar(cursor.open, to, shaftY))
          case MessageControl.Deactivate => popBar(cursor.open, from, shaftY, env)
        cursor.copy(
          y = cursor.y + rowH,
          open = pushed,
          numbering = numbering,
          msgIndex = cursor.msgIndex + 1,
          messages = cursor.messages :+ message,
          bars = cursor.bars ++ closed,
        )
      case _ => cursor

  private def hook(cx: Double, shaftY: Double, seq: SequenceConfig): MessagePath =
    MessagePath.Hook(
      out = Point(cx, shaftY),
      down = Point(cx + seq.selfHookWidth, shaftY),
      back = Point(cx + seq.selfHookWidth, shaftY + seq.selfHookDrop),
      head = Point(cx, shaftY + seq.selfHookDrop),
    )

  private def placeNote(cursor: Cursor, place: NotePlace, text: String, env: Env): Cursor =
    val lines  = linesOf(Some(text))
    val width  = math.max(48.0, textWidth(lines, env.lc.charWidthEstimate) + 16)
    val height = math.max(lines.size, 1) * env.lineH + 12
    val box    = place match
      case NotePlace.LeftOf(id) =>
        env.byId.get(id).map(col => Rect(col.center - 8 - width, cursor.y, width, height))
      case NotePlace.RightOf(id) =>
        env.byId.get(id).map(col => Rect(col.center + 8, cursor.y, width, height))
      case NotePlace.Over(from, to) =>
        (env.byId.get(from), env.byId.get(to)) match
          case (Some(a), Some(b)) =>
            val left  = math.min(a.boxLeft, b.boxLeft)
            val right = math.max(a.boxLeft + a.boxW, b.boxLeft + b.boxW)
            val span  = right - left
            val w     = math.max(span, width)
            Some(Rect(left + (span - w) / 2, cursor.y, w, height))
          case _ => None
    box match
      case None       => cursor
      case Some(rect) =>
        cursor.copy(
          y = cursor.y + rect.h + 8,
          noteIndex = cursor.noteIndex + 1,
          notes = cursor.notes :+ PlacedNote(cursor.noteIndex, lines, rect),
        )
  end placeNote

  private def placeGroup(
      cursor: Cursor,
      kind: GroupKind,
      sections: List[GroupSection],
      env: Env,
  ): Cursor =
    val y0               = cursor.y
    val first            = sections.headOption.flatMap(_.label).getOrElse("")
    val tab              = tabOf(kind, first)
    val tabH             = if tab.isEmpty then 6.0 else env.lineH + 8
    val insertAt         = cursor.groups.size
    val (dividers, body) = sections match
      case Nil          => (Nil, cursor.copy(y = y0 + tabH))
      case head :: tail =>
        val started = placeStatements(head.body, cursor.copy(y = y0 + tabH), env)
        tail.foldLeft((List.empty[SectionDivider], started)) { case ((divs, c), section) =>
          val label = titled(dividerWord(kind), section.label.getOrElse(""))
          val div   = SectionDivider(label, c.y + env.lineH, env.frameLeft, env.frameRight)
          val next  = placeStatements(section.body, c.copy(y = c.y + env.lineH + 8), env)
          (divs :+ div, next)
        }
    val frame = Rect(env.frameLeft, y0, env.frameRight - env.frameLeft, body.y + 6 - y0)
    val group = PlacedGroup(cursor.groupIndex, kind, tab, frame, dividers)
    body.copy(
      y = body.y + 8,
      groupIndex = cursor.groupIndex + 1,
      groups = body.groups.patch(insertAt, List(group), 0),
    )
  end placeGroup

  private def titled(word: String, label: String): String =
    if label.isEmpty then word else s"$word $label"

  private def tabOf(kind: GroupKind, firstLabel: String): Option[String] = kind match
    case GroupKind.Loop(label)     => Some(titled("loop", label))
    case GroupKind.Opt(label)      => Some(titled("opt", label))
    case GroupKind.Critical(label) => Some(titled("critical", label))
    case GroupKind.Break(label)    => Some(titled("break", label))
    case GroupKind.Alt             => Some(titled("alt", firstLabel))
    case GroupKind.Par             => Some(titled("par", firstLabel))
    case GroupKind.Highlight(_)    => None

  private def dividerWord(kind: GroupKind): String = kind match
    case GroupKind.Par => "and"
    case _             => "else"

  private case class Bounds(minX: Double, minY: Double, maxX: Double, maxY: Double):
    def addX(x: Double): Bounds           = copy(minX = math.min(minX, x), maxX = math.max(maxX, x))
    def addY(y: Double): Bounds           = copy(minY = math.min(minY, y), maxY = math.max(maxY, y))
    def add(x: Double, y: Double): Bounds = addX(x).addY(y)
    def addRect(r: Rect): Bounds          = add(r.x, r.y).add(r.x + r.w, r.y + r.h)

  private def finish(cursor: Cursor, env: Env, config: RenderConfig): SequenceScene =
    val endY      = cursor.y + env.seq.footerGap
    val flushed   = cursor.open.map(bar => barOf(bar, endY, env))
    val bars      = cursor.bars ++ flushed
    val lifelines =
      if env.layout.columns.isEmpty then Nil
      else
        env.layout.columns.map { col =>
          Lifeline(col.id, col.center, env.headerBand, math.max(endY, env.headerBand))
        }.toList
    val participants = env.layout.columns.map { col =>
      PlacedParticipant(col.id, col.label, col.kind, Rect(col.boxLeft, col.boxTop, col.boxW, col.boxH))
    }.toList
    val charW = env.lc.charWidthEstimate
    varBounds(participants, lifelines, cursor, bars, env, charW, endY) match
      case bounds =>
        val pad = env.lc.padding
        val dx  = pad - bounds.minX
        val dy  = pad - bounds.minY
        SequenceScene(
          width = bounds.maxX + dx + pad,
          height = bounds.maxY + dy + pad,
          participants = participants.map(p => p.copy(box = shiftRect(p.box, dx, dy))),
          lifelines = lifelines.map(l => l.copy(x = l.x + dx, y0 = l.y0 + dy, y1 = l.y1 + dy)),
          messages = cursor.messages.map(m => shiftMessage(m, dx, dy)),
          notes = cursor.notes.map(n => n.copy(box = shiftRect(n.box, dx, dy))),
          activations = bars.map(b => b.copy(rect = shiftRect(b.rect, dx, dy))),
          groups = cursor.groups.map(g => shiftGroup(g, dx, dy)),
          config = config,
        )
    end match
  end finish

  private def varBounds(
      participants: List[PlacedParticipant],
      lifelines: List[Lifeline],
      cursor: Cursor,
      bars: List[ActivationBar],
      env: Env,
      charW: Double,
      endY: Double,
  ): Bounds =
    val base       = Bounds(0, 0, math.max(env.layout.contentRight, env.frameRight), math.max(endY, env.headerBand))
    val withPeople = participants.foldLeft(base)((b, p) => b.addRect(p.box))
    val withLines  = lifelines.foldLeft(withPeople)((b, l) => b.add(l.x, l.y0).add(l.x, l.y1))
    val withMsg    = cursor.messages.foldLeft(withLines) { (b, m) =>
      val geom = m.path match
        case MessagePath.Straight(from, to)          => b.add(from.x, from.y).add(to.x, to.y)
        case MessagePath.Hook(out, down, back, head) =>
          b.add(out.x, out.y).add(down.x, down.y).add(back.x, back.y).add(head.x, head.y)
      val shown = m.shown
      if shown.isEmpty then geom
      else
        val middle = m.path match
          case MessagePath.Straight(_, _)   => true
          case MessagePath.Hook(_, _, _, _) => false
        geom.addRect(labelBox(m.labelAt, shown, middle, charW, env.lineH))
    }
    val withNotes = cursor.notes.foldLeft(withMsg)((b, n) => b.addRect(n.box))
    val withBars  = bars.foldLeft(withNotes)((b, bar) => b.addRect(bar.rect))
    cursor.groups.foldLeft(withBars)((b, g) => b.addRect(g.frame))
  end varBounds

  private def labelBox(at: Point, lines: List[String], middle: Boolean, charW: Double, lineH: Double): Rect =
    val w = textWidth(lines, charW)
    val h = math.max(lines.size, 1) * lineH
    val x = if middle then at.x - w / 2 else at.x
    Rect(x, at.y - lineH * 0.85, w, h)

  private def shiftRect(r: Rect, dx: Double, dy: Double): Rect =
    r.copy(x = r.x + dx, y = r.y + dy)

  private def shiftPoint(p: Point, dx: Double, dy: Double): Point =
    Point(p.x + dx, p.y + dy)

  private def shiftMessage(m: PlacedMessage, dx: Double, dy: Double): PlacedMessage =
    val path = m.path match
      case MessagePath.Straight(from, to) =>
        MessagePath.Straight(shiftPoint(from, dx, dy), shiftPoint(to, dx, dy))
      case MessagePath.Hook(out, down, back, head) =>
        MessagePath.Hook(
          shiftPoint(out, dx, dy),
          shiftPoint(down, dx, dy),
          shiftPoint(back, dx, dy),
          shiftPoint(head, dx, dy),
        )
    m.copy(path = path, labelAt = shiftPoint(m.labelAt, dx, dy))
  end shiftMessage

  private def shiftGroup(g: PlacedGroup, dx: Double, dy: Double): PlacedGroup =
    g.copy(
      frame = shiftRect(g.frame, dx, dy),
      dividers = g.dividers.map(d => d.copy(y = d.y + dy, x0 = d.x0 + dx, x1 = d.x1 + dx)),
    )
end SequenceLayout
