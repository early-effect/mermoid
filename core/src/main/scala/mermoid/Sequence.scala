package mermoid

/** `participant` is a rounded box. `actor` is a stick figure in the same column. */
enum ParticipantKind:
  case Participant
  case Actor

/** The icon at the head of a sequence column. `actor` is also [[ParticipantKind.Actor]]. */
enum SequenceStereotype:
  case Actor
  case Boundary
  case Control
  case Entity
  case Database
  case Collections
  case Queue

object SequenceStereotype:
  def parse(raw: String): Option[SequenceStereotype] = raw.trim.toLowerCase match
    case "actor"       => Some(SequenceStereotype.Actor)
    case "boundary"    => Some(SequenceStereotype.Boundary)
    case "control"     => Some(SequenceStereotype.Control)
    case "entity"      => Some(SequenceStereotype.Entity)
    case "database"    => Some(SequenceStereotype.Database)
    case "collections" => Some(SequenceStereotype.Collections)
    case "queue"       => Some(SequenceStereotype.Queue)
    case _             => None
end SequenceStereotype

enum SequenceLine:
  case Solid
  case Dashed

/** How a sequence arrow ends. Not a boolean: Mermaid has four distinct ends. */
enum SequenceHead:
  case None
  case Filled
  case Open
  case Cross

/** The ten arrows Mermaid sequence diagrams can write. Other combinations are not syntax. */
enum SequenceArrow:
  case Solid, Dashed
  case SolidHead, DashedHead
  case SolidBoth, DashedBoth
  case SolidCross, DashedCross
  case SolidOpen, DashedOpen

  def line: SequenceLine = this match
    case Solid | SolidHead | SolidBoth | SolidCross | SolidOpen      => SequenceLine.Solid
    case Dashed | DashedHead | DashedBoth | DashedCross | DashedOpen => SequenceLine.Dashed

  def head: SequenceHead = this match
    case Solid | Dashed                                  => SequenceHead.None
    case SolidHead | DashedHead | SolidBoth | DashedBoth => SequenceHead.Filled
    case SolidOpen | DashedOpen                          => SequenceHead.Open
    case SolidCross | DashedCross                        => SequenceHead.Cross

  def tail: SequenceHead = this match
    case SolidBoth | DashedBoth => SequenceHead.Filled
    case _                      => SequenceHead.None
end SequenceArrow

/** `+` / `-` after an arrow. `+` opens the target. `-` closes the sender. */
enum MessageControl:
  case None
  case Activate
  case Deactivate

enum NotePlace:
  case LeftOf(id: NodeId)
  case RightOf(id: NodeId)

  /** `Over(a, a)` is a note on one column. */
  case Over(from: NodeId, to: NodeId)

enum Numbering:
  case On(start: Int, step: Int)
  case Off

case class Rgb(r: Int, g: Int, b: Int, alpha: Option[Double]):
  def show: String = alpha match
    case None    => s"rgb($r, $g, $b)"
    case Some(a) => s"rgba($r, $g, $b, ${Num.format(a)})"

enum GroupKind:
  case Loop(label: String)
  case Opt(label: String)
  case Critical(label: String)
  case Break(label: String)
  case Alt
  case Par
  case Highlight(color: Rgb)

case class GroupSection(label: Option[String], body: List[SequenceStatement])

enum SequenceStatement:
  case Declare(
      id: NodeId,
      label: Option[String],
      kind: ParticipantKind,
      stereotype: Option[SequenceStereotype] = None,
  )

  /** A participant whose lifeline starts at this row, not at the header. */
  case Create(
      id: NodeId,
      label: Option[String],
      kind: ParticipantKind,
      stereotype: Option[SequenceStereotype] = None,
  )
  case Destroy(id: NodeId)
  case Box(title: Option[String], fill: String, body: List[SequenceStatement])
  case Message(
      from: NodeId,
      to: NodeId,
      arrow: SequenceArrow,
      text: Option[String],
      control: MessageControl,
  )
  case Activate(id: NodeId)
  case Deactivate(id: NodeId)
  case Note(place: NotePlace, text: String)
  case Autonumber(mode: Numbering)
  case Group(kind: GroupKind, sections: List[GroupSection])
  case Link(id: NodeId, pairs: List[(String, String)])
  case ClickSt(binding: ClickBinding)
  case StyleSt(id: NodeId, styles: Map[mermoid.css.CssProperty, String])
  case ClassDefSt(name: String, styles: Map[mermoid.css.CssProperty, String])
  case ClassSt(ids: List[NodeId], className: String)
  case AccTitle(text: String)
  case AccDescr(text: String)
  case BadStereotype(raw: String)
end SequenceStatement

/** Axis-aligned box. `x` and `y` are the top-left. */
case class Rect(x: Double, y: Double, w: Double, h: Double)

/** A self-message is four corners in order: leave the lifeline, turn down, turn back, tip. */
enum MessagePath:
  case Straight(from: Point, to: Point)
  case Hook(out: Point, down: Point, back: Point, head: Point)

case class PlacedParticipant(
    id: NodeId,
    label: String,
    kind: ParticipantKind,
    box: Rect,
    stereotype: Option[SequenceStereotype] = None,
    cssClasses: List[String] = Nil,
    styles: Map[mermoid.css.CssProperty, String] = Map.empty,
    links: List[(String, String)] = Nil,
    tooltip: Option[String] = None,
):
  def centerX: Double = box.x + box.w / 2
end PlacedParticipant

case class Lifeline(id: NodeId, x: Double, y0: Double, y1: Double, destroyed: Boolean = false)

case class PlacedMessage(
    index: Int,
    from: NodeId,
    to: NodeId,
    arrow: SequenceArrow,
    lines: List[String],
    number: Option[Int],
    path: MessagePath,
    labelAt: Point,
):
  def shown: List[String] = PlacedMessage.shown(lines, number)
end PlacedMessage

object PlacedMessage:
  /** The text paint draws. A number prefixes the first line, and it is the only line when the message has no text. */
  def shown(lines: List[String], number: Option[Int]): List[String] =
    number match
      case None    => lines
      case Some(n) =>
        lines match
          case Nil    => List(n.toString)
          case h :: t => s"$n $h" :: t

case class ActivationBar(id: NodeId, depth: Int, rect: Rect)

case class PlacedNote(index: Int, lines: List[String], box: Rect)

case class SectionDivider(label: String, y: Double, x0: Double, x1: Double, tabWidth: Double = 0)

case class PlacedGroup(
    index: Int,
    kind: GroupKind,
    tab: Option[String],
    frame: Rect,
    dividers: List[SectionDivider],
    /** The keyword tab in the top-left of the frame. Empty for a highlight. */
    tabBox: Option[Rect] = None,
)

/** A `box` band behind a run of participants. `rect` covers their columns. */
case class PlacedBand(title: Option[String], fill: String, rect: Rect)

/** Paint-ready sequence diagram. Columns are participants. Rows are time. */
case class SequenceScene(
    width: Double,
    height: Double,
    participants: List[PlacedParticipant],
    lifelines: List[Lifeline],
    messages: List[PlacedMessage],
    notes: List[PlacedNote],
    activations: List[ActivationBar],
    groups: List[PlacedGroup],
    config: RenderConfig,
    bands: List[PlacedBand] = Nil,
    accTitle: Option[String] = None,
    accDescr: Option[String] = None,
    classDefRules: List[mermoid.css.CssRule] = Nil,
)

/** A participant as layout sees it: explicit declare, or the first message that named the id. */
case class DeclaredParticipant(
    id: NodeId,
    label: String,
    kind: ParticipantKind,
    stereotype: Option[SequenceStereotype] = None,
)

object SequenceModel:

  def participantOrder(statements: List[SequenceStatement]): List[NodeId] =
    declarations(statements).map(_.id)

  /** First-seen order. A later declare for the same id does not move the column. */
  def declarations(statements: List[SequenceStatement]): List[DeclaredParticipant] =
    def walk(stmts: List[SequenceStatement], acc: List[DeclaredParticipant]): List[DeclaredParticipant] =
      stmts.foldLeft(acc)(see)

    def see(acc: List[DeclaredParticipant], stmt: SequenceStatement): List[DeclaredParticipant] =
      stmt match
        case SequenceStatement.Declare(id, label, kind, stereotype) =>
          if acc.exists(_.id == id) then acc
          else acc :+ DeclaredParticipant(id, label.getOrElse(id.value), kind, stereotype)
        case SequenceStatement.Create(id, label, kind, stereotype) =>
          if acc.exists(_.id == id) then acc
          else acc :+ DeclaredParticipant(id, label.getOrElse(id.value), kind, stereotype)
        case SequenceStatement.Destroy(id)                => seeId(acc, id)
        case SequenceStatement.Box(_, _, body)            => walk(body, acc)
        case SequenceStatement.Link(id, _)                => seeId(acc, id)
        case SequenceStatement.ClickSt(binding)           => seeId(acc, binding.nodeId)
        case SequenceStatement.StyleSt(id, _)             => seeId(acc, id)
        case SequenceStatement.ClassSt(ids, _)            => ids.foldLeft(acc)(seeId)
        case SequenceStatement.ClassDefSt(_, _)           => acc
        case SequenceStatement.AccTitle(_)                => acc
        case SequenceStatement.AccDescr(_)                => acc
        case SequenceStatement.BadStereotype(_)           => acc
        case SequenceStatement.Message(from, to, _, _, _) =>
          seeId(seeId(acc, from), to)
        case SequenceStatement.Activate(id)   => seeId(acc, id)
        case SequenceStatement.Deactivate(id) => seeId(acc, id)
        case SequenceStatement.Note(place, _) =>
          place match
            case NotePlace.LeftOf(id)     => seeId(acc, id)
            case NotePlace.RightOf(id)    => seeId(acc, id)
            case NotePlace.Over(from, to) => seeId(seeId(acc, from), to)
        case SequenceStatement.Autonumber(_)      => acc
        case SequenceStatement.Group(_, sections) =>
          sections.foldLeft(acc)((a, section) => walk(section.body, a))

    def seeId(acc: List[DeclaredParticipant], id: NodeId): List[DeclaredParticipant] =
      if acc.exists(_.id == id) then acc
      else acc :+ DeclaredParticipant(id, id.value, ParticipantKind.Participant)

    walk(statements, Nil)
  end declarations

  /** Alias and color checks. A parsed [[Diagram.Sequence]] has already passed. */
  def resolve(statements: List[SequenceStatement]): Either[ParseError, List[SequenceStatement]] =
    stereotypes(statements)
      .flatMap(_ => aliases(statements, Map.empty))
      .flatMap(_ => colors(statements))
      .map(_ => statements)

  private def stereotypes(stmts: List[SequenceStatement]): Either[ParseError, Unit] =
    stmts.foldLeft[Either[ParseError, Unit]](Right(())) { (acc, stmt) =>
      acc.flatMap { _ =>
        stmt match
          case SequenceStatement.BadStereotype(raw) => Left(ParseError.UnknownStereotype(raw))
          case SequenceStatement.Box(_, _, body)    => stereotypes(body)
          case SequenceStatement.Group(_, sections) => stereotypes(sections.flatMap(_.body))
          case _                                    => Right(())
      }
    }

  private case class Alias(label: String, kind: ParticipantKind)

  private def aliases(
      stmts: List[SequenceStatement],
      seen: Map[NodeId, Alias],
  ): Either[ParseError, Map[NodeId, Alias]] =
    stmts.foldLeft[Either[ParseError, Map[NodeId, Alias]]](Right(seen)) { (acc, stmt) =>
      acc.flatMap(seen => oneAlias(stmt, seen))
    }

  private def oneAlias(
      stmt: SequenceStatement,
      seen: Map[NodeId, Alias],
  ): Either[ParseError, Map[NodeId, Alias]] =
    stmt match
      case SequenceStatement.Declare(id, label, kind, _) =>
        noteAlias(seen, id, label, kind)
      case SequenceStatement.Create(id, label, kind, _) =>
        noteAlias(seen, id, label, kind)
      case SequenceStatement.Box(_, _, body)    => aliases(body, seen)
      case SequenceStatement.Group(_, sections) =>
        sections.foldLeft[Either[ParseError, Map[NodeId, Alias]]](Right(seen)) { (acc, section) =>
          acc.flatMap(seen => aliases(section.body, seen))
        }
      case _ => Right(seen)

  private def noteAlias(
      seen: Map[NodeId, Alias],
      id: NodeId,
      label: Option[String],
      kind: ParticipantKind,
  ): Either[ParseError, Map[NodeId, Alias]] =
    seen.get(id) match
      case None =>
        Right(seen.updated(id, Alias(label.getOrElse(id.value), kind)))
      case Some(prev) =>
        val next = label.getOrElse(prev.label)
        if next != prev.label then Left(ParseError.ConflictingAlias(id, prev.label, next))
        else if kind != prev.kind then Left(ParseError.ConflictingAlias(id, kindWord(prev.kind), kindWord(kind)))
        else Right(seen)

  private def kindWord(kind: ParticipantKind): String = kind match
    case ParticipantKind.Participant => "a participant"
    case ParticipantKind.Actor       => "an actor"

  private def colors(stmts: List[SequenceStatement]): Either[ParseError, Unit] =
    stmts.foldLeft[Either[ParseError, Unit]](Right(())) { (acc, stmt) =>
      acc.flatMap(_ => colorOne(stmt))
    }

  private def colorOne(stmt: SequenceStatement): Either[ParseError, Unit] =
    stmt match
      case SequenceStatement.Group(GroupKind.Highlight(rgb), sections) =>
        checkColor(rgb).flatMap(_ => colors(sections.flatMap(_.body)))
      case SequenceStatement.Group(_, sections) =>
        colors(sections.flatMap(_.body))
      case SequenceStatement.Box(_, _, body) => colors(body)
      case _                                 => Right(())

  private def checkColor(rgb: Rgb): Either[ParseError, Unit] =
    val badChannel = rgb.r < 0 || rgb.r > 255 || rgb.g < 0 || rgb.g > 255 || rgb.b < 0 || rgb.b > 255
    val badAlpha   = rgb.alpha.exists(a => a < 0.0 || a > 1.0)
    if badChannel || badAlpha then Left(ParseError.BadColor(rgb.show)) else Right(())

  /** Ids whose first mention is `create`. Their box sits on that row. */
  def createdIds(statements: List[SequenceStatement]): Set[NodeId] =
    def walk(stmts: List[SequenceStatement], seen: Set[NodeId], created: Set[NodeId]): (Set[NodeId], Set[NodeId]) =
      stmts.foldLeft((seen, created)) { case ((seen, created), stmt) =>
        stmt match
          case SequenceStatement.Create(id, _, _, _) =>
            if seen.contains(id) then (seen + id, created) else (seen + id, created + id)
          case SequenceStatement.Declare(id, _, _, _)       => (seen + id, created)
          case SequenceStatement.Destroy(id)                => (seen + id, created)
          case SequenceStatement.Link(id, _)                => (seen + id, created)
          case SequenceStatement.ClickSt(binding)           => (seen + binding.nodeId, created)
          case SequenceStatement.StyleSt(id, _)             => (seen + id, created)
          case SequenceStatement.ClassSt(ids, _)            => (seen ++ ids, created)
          case SequenceStatement.Activate(id)               => (seen + id, created)
          case SequenceStatement.Deactivate(id)             => (seen + id, created)
          case SequenceStatement.Message(from, to, _, _, _) =>
            (seen + from + to, created)
          case SequenceStatement.Note(place, _) =>
            place match
              case NotePlace.LeftOf(id)     => (seen + id, created)
              case NotePlace.RightOf(id)    => (seen + id, created)
              case NotePlace.Over(from, to) => (seen + from + to, created)
          case SequenceStatement.Box(_, _, body)    => walk(body, seen, created)
          case SequenceStatement.Group(_, sections) =>
            sections.foldLeft((seen, created)) { case (acc, section) => walk(section.body, acc._1, acc._2) }
          case SequenceStatement.Autonumber(_) | SequenceStatement.ClassDefSt(_, _) | SequenceStatement.AccTitle(_) |
              SequenceStatement.AccDescr(_) | SequenceStatement.BadStereotype(_) =>
            (seen, created)
      }
    walk(statements, Set.empty, Set.empty)._2
  end createdIds

  def access(statements: List[SequenceStatement]): (Option[String], Option[String]) =
    def walk(
        stmts: List[SequenceStatement],
        title: Option[String],
        descr: Option[String],
    ): (Option[String], Option[String]) =
      stmts.foldLeft((title, descr)) { case ((title, descr), stmt) =>
        stmt match
          case SequenceStatement.AccTitle(text)     => (Some(text), descr)
          case SequenceStatement.AccDescr(text)     => (title, Some(text))
          case SequenceStatement.Box(_, _, body)    => walk(body, title, descr)
          case SequenceStatement.Group(_, sections) =>
            sections.foldLeft((title, descr)) { case ((t, d), section) => walk(section.body, t, d) }
          case _ => (title, descr)
      }
    walk(statements, None, None)
  end access

  def classRules(statements: List[SequenceStatement]): List[mermoid.css.CssRule] =
    classDefs(statements).toList.flatMap { (name, styles) => StyleResolver.classDefRulesFor(name, styles) }

  def chrome(statements: List[SequenceStatement]): Map[NodeId, ParticipantChrome] =
    paintChrome(statements, classDefs(statements), Map.empty)

  private def classDefs(
      stmts: List[SequenceStatement]
  ): Map[String, Map[mermoid.css.CssProperty, String]] =
    def walk(
        stmts: List[SequenceStatement],
        acc: Map[String, Map[mermoid.css.CssProperty, String]],
    ): Map[String, Map[mermoid.css.CssProperty, String]] =
      stmts.foldLeft(acc) { (acc, stmt) =>
        stmt match
          case SequenceStatement.ClassDefSt(name, styles) =>
            acc.updated(name, acc.getOrElse(name, Map.empty) ++ styles)
          case SequenceStatement.Box(_, _, body)    => walk(body, acc)
          case SequenceStatement.Group(_, sections) =>
            sections.foldLeft(acc)((a, section) => walk(section.body, a))
          case _ => acc
      }
    walk(stmts, Map.empty)
  end classDefs

  private def paintChrome(
      stmts: List[SequenceStatement],
      defs: Map[String, Map[mermoid.css.CssProperty, String]],
      acc: Map[NodeId, ParticipantChrome],
  ): Map[NodeId, ParticipantChrome] =
    stmts.foldLeft(acc) { (acc, stmt) =>
      stmt match
        case SequenceStatement.Declare(id, _, _, stereo) => withStereo(acc, id, stereo)
        case SequenceStatement.Create(id, _, _, stereo)  => withStereo(acc, id, stereo)
        case SequenceStatement.StyleSt(id, styles)       =>
          val cur = acc.getOrElse(id, ParticipantChrome.empty)
          acc.updated(id, cur.copy(styles = cur.styles ++ styles))
        case SequenceStatement.ClassSt(ids, name) =>
          val extra = defs.getOrElse(name, Map.empty)
          ids.foldLeft(acc) { (acc, id) =>
            val cur     = acc.getOrElse(id, ParticipantChrome.empty)
            val classes = if cur.cssClasses.contains(name) then cur.cssClasses else cur.cssClasses :+ name
            acc.updated(id, cur.copy(cssClasses = classes, styles = cur.styles ++ extra))
          }
        case SequenceStatement.Link(id, pairs) =>
          val cur = acc.getOrElse(id, ParticipantChrome.empty)
          acc.updated(id, cur.copy(links = cur.links ++ pairs))
        case SequenceStatement.ClickSt(binding) =>
          val cur   = acc.getOrElse(binding.nodeId, ParticipantChrome.empty)
          val links = binding.href match
            case Some(href) => cur.links :+ (binding.tooltip.getOrElse(href) -> href)
            case None       => cur.links
          acc.updated(
            binding.nodeId,
            cur.copy(links = links, tooltip = binding.tooltip.orElse(cur.tooltip)),
          )
        case SequenceStatement.Box(_, _, body)    => paintChrome(body, defs, acc)
        case SequenceStatement.Group(_, sections) =>
          sections.foldLeft(acc)((a, section) => paintChrome(section.body, defs, a))
        case _ => acc
    }
  end paintChrome

  private def withStereo(
      acc: Map[NodeId, ParticipantChrome],
      id: NodeId,
      stereo: Option[SequenceStereotype],
  ): Map[NodeId, ParticipantChrome] =
    stereo match
      case None        => acc
      case Some(value) =>
        val cur = acc.getOrElse(id, ParticipantChrome.empty)
        acc.updated(id, cur.copy(stereotype = Some(value)))
end SequenceModel

/** Classes, inline paint, and links gathered for one participant column. */
case class ParticipantChrome(
    stereotype: Option[SequenceStereotype] = None,
    cssClasses: List[String] = Nil,
    styles: Map[mermoid.css.CssProperty, String] = Map.empty,
    links: List[(String, String)] = Nil,
    tooltip: Option[String] = None,
)

object ParticipantChrome:
  val empty: ParticipantChrome = ParticipantChrome()
