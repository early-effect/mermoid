package mermoid

/** `participant` is a rounded box. `actor` is a stick figure in the same column. */
enum ParticipantKind:
  case Participant
  case Actor

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
  case Declare(id: NodeId, label: Option[String], kind: ParticipantKind)
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
end SequenceStatement

/** Axis-aligned box. `x` and `y` are the top-left. */
case class Rect(x: Double, y: Double, w: Double, h: Double)

/** A self-message is four corners in order: leave the lifeline, turn down, turn back, tip. */
enum MessagePath:
  case Straight(from: Point, to: Point)
  case Hook(out: Point, down: Point, back: Point, head: Point)

case class PlacedParticipant(id: NodeId, label: String, kind: ParticipantKind, box: Rect):
  def centerX: Double = box.x + box.w / 2

case class Lifeline(id: NodeId, x: Double, y0: Double, y1: Double)

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

case class SectionDivider(label: String, y: Double, x0: Double, x1: Double)

case class PlacedGroup(
    index: Int,
    kind: GroupKind,
    tab: Option[String],
    frame: Rect,
    dividers: List[SectionDivider],
)

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
)

/** A participant as layout sees it: explicit declare, or the first message that named the id. */
case class DeclaredParticipant(id: NodeId, label: String, kind: ParticipantKind)

object SequenceModel:

  def participantOrder(statements: List[SequenceStatement]): List[NodeId] =
    declarations(statements).map(_.id)

  /** First-seen order. A later declare for the same id does not move the column. */
  def declarations(statements: List[SequenceStatement]): List[DeclaredParticipant] =
    def walk(stmts: List[SequenceStatement], acc: List[DeclaredParticipant]): List[DeclaredParticipant] =
      stmts.foldLeft(acc)(see)

    def see(acc: List[DeclaredParticipant], stmt: SequenceStatement): List[DeclaredParticipant] =
      stmt match
        case SequenceStatement.Declare(id, label, kind) =>
          if acc.exists(_.id == id) then acc
          else acc :+ DeclaredParticipant(id, label.getOrElse(id.value), kind)
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
    aliases(statements, Map.empty).flatMap(_ => colors(statements)).map(_ => statements)

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
      case SequenceStatement.Declare(id, label, kind) =>
        seen.get(id) match
          case None =>
            Right(seen.updated(id, Alias(label.getOrElse(id.value), kind)))
          case Some(prev) =>
            val next = label.getOrElse(prev.label)
            if next != prev.label then Left(ParseError.ConflictingAlias(id, prev.label, next))
            else if kind != prev.kind then Left(ParseError.ConflictingAlias(id, kindWord(prev.kind), kindWord(kind)))
            else Right(seen)
      case SequenceStatement.Group(_, sections) =>
        sections.foldLeft[Either[ParseError, Map[NodeId, Alias]]](Right(seen)) { (acc, section) =>
          acc.flatMap(seen => aliases(section.body, seen))
        }
      case _ => Right(seen)

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
      case _ => Right(())

  private def checkColor(rgb: Rgb): Either[ParseError, Unit] =
    val badChannel = rgb.r < 0 || rgb.r > 255 || rgb.g < 0 || rgb.g > 255 || rgb.b < 0 || rgb.b > 255
    val badAlpha   = rgb.alpha.exists(a => a < 0.0 || a > 1.0)
    if badChannel || badAlpha then Left(ParseError.BadColor(rgb.show)) else Right(())
end SequenceModel
