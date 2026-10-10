package mermoid

import fastparse.Parsed

/** Why [[MermaidParser.parse]] rejected a diagram. The fastparse trace stays in [[ParseError.Failed]]. */
/** Where a state was claimed. Region indexes are 1-based in [[StatePlace.show]]. */
enum StatePlace:
  case Diagram
  case Of(id: NodeId)
  case Region(id: NodeId, index: Int)

  def show: String = this match
    case Diagram           => "the diagram"
    case Of(id)            => id.value
    case Region(id, index) => s"region ${index + 1} of ${id.value}"
end StatePlace

enum ParseError:
  /** `index` is the failure offset. `expected` is the parser label stack, not a prose sentence. */
  case Failed(index: Int, expected: List[String])
  case ConflictingAlias(id: NodeId, existing: String, duplicate: String)
  case BadColor(raw: String)

  /** The same id is a member of two composites, or of two regions of one composite. */
  case StateInTwoComposites(id: NodeId, first: StatePlace, second: StatePlace)
  case DuplicateComposite(id: NodeId)
  case ConflictingDescription(id: NodeId, existing: String, next: String)
  case ConflictingForm(id: NodeId, existing: String, next: String)
  case DividerOutsideComposite
  case UnknownStereotype(raw: String)
  case BadScale(raw: String)
  case ClassInTwoNamespaces(id: NodeId, first: String, second: String)

  def message: String = this match
    case Failed(index, expected) =>
      val tail = if expected.isEmpty then "" else s": expected ${expected.mkString(" / ")}"
      s"parse error at $index$tail"
    case ConflictingAlias(id, existing, duplicate) =>
      s"${id.value} is already $existing, not $duplicate"
    case BadColor(raw) =>
      s"not a color: $raw"
    case StateInTwoComposites(id, first, second) =>
      s"${id.value} is in ${first.show} and in ${second.show}"
    case DuplicateComposite(id) =>
      s"${id.value} already has a body"
    case ConflictingDescription(id, existing, next) =>
      s"${id.value} is \"$existing\", not \"$next\""
    case ConflictingForm(id, existing, next) =>
      s"${id.value} is $existing, not $next"
    case DividerOutsideComposite =>
      "a -- divider belongs inside a composite"
    case UnknownStereotype(raw) =>
      s"unknown stereotype <<$raw>>"
    case BadScale(raw) =>
      s"scale wants a width in pixels, not \"$raw\""
    case ClassInTwoNamespaces(id, first, second) =>
      s"${id.value} is in $first and in $second"
end ParseError

object ParseError:
  private[mermoid] def fromFastparse(failure: Parsed.Failure): ParseError =
    ParseError.Failed(failure.index, expectedLabels(failure))

  /** The fastparse label stack at a failure, outermost first, without blanks or repeats. */
  private[mermoid] def expectedLabels(failure: Parsed.Failure): List[String] =
    val traced = failure.trace()
    (traced.stack.map(_._1) :+ traced.label).filter(_.nonEmpty).distinct
