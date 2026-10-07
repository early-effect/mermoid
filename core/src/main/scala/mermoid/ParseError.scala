package mermoid

import fastparse.Parsed

/** Why [[MermaidParser.parse]] rejected a diagram. The fastparse trace stays in [[ParseError.Failed]]. */
enum ParseError:
  /** `index` is the failure offset. `expected` is the parser label stack, not a prose sentence. */
  case Failed(index: Int, expected: List[String])
  case ConflictingAlias(id: ParticipantId, existing: String, duplicate: String)
  case BadColor(raw: String)

  def message: String = this match
    case Failed(index, expected) =>
      val tail = if expected.isEmpty then "" else s": expected ${expected.mkString(" / ")}"
      s"parse error at $index$tail"
    case ConflictingAlias(id, existing, duplicate) =>
      s"${id.value} is already $existing, not $duplicate"
    case BadColor(raw) =>
      s"not a color: $raw"
end ParseError

object ParseError:
  private[mermoid] def fromFastparse(failure: Parsed.Failure): ParseError =
    ParseError.Failed(failure.index, expectedLabels(failure))

  /** The fastparse label stack at a failure, outermost first, without blanks or repeats. */
  private[mermoid] def expectedLabels(failure: Parsed.Failure): List[String] =
    val traced = failure.trace()
    (traced.stack.map(_._1) :+ traced.label).filter(_.nonEmpty).distinct
