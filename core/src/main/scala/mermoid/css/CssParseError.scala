package mermoid.css

import fastparse.Parsed
import mermoid.ParseError

/** Why [[CssParser]] rejected its input. */
enum CssParseError:
  /** `index` is the failure offset. `expected` is the parser label stack, not a prose sentence. */
  case Failed(index: Int, expected: List[String])

  def message: String = this match
    case Failed(index, expected) =>
      val tail = if expected.isEmpty then "" else s": expected ${expected.mkString(" / ")}"
      s"CSS parse error at $index$tail"
end CssParseError

object CssParseError:
  private[mermoid] def fromFastparse(failure: Parsed.Failure): CssParseError =
    CssParseError.Failed(failure.index, ParseError.expectedLabels(failure))
