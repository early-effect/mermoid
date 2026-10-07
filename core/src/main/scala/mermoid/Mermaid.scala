package mermoid

import scala.annotation.publicInBinary
import scala.quoted.*

/** A Mermaid source that has already parsed. Every renderer that takes one is total.
  *
  * `Mermaid("""...""")` parses at compile time, so a broken diagram is a compile error and the parsed [[Diagram]] is
  * built into the call site. `Mermaid.from` is the runtime path for text read from files or users.
  */
final class Mermaid @publicInBinary private (val source: String, val diagram: Diagram):
  override def equals(that: Any): Boolean = that match
    case m: Mermaid => m.source == source
    case _          => false
  override def hashCode: Int    = source.hashCode
  override def toString: String = s"Mermaid($source)"
end Mermaid

object Mermaid:

  /** A literal (or a literal's `.stripMargin`) checked by [[MermaidParser]] while it compiles. */
  inline def apply(inline raw: String): Mermaid = ${ literal('raw) }

  def from(raw: String): Either[ParseError, Mermaid] =
    MermaidParser.parse(raw).map(new Mermaid(raw, _))

  private def literal(raw: Expr[String])(using Quotes): Expr[Mermaid] =
    import quotes.reflect.*
    import MermaidLift.given
    val text = raw match
      case '{ scala.Predef.augmentString(${ Expr(s) }).stripMargin }               => Some(s.stripMargin)
      case '{ scala.Predef.augmentString(${ Expr(s) }).stripMargin(${ Expr(c) }) } => Some(s.stripMargin(c))
      case Expr(s)                                                                 => Some(s)
      case _                                                                       => None
    text match
      case None =>
        report.errorAndAbort(
          "Mermaid(...) takes a string literal. Use Mermaid.from for text known only at runtime.",
          raw,
        )
      case Some(src) =>
        MermaidParser.parse(src) match
          case Right(diagram) => '{ new Mermaid(${ Expr(src) }, ${ Expr(diagram) }) }
          case Left(error)    =>
            report.errorAndAbort(s"Not a Mermaid diagram: ${error.message}", errorPosition(raw, src, error))
    end match
  end literal

  /** The failing character when the literal's text is its source (no escapes, no margin), else the whole literal. */
  private def errorPosition(raw: Expr[String], src: String, error: ParseError)(using Quotes): quotes.reflect.Position =
    import quotes.reflect.*
    val pos = raw.asTerm.pos
    error match
      case ParseError.Failed(index, _) =>
        val opening = pos.sourceCode.collect {
          case code if code == "\"\"\"" + src + "\"\"\"" => 3
          case code if code == "\"" + src + "\""         => 1
        }
        opening.fold(pos) { quote =>
          val at = pos.start + quote + index
          Position(pos.sourceFile, at, at)
        }
      case _ => pos
    end match
  end errorPosition
end Mermaid
