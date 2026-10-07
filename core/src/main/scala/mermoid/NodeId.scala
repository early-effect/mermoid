package mermoid

import scala.quoted.*

/** A flowchart node or state id, as written in the source (`A`, `Idle`, `[*]`). */
opaque type NodeId = String

object NodeId:

  /** A literal id, checked while it compiles. */
  inline def apply(inline raw: String): NodeId = ${ literal('raw) }

  /** Mermaid's id grammar: letters, digits, and `_`, or the state marker `[*]`. */
  def from(raw: String): Either[NodeIdError, NodeId] =
    if raw.isEmpty then Left(NodeIdError.Empty)
    else if raw == "[*]" then Right(raw)
    else
      raw.indexWhere(c => !(c.isLetterOrDigit || c == '_')) match
        case -1    => Right(raw)
        case index => Left(NodeIdError.IllegalCharacter(raw, index, raw.charAt(index)))

  /** `[*]`: a state diagram's start marker, and its end marker until layout splits the two. */
  private[mermoid] val stateMarker: NodeId = "[*]"

  /** The end marker when a state diagram uses `[*]` as both start and end. */
  private[mermoid] val stateEnd: NodeId = "[*]-end"

  /** The parser's ids and the ids layout makes up (dummy waypoints, the split `[*]` end marker). */
  private[mermoid] def trusted(raw: String): NodeId = raw

  /** A sequence participant, where a host selects by node id. Both ids share Mermaid's identifier grammar. */
  private[mermoid] def ofParticipant(id: ParticipantId): NodeId = id.value

  extension (id: NodeId) def value: String = id

  private def literal(raw: Expr[String])(using Quotes): Expr[NodeId] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("NodeId(...) takes a string literal. Use NodeId.from at runtime.", raw)
      case Some(text) =>
        from(text) match
          case Right(_)    => '{ NodeId.trusted(${ Expr(text) }) }
          case Left(error) => report.errorAndAbort(error.message, raw)
end NodeId

/** Why [[NodeId.from]] rejected a string. */
enum NodeIdError:
  case Empty
  case IllegalCharacter(raw: String, index: Int, char: Char)

  def message: String = this match
    case Empty                              => "a node id cannot be empty"
    case IllegalCharacter(raw, index, char) =>
      s"'$char' at $index in \"$raw\" is not allowed in a node id (letters, digits, _, or [*])"
end NodeIdError
