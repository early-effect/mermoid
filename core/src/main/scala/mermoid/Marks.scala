package mermoid

/** A glyph at one end of a class or ER edge. Not a flowchart arrow. */
enum MarkKind:
  case Triangle
  case Diamond
  case OpenDiamond
  case Arrow
  case Lollipop
  case ExactlyOne
  case ZeroOrOne
  case OneOrMore
  case ZeroOrMore

  def markerId: String = this match
    case MarkKind.Triangle    => "mark-triangle"
    case MarkKind.Diamond     => "mark-diamond"
    case MarkKind.OpenDiamond => "mark-diamond-open"
    case MarkKind.Arrow       => "mark-arrow"
    case MarkKind.Lollipop    => "mark-lollipop"
    case MarkKind.ExactlyOne  => "mark-exactly-one"
    case MarkKind.ZeroOrOne   => "mark-zero-or-one"
    case MarkKind.OneOrMore   => "mark-one-or-more"
    case MarkKind.ZeroOrMore  => "mark-zero-or-more"
end MarkKind

/** Which glyphs sit on an edge, and whether the shaft is dashed. */
case class RelationMark(from: Option[MarkKind], to: Option[MarkKind], dashed: Boolean):
  def kinds: List[MarkKind] = from.toList ++ to.toList
