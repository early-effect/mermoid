package mermoid

/** Turns an [[SvgNode]] tree into markup. This is the single place XML escaping happens. */
object SvgSerializer:

  private val indentUnit = "  "

  /** Serialize with indentation — the readable form written to `.svg` files. */
  def render(node: SvgNode): String = renderNode(node, Some(0))

  /** Serialize without any added whitespace — for embedding in HTML or comparing structurally. */
  def renderCompact(node: SvgNode): String = renderNode(node, None)

  /** `depth` is `Some(n)` when indenting, `None` when compact. */
  private def renderNode(node: SvgNode, depth: Option[Int]): String = node match
    case SvgNode.Text(value)                   => SvgUtil.escapeXml(value)
    case SvgNode.Raw(content)                  => content
    case SvgNode.Element(tag, attrs, children) =>
      val pad  = depth.map(indentUnit * _).getOrElse("")
      val open = s"$pad<$tag${renderAttrs(attrs)}"
      if children.isEmpty then s"$open/>"
      else if children.forall(isCharacterData) then s"$open>${children.map(renderNode(_, None)).mkString}</$tag>"
      else
        val sep  = if depth.isDefined then "\n" else ""
        val kids = children.map(renderNode(_, depth.map(_ + 1))).mkString(sep)
        s"$open>$sep$kids$sep$pad</$tag>"

  private def isCharacterData(node: SvgNode): Boolean = node match
    case SvgNode.Text(_) | SvgNode.Raw(_) => true
    case SvgNode.Element(_, _, _)         => false

  private def renderAttrs(attrs: List[(String, String)]): String =
    attrs.map((name, value) => s""" $name="${SvgUtil.escapeXml(value)}"""").mkString
end SvgSerializer
