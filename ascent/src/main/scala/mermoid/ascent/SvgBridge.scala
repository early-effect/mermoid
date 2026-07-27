package mermoid.ascent

import ascent.ast.{Attr, UI}
import ascent.domtypes.AttrValue
import mermoid.SvgNode

/** Structural map from mermoid [[SvgNode]] to ascent [[UI]] (no string round-trip). */
object SvgBridge:

  def toUi(node: SvgNode): UI[Any] = node match
    case SvgNode.Text(value)                   => UI.Text(value)
    case SvgNode.Raw(content)                  => UI.Text(content)
    case SvgNode.Element(tag, attrs, children) =>
      UI.Element(
        tag,
        attrs.map((name, value) => Attr.StaticAttr(name, AttrValue.Str(value))).toVector,
        children.map(toUi).toVector,
      )

  def cssIsEntitySafe(css: String): Boolean =
    !css.exists(c => c == '&' || c == '<' || c == '>')
end SvgBridge
