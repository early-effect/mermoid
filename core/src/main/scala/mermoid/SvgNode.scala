package mermoid

/** An SVG document as a tree.
  *
  * This is the integration contract for downstream consumers: anything that wants to build a UI tree (e.g. a Scala.js
  * virtual DOM) or post-process the diagram maps `SvgNode` directly instead of re-parsing serialized markup.
  * `SvgSerializer` turns it back into a `String`.
  */
enum SvgNode:
  /** An element. `attrs` order is preserved; values are escaped at serialization time. */
  case Element(tag: String, attrs: List[(String, String)], children: List[SvgNode])

  /** Character data. Escaped at serialization time. */
  case Text(value: String)

  /** Verbatim content — used for the `<style>` body, which is CSS and must never be XML-escaped. */
  case Raw(content: String)

object SvgNode:

  /** An element with no children, e.g. `<rect .../>`. */
  def leaf(tag: String)(attrs: (String, String)*): SvgNode =
    Element(tag, attrs.toList, Nil)

  /** An element with children. */
  def elem(tag: String)(attrs: (String, String)*)(children: SvgNode*): SvgNode =
    Element(tag, attrs.toList, children.toList)

  /** An element whose only child is escaped character data, e.g. `<text ...>label</text>`. */
  def textElem(tag: String)(attrs: (String, String)*)(content: String): SvgNode =
    Element(tag, attrs.toList, List(Text(content)))

  def attr(name: String, value: String): (String, String) = name -> value

  def attr(name: String, value: Double): (String, String) = name -> Num.format(value)
end SvgNode
