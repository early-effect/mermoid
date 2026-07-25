package mermoid.docs

import ascent.ast.{Attr, UI}
import ascent.domtypes.AttrValue
import _root_.mermoid.{MermaidParser, RenderConfig, SvgNode, SvgRenderer}

/** Bridges a mermoid `SvgNode` tree into an ascent `UI`, so a diagram can be embedded in a doc page.
  *
  * This lives on the docs Test classpath and is deliberately **not published**. mermoid's only dependency is fastparse
  * — it knows nothing about ascent — and the reverse dependency can't exist yet either: a published `specular-mermoid`
  * would need mermoid 0.1.0, which this release is what creates. So the ~40 lines that close the gap live here until
  * that module ships upstream, at which point this file is deleted and the docs switch to the artifact.
  *
  * It is also the reference implementation the upstream issue points at: `SvgNode` → `UI` is a total structural map, no
  * string parsing, which is exactly why core exposes a tree rather than only a `String`.
  */
object MermoidUi:

  /** Parse and render `mmd`, or fail loudly.
    *
    * A doc page is a test. A diagram that no longer parses should turn CI red, not silently render an empty box, so
    * this throws rather than returning an `Either` the page could ignore.
    */
  def diagram(mmd: String, config: RenderConfig = RenderConfig()): UI[Any] =
    MermaidParser.parse(mmd) match
      case Right(d)  => toUi(SvgRenderer.renderTree(d, config))
      case Left(err) => throw new IllegalArgumentException(s"mermoid could not parse this diagram: $err\n$mmd")

  /** The SVG source for the same diagram — for pages that show the markup rather than the picture. */
  def svg(mmd: String, config: RenderConfig = RenderConfig()): String =
    MermaidParser.parse(mmd) match
      case Right(d)  => SvgRenderer.render(d, config)
      case Left(err) => throw new IllegalArgumentException(s"mermoid could not parse this diagram: $err\n$mmd")

  /** Total structural map. `Raw` carries a `<style>` body; ascent's SSR escapes text nodes, and `<style>` is a raw-text
    * element where a browser would NOT decode entities — so escaped CSS would be broken CSS. mermoid's generated CSS
    * contains none of `& < >` today, and [[cssIsEntitySafe]] is asserted in the spec so the day that stops being true
    * fails the build instead of shipping a corrupt stylesheet.
    */
  private[docs] def toUi(node: SvgNode): UI[Any] = node match
    case SvgNode.Text(value)                   => UI.Text(value)
    case SvgNode.Raw(content)                  => UI.Text(content)
    case SvgNode.Element(tag, attrs, children) =>
      UI.Element(
        tag,
        attrs.map((name, value) => Attr.StaticAttr(name, AttrValue.Str(value))).toVector,
        children.map(toUi).toVector,
      )

  /** True when `css` survives HTML text-node escaping unchanged — see [[toUi]]. */
  private[docs] def cssIsEntitySafe(css: String): Boolean =
    !css.exists(c => c == '&' || c == '<' || c == '>')
end MermoidUi
