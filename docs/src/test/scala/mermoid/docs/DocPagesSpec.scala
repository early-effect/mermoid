package mermoid.docs

import ascent.ast.UI
import ascent.html.Html
import specular.*
import zio.*
import zio.test.*

/** Checks every diagram on the site, not just the ones carrying an explicit `.assert`.
  *
  * Specular's interpreter turns an `Example` into a test only when it has an assertion, so a bare `example {
  * MermoidAscent.svgDiagram(...) }` would be built during page construction and then never looked at again — a diagram
  * that silently rendered an empty shell would still ship. This walks the real `pages` vector `BuildSite` deploys and
  * asserts each example produces a plausible diagram (SVG embed **or** hybrid `mermoid-ascent` root).
  */
object DocPagesSpec extends ZIOSpecDefault:

  private def examplesOf(nodes: Vector[DocNode]): Vector[Example[Any]] = nodes.flatMap {
    case Section(_, children) => examplesOf(children)
    case ex: Example[?]       => Vector(ex.asInstanceOf[Example[Any]])
    case _: ValueExample[?]   => Vector.empty
    case _: Prose             => Vector.empty
  }

  private def elements(ui: UI[Any]): List[UI.Element[Any]] = ui match
    case e: UI.Element[Any] => e :: e.children.toList.flatMap(elements)
    case _                  => Nil

  private def classOf(e: UI.Element[Any]): Option[String] =
    e.attrs.collectFirst { case ascent.ast.Attr.StaticAttr("class", ascent.domtypes.AttrValue.Str(v)) =>
      v
    }

  private def looksLikeDiagram(ui: UI[Any], html: String): Boolean =
    val els     = elements(ui)
    val svgRoot =
      els.headOption.map(_.tag).contains("svg") &&
        els.exists(_.tag == "style") &&
        els.exists(e => classOf(e).exists(_.startsWith("node "))) &&
        html.startsWith("<svg")
    val hybrid =
      els.exists(e =>
        classOf(e).exists(c =>
          c.contains("mermoid-diagram") || c.contains("mermoid-ascent") || c.contains("mermoid-root")
        )
      ) &&
        (html.contains("mermoid-node") || html.contains("<svg"))
    (svgRoot || hybrid) && !html.contains("NaN") && !html.contains("Infinity")
  end looksLikeDiagram

  private def pageTests(p: DocPage): Vector[Spec[Any, Nothing]] =
    examplesOf(p.children).map { ex =>
      test(s"${p.title}: ${ex.id} renders a diagram") {
        for
          ui   <- ZIO.scoped(ex.body)
          html <- Html.render(ui)
        yield assertTrue(looksLikeDiagram(ui, html))
      }
    }

  def spec = suite("doc pages")(
    BuildSite.pages.map(p => suite(p.title)(pageTests(p)*))*
  )
end DocPagesSpec
