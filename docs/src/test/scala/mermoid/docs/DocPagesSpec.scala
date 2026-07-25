package mermoid.docs

import ascent.ast.UI
import ascent.html.Html
import specular.*
import zio.*
import zio.test.*

/** Checks every diagram on the site, not just the ones carrying an explicit `.assert`.
  *
  * Specular's interpreter turns an `Example` into a test only when it has an assertion, so a bare `example {
  * MermoidUi.diagram(...) }` would be built during page construction and then never looked at again — a diagram that
  * silently rendered an empty `<svg>` would still ship. This walks the real `pages` vector `BuildSite` deploys and
  * asserts each example produces a plausible diagram, so the picture on the page and the check in CI can't diverge.
  */
object DocPagesSpec extends ZIOSpecDefault:

  private def examplesOf(nodes: Vector[DocNode]): Vector[Example[Any]] = nodes.flatMap {
    case Section(_, children) => examplesOf(children)
    case ex: Example[?]       => Vector(ex.asInstanceOf[Example[Any]])
    case _: ValueExample[?]   => Vector.empty
    case _: Prose             => Vector.empty
  }

  /** Every element in a UI tree, flattened — a diagram's node/edge groups all appear here. */
  private def elements(ui: UI[Any]): List[UI.Element[Any]] = ui match
    case e: UI.Element[Any] => e :: e.children.toList.flatMap(elements)
    case _                  => Nil

  private def pageTests(p: DocPage): Vector[Spec[Any, Nothing]] =
    examplesOf(p.children).map { ex =>
      test(s"${p.title}: ${ex.id} renders a diagram") {
        for
          ui   <- ZIO.scoped(ex.body)
          html <- Html.render(ui)
        yield
          val els = elements(ui)
          assertTrue(
            // Not an empty shell: a real diagram has a root <svg>, a <style>, and at least one node group.
            els.headOption.map(_.tag).contains("svg"),
            els.exists(_.tag == "style"),
            els.exists(_.attrs.exists {
              case ascent.ast.Attr.StaticAttr("class", ascent.domtypes.AttrValue.Str(v)) => v.startsWith("node ")
              case _                                                                     => false
            }),
            // The serialized form is well-formed enough to embed, and free of the divergence
            // Num.format exists to prevent.
            html.startsWith("<svg"),
            !html.contains("NaN"),
            !html.contains("Infinity"),
          )
      }
    }

  def spec = suite("doc pages")(
    BuildSite.pages.map(p => suite(p.title)(pageTests(p)*))*
  )
end DocPagesSpec
