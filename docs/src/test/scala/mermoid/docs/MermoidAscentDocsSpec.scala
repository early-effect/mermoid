package mermoid.docs

import ascent.ast.{Attr, UI}
import ascent.domtypes.AttrValue
import ascent.html.Html
import mermoid.ascent.{MermoidAscent, SvgBridge}
import mermoid.*
import zio.test.*

/** Proves the SVG→UI bridge and static svgDiagram path still round-trip for structure docs. */
object MermoidAscentDocsSpec extends ZIOSpecDefault:

  private val flowchart =
    """flowchart LR
      |  A((Go)) --> B[Stop]
      |""".stripMargin

  def spec = suite("MermoidAscent docs bridge")(
    test("svgDiagram SSR starts with svg and carries node classes") {
      for html <- Html.render(MermoidAscent.svgDiagram(flowchart))
      yield assertTrue(html.startsWith("<svg"), html.contains("node-"), !html.contains("NaN"))
    },
    test("SvgBridge maps Element/Text/Raw") {
      val node = SvgNode.Element("g", List("class" -> "node node-rect"), List(SvgNode.Text("Hi")))
      SvgBridge.toUi(node) match
        case UI.Element("g", attrs, kids) =>
          assertTrue(
            attrs.contains(Attr.StaticAttr("class", AttrValue.Str("node node-rect"))),
            kids == Vector(UI.Text("Hi")),
          )
        case other => assertTrue(false, other.toString.nonEmpty)
    },
    test("cssIsEntitySafe rejects entity-breaking characters") {
      assertTrue(
        SvgBridge.cssIsEntitySafe(".a { fill: red }"),
        !SvgBridge.cssIsEntitySafe("a < b"),
      )
    },
    test("bad mermaid throws from svgDiagram") {
      assertTrue(scala.util.Try(MermoidAscent.svgDiagram("not a diagram at all")).isFailure)
    },
  )
end MermoidAscentDocsSpec
