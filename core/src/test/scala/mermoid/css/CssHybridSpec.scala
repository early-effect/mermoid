package mermoid.css

import zio.test.*

object CssHybridSpec extends ZIOSpecDefault:

  def spec = suite("CssHybrid")(
    test("maps fill and stroke on .node-shape to background and border-color") {
      val sheet = Stylesheet(
        rules = List(
          CssRule(
            CssSelector.Descendant(CssSelector.Class("warn"), CssSelector.Class("node-shape")),
            List(
              CssDeclaration("fill", CssValue.Color("#4a4030")),
              CssDeclaration("stroke", CssValue.Color("#e0c070")),
            ),
          )
        )
      )
      val css = CssRenderer.render(CssHybrid.htmlCompat(sheet))
      assertTrue(
        css.contains("background: #4a4030"),
        css.contains("border-color: #e0c070"),
        css.contains("fill: #4a4030"),
        css.contains("stroke: #e0c070"),
      )
    },
    test("classDef-style .hot { fill } also targets .hot .node-shape") {
      val sheet = Stylesheet(
        rules = List(
          CssRule(
            CssSelector.Class("hot"),
            List(CssDeclaration("fill", CssValue.Color("#ffdddd"))),
          )
        )
      )
      val css = CssRenderer.render(CssHybrid.htmlCompat(sheet))
      assertTrue(
        css.contains(".hot .node-shape"),
        css.contains("background: #ffdddd"),
      )
    },
    test("fill:none becomes transparent background") {
      val sheet = Stylesheet(
        rules = List(
          CssRule(
            CssSelector.Class("node-shape"),
            List(CssDeclaration("fill", CssValue.Str("none"))),
          )
        )
      )
      val css = CssRenderer.render(CssHybrid.htmlCompat(sheet))
      assertTrue(css.contains("background: transparent"))
    },
    test("node-label fill becomes mermoid-node-label color") {
      val sheet = Stylesheet(
        rules = List(
          CssRule(
            CssSelector.Class("node-label"),
            List(CssDeclaration("fill", CssValue.Var("--mermoid-text", None))),
          )
        )
      )
      val css = CssRenderer.render(CssHybrid.htmlCompat(sheet))
      assertTrue(
        css.contains(".mermoid-node-label"),
        css.contains("color: var(--mermoid-text)"),
      )
    },
    test("htmlInline maps style statement properties") {
      val mapped = CssHybrid.htmlInline(Map(CssProperty.Fill -> "#eee", CssProperty.Stroke -> "#333"))
      assertTrue(
        mapped.get(CssProperty.Fill).contains("#eee"),
        mapped.get(CssProperty.Background).contains("#eee"),
        mapped.get(CssProperty.BorderColor).contains("#333"),
      )
    },
  )
end CssHybridSpec
