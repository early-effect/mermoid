package mermoid.css

import zio.test.*

object CssAstSpec extends ZIOSpecDefault:

  def spec = suite("CssAst")(
    suite("CssValue")(
      test("Color holds a color string") {
        assertTrue(CssValue.Color("#ff0000") == CssValue.Color("#ff0000"))
      },
      test("Length holds value and unit") {
        assertTrue(CssValue.Length(14.0, "px") == CssValue.Length(14.0, "px"))
      },
      test("Number holds a numeric value") {
        assertTrue(CssValue.Number(0.8) == CssValue.Number(0.8))
      },
      test("Str holds a string value") {
        assertTrue(CssValue.Str("sans-serif") == CssValue.Str("sans-serif"))
      },
      test("Var with fallback") {
        val v = CssValue.Var("--primary", Some(CssValue.Color("#333")))
        assertTrue(v == CssValue.Var("--primary", Some(CssValue.Color("#333"))))
      },
      test("Var without fallback") {
        val v = CssValue.Var("--bg", None)
        assertTrue(v == CssValue.Var("--bg", None))
      },
      test("different values are not equal") {
        assertTrue(CssValue.Color("#111") != CssValue.Color("#222"))
      },
    ),
    suite("CssSelector")(
      test("Element selector") {
        val s: CssSelector = CssSelector.Element("rect")
        assertTrue(s == CssSelector.Element("rect"))
      },
      test("Class selector") {
        val s: CssSelector = CssSelector.Class("node")
        assertTrue(s == CssSelector.Class("node"))
      },
      test("Id selector") {
        val s: CssSelector = CssSelector.Id("node-A")
        assertTrue(s == CssSelector.Id("node-A"))
      },
      test("Compound selector combines parts") {
        val parts          = List(CssSelector.Class("node"), CssSelector.Class("highlight"))
        val s: CssSelector = CssSelector.Compound(parts)
        assertTrue(s == CssSelector.Compound(parts))
      },
      test("Descendant selector has parent and child") {
        val parent         = CssSelector.Class("node")
        val child          = CssSelector.Element("text")
        val s: CssSelector = CssSelector.Descendant(parent, child)
        assertTrue(s == CssSelector.Descendant(parent, child))
      },
      test("PseudoClass selector") {
        val base           = CssSelector.Class("node")
        val s: CssSelector = CssSelector.PseudoClass(base, "hover")
        assertTrue(s == CssSelector.PseudoClass(base, "hover"))
      },
    ),
    suite("CssDeclaration")(
      test("holds property and value") {
        val d = CssDeclaration("fill", CssValue.Color("#f9f9f9"))
        assertTrue(d.property == "fill", d.value == CssValue.Color("#f9f9f9"))
      }
    ),
    suite("CssRule")(
      test("holds selector and declarations") {
        val rule = CssRule(
          CssSelector.Class("node-shape"),
          List(
            CssDeclaration("fill", CssValue.Color("#f9f")),
            CssDeclaration("stroke", CssValue.Color("#333")),
          ),
        )
        assertTrue(rule.selector == CssSelector.Class("node-shape"), rule.declarations.length == 2)
      }
    ),
    suite("Stylesheet")(
      test("empty stylesheet has no variables or rules") {
        val s = Stylesheet.empty
        assertTrue(s.variables.isEmpty, s.rules.isEmpty)
      },
      test("merge combines variables with override precedence") {
        val base   = Stylesheet(variables = Map("--a" -> CssValue.Color("#111"), "--b" -> CssValue.Color("#222")))
        val over   = Stylesheet(variables = Map("--b" -> CssValue.Color("#999"), "--c" -> CssValue.Color("#333")))
        val merged = Stylesheet.merge(base, over)
        assertTrue(
          merged.variables("--a") == CssValue.Color("#111"),
          merged.variables("--b") == CssValue.Color("#999"),
          merged.variables("--c") == CssValue.Color("#333"),
        )
      },
      test("merge appends override rules after base rules") {
        val baseRule = CssRule(CssSelector.Class("a"), List(CssDeclaration("fill", CssValue.Color("#111"))))
        val overRule = CssRule(CssSelector.Class("b"), List(CssDeclaration("fill", CssValue.Color("#222"))))
        val base     = Stylesheet(rules = List(baseRule))
        val over     = Stylesheet(rules = List(overRule))
        val merged   = Stylesheet.merge(base, over)
        assertTrue(merged.rules == List(baseRule, overRule))
      },
    ),
  )
end CssAstSpec
