package mermoid.css

import zio.test.*

object CssRendererSpec extends ZIOSpecDefault:

  def spec = suite("CssRenderer")(
    suite("renderValue")(
      test("Color renders as-is") {
        assertTrue(CssRenderer.renderValue(CssValue.Color("#333")) == "#333")
      },
      test("Length renders value with unit, whole numbers without a decimal") {
        assertTrue(
          CssRenderer.renderValue(CssValue.Length(14.0, "px")) == "14px",
          CssRenderer.renderValue(CssValue.Length(1.5, "em")) == "1.5em",
        )
      },
      test("Number renders as string") {
        assertTrue(CssRenderer.renderValue(CssValue.Number(0.8)) == "0.8")
      },
      test("Str renders as-is") {
        assertTrue(CssRenderer.renderValue(CssValue.Str("sans-serif")) == "sans-serif")
      },
      test("Var without fallback") {
        assertTrue(CssRenderer.renderValue(CssValue.Var("--bg", None)) == "var(--bg)")
      },
      test("Var with fallback") {
        val v = CssValue.Var("--bg", Some(CssValue.Color("#fff")))
        assertTrue(CssRenderer.renderValue(v) == "var(--bg, #fff)")
      },
    ),
    suite("resolveValue")(
      test("resolves a Var to its defined value") {
        val vars   = Map("--primary" -> CssValue.Color("#00f"))
        val result = CssRenderer.resolveValue(CssValue.Var("--primary", None), vars)
        assertTrue(result == CssValue.Color("#00f"))
      },
      test("falls back when variable not defined") {
        val result = CssRenderer.resolveValue(
          CssValue.Var("--missing", Some(CssValue.Color("#999"))),
          Map.empty,
        )
        assertTrue(result == CssValue.Color("#999"))
      },
      test("returns Var unchanged when no definition and no fallback") {
        val v = CssValue.Var("--missing", None)
        assertTrue(CssRenderer.resolveValue(v, Map.empty) == v)
      },
      test("resolves chained variables") {
        val vars = Map(
          "--a" -> CssValue.Var("--b", None),
          "--b" -> CssValue.Color("#123"),
        )
        val result = CssRenderer.resolveValue(CssValue.Var("--a", None), vars)
        assertTrue(result == CssValue.Color("#123"))
      },
      test("non-Var values pass through unchanged") {
        val c = CssValue.Color("#abc")
        assertTrue(CssRenderer.resolveValue(c, Map("--x" -> CssValue.Number(1))) == c)
      },
    ),
    suite("renderDeclaration")(
      test("renders property: value with indent") {
        val d = CssDeclaration("fill", CssValue.Color("#f9f"))
        assertTrue(CssRenderer.renderDeclaration(d) == "  fill: #f9f;")
      }
    ),
    suite("renderSelector")(
      test("Element") {
        assertTrue(CssRenderer.renderSelector(CssSelector.Element("rect")) == "rect")
      },
      test("Class") {
        assertTrue(CssRenderer.renderSelector(CssSelector.Class("node")) == ".node")
      },
      test("Id") {
        assertTrue(CssRenderer.renderSelector(CssSelector.Id("node-A")) == "#node-A")
      },
      test("Compound") {
        val s = CssSelector.Compound(List(CssSelector.Class("node"), CssSelector.Class("highlight")))
        assertTrue(CssRenderer.renderSelector(s) == ".node.highlight")
      },
      test("Descendant") {
        val s = CssSelector.Descendant(CssSelector.Class("node"), CssSelector.Element("text"))
        assertTrue(CssRenderer.renderSelector(s) == ".node text")
      },
      test("PseudoClass") {
        val s = CssSelector.PseudoClass(CssSelector.Class("node"), "hover")
        assertTrue(CssRenderer.renderSelector(s) == ".node:hover")
      },
    ),
    suite("renderRule")(
      test("renders a complete rule block") {
        val rule = CssRule(
          CssSelector.Class("node-shape"),
          List(
            CssDeclaration("fill", CssValue.Color("#f9f")),
            CssDeclaration("stroke", CssValue.Color("#333")),
          ),
        )
        val expected = ".node-shape {\n  fill: #f9f;\n  stroke: #333;\n}"
        assertTrue(CssRenderer.renderRule(rule) == expected)
      }
    ),
    suite("renderVariables")(
      test("empty variables produce empty string") {
        assertTrue(CssRenderer.renderVariables(Map.empty) == "")
      },
      test("renders :root block sorted by name") {
        val vars = Map(
          "--b-color" -> CssValue.Color("#222"),
          "--a-color" -> CssValue.Color("#111"),
        )
        val expected = ":root {\n  --a-color: #111;\n  --b-color: #222;\n}"
        assertTrue(CssRenderer.renderVariables(vars) == expected)
      },
    ),
    suite("render (full stylesheet)")(
      test("renders variables and rules") {
        val ss = Stylesheet(
          variables = Map("--bg" -> CssValue.Color("#fff")),
          rules = List(
            CssRule(CssSelector.Class("node-shape"), List(CssDeclaration("fill", CssValue.Var("--bg", None))))
          ),
        )
        val result = CssRenderer.render(ss)
        assertTrue(
          result.contains(":root {"),
          result.contains("--bg: #fff;"),
          result.contains(".node-shape {"),
          result.contains("fill: var(--bg);"),
        )
      },
      test("resolveVariables substitutes var references") {
        val ss = Stylesheet(
          variables = Map("--bg" -> CssValue.Color("#fff")),
          rules = List(
            CssRule(CssSelector.Class("node-shape"), List(CssDeclaration("fill", CssValue.Var("--bg", None))))
          ),
        )
        val result = CssRenderer.render(ss, resolveVariables = true)
        assertTrue(
          result.contains("fill: #fff;"),
          !result.contains("var(--bg)"),
        )
      },
      test("empty stylesheet renders empty string") {
        assertTrue(CssRenderer.render(Stylesheet.empty) == "")
      },
    ),
  )
end CssRendererSpec
