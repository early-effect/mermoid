package mermoid.css

import zio.test.*

object CssParserSpec extends ZIOSpecDefault:

  def spec = suite("CssParser")(
    suite("values")(
      test("parses hex color") {
        val result = CssParser.parseValue("#ff0033")
        assertTrue(result == Right(CssValue.Color("#ff0033")))
      },
      test("parses short hex color") {
        val result = CssParser.parseValue("#f03")
        assertTrue(result == Right(CssValue.Color("#f03")))
      },
      test("parses number") {
        val result = CssParser.parseValue("42")
        assertTrue(result == Right(CssValue.Number(42.0)))
      },
      test("parses negative number") {
        val result = CssParser.parseValue("-3.5")
        assertTrue(result == Right(CssValue.Number(-3.5)))
      },
      test("parses length with px") {
        val result = CssParser.parseValue("14px")
        assertTrue(result == Right(CssValue.Length(14.0, "px")))
      },
      test("parses length with em") {
        val result = CssParser.parseValue("1.5em")
        assertTrue(result == Right(CssValue.Length(1.5, "em")))
      },
      test("parses length with percent") {
        val result = CssParser.parseValue("100%")
        assertTrue(result == Right(CssValue.Length(100.0, "%")))
      },
      test("parses var reference") {
        val result = CssParser.parseValue("var(--my-color)")
        assertTrue(result == Right(CssValue.Var("--my-color", None)))
      },
      test("parses var with fallback") {
        val result = CssParser.parseValue("var(--my-color, #f00)")
        assertTrue(result == Right(CssValue.Var("--my-color", Some(CssValue.Color("#f00")))))
      },
      test("parses quoted string") {
        val result = CssParser.parseValue("\"sans-serif\"")
        assertTrue(result == Right(CssValue.Str("sans-serif")))
      },
      test("parses single-quoted string") {
        val result = CssParser.parseValue("'monospace'")
        assertTrue(result == Right(CssValue.Str("monospace")))
      },
      test("parses unquoted keyword as Str") {
        val result = CssParser.parseValue("none")
        assertTrue(result == Right(CssValue.Str("none")))
      },
    ),
    suite("selectors")(
      test("parses element selector") {
        val result = CssParser.parseSelector("rect")
        assertTrue(result == Right(CssSelector.Element("rect")))
      },
      test("parses class selector") {
        val result = CssParser.parseSelector(".node-shape")
        assertTrue(result == Right(CssSelector.Class("node-shape")))
      },
      test("parses id selector") {
        val result = CssParser.parseSelector("#main")
        assertTrue(result == Right(CssSelector.Id("main")))
      },
      test("parses descendant selector") {
        val result = CssParser.parseSelector(".edge-thick .edge-line")
        assertTrue(
          result == Right(
            CssSelector.Descendant(CssSelector.Class("edge-thick"), CssSelector.Class("edge-line"))
          )
        )
      },
      test("parses multi-level descendant") {
        val result = CssParser.parseSelector(".a .b .c")
        assertTrue(
          result == Right(
            CssSelector.Descendant(
              CssSelector.Descendant(CssSelector.Class("a"), CssSelector.Class("b")),
              CssSelector.Class("c"),
            )
          )
        )
      },
      test("parses pseudo-class") {
        val result = CssParser.parseSelector("a:hover")
        assertTrue(result == Right(CssSelector.PseudoClass(CssSelector.Element("a"), "hover")))
      },
    ),
    suite("rules")(
      test("parses rule with hyphenated class name and multiple declarations") {
        val css    = """.node-shape { fill: #f00; stroke: #333; stroke-width: 2px; }"""
        val result = CssParser.parse(css)
        assertTrue(
          result.isRight,
          result.toOption.get.rules.head.declarations.size == 3,
        )
      },
      test("parses a simple rule") {
        val css    = """.node-shape { fill: #f00; stroke: #333; }"""
        val result = CssParser.parse(css)
        assertTrue(
          result == Right(
            Stylesheet(
              rules = List(
                CssRule(
                  CssSelector.Class("node-shape"),
                  List(
                    CssDeclaration("fill", CssValue.Color("#f00")),
                    CssDeclaration("stroke", CssValue.Color("#333")),
                  ),
                )
              )
            )
          )
        )
      },
      test("parses rule with var values") {
        val css    = """.edge-line { stroke: var(--mermoid-line); }"""
        val result = CssParser.parse(css)
        assertTrue(
          result == Right(
            Stylesheet(
              rules = List(
                CssRule(
                  CssSelector.Class("edge-line"),
                  List(CssDeclaration("stroke", CssValue.Var("--mermoid-line", None))),
                )
              )
            )
          )
        )
      },
      test("parses rule without trailing semicolon") {
        val css    = """.x { fill: red }"""
        val result = CssParser.parse(css)
        assertTrue(result.isRight)
      },
      test("parses a space-separated composite value") {
        val result = CssParser.parse(""".x { border: 1px solid #333; }""")
        assertTrue(
          result.map(_.rules.head.declarations) == Right(
            List(
              CssDeclaration("border", CssValue.Str("1px solid #333"))
            )
          )
        )
      },
      test("parses a comma-separated value list") {
        // stroke-dasharray is the case that matters: the built-in themes emit `4,2`, so without
        // comma support a theme's own rendered output would not parse back in.
        val result = CssParser.parse(""".x { stroke-dasharray: 4,2; }""")
        assertTrue(
          result.map(_.rules.head.declarations) == Right(
            List(
              CssDeclaration("stroke-dasharray", CssValue.Str("4, 2"))
            )
          )
        )
      },
      test("parses a font stack") {
        val result = CssParser.parse(""".x { font-family: ui-monospace, "Fira Code", monospace; }""")
        assertTrue(
          result.map(_.rules.head.declarations) == Right(
            List(
              CssDeclaration("font-family", CssValue.Str("ui-monospace, Fira Code, monospace"))
            )
          )
        )
      },
    ),
    suite(":root variables")(
      test("parses :root block") {
        val css    = """:root { --my-color: #abc; --my-size: 14px; }"""
        val result = CssParser.parse(css)
        assertTrue(
          result == Right(
            Stylesheet(
              variables = Map(
                "--my-color" -> CssValue.Color("#abc"),
                "--my-size"  -> CssValue.Length(14.0, "px"),
              )
            )
          )
        )
      },
      test("parses a comma-separated variable value") {
        val result = CssParser.parse(""":root { --font: ui-monospace, monospace; }""")
        assertTrue(result.map(_.variables) == Right(Map("--font" -> CssValue.Str("ui-monospace, monospace"))))
      },
    ),
    suite("full stylesheet")(
      test("every built-in theme's rendered output parses back") {
        // The renderer and the parser have to agree: a consumer who reads a theme, edits the text and
        // feeds it back as a customStylesheet is doing exactly this round-trip.
        val failures = for
          name     <- ThemeName.values.toList
          resolved <- List(true, false)
          rendered = CssRenderer.render(Theme.toStylesheet(name), resolveVariables = resolved)
          err <- CssParser.parse(rendered).left.toOption
        yield s"$name (resolveVariables=$resolved): $err"
        assertTrue(failures.isEmpty)
      },
      test("parses :root + rules") {
        val css =
          """:root {
            |  --color: #f00;
            |}
            |.node-shape {
            |  fill: var(--color);
            |  stroke-width: 2px;
            |}""".stripMargin
        val result = CssParser.parse(css)
        assertTrue(
          result.isRight,
          result.toOption.get.variables.size == 1,
          result.toOption.get.rules.size == 1,
          result.toOption.get.rules.head.declarations.size == 2,
        )
      },
      test("parses multiple rules") {
        val css =
          """.a { fill: red; }
            |.b { stroke: blue; }""".stripMargin
        val result = CssParser.parse(css)
        assertTrue(
          result.isRight,
          result.toOption.get.rules.size == 2,
        )
      },
      test("handles CSS comments") {
        val css =
          """/* theme */
            |.node-shape {
            |  /* primary fill */
            |  fill: #f00;
            |}""".stripMargin
        val result = CssParser.parse(css)
        assertTrue(
          result.isRight,
          result.toOption.get.rules.size == 1,
        )
      },
      test("empty input produces empty stylesheet") {
        val result = CssParser.parse("")
        assertTrue(result == Right(Stylesheet.empty))
      },
      test("parses :root followed by a rule") {
        val css    = ":root {\n  --bg: #fff;\n}\n.node-shape {\n  fill: var(--bg);\n}"
        val result = CssParser.parse(css)
        assertTrue(
          result.isRight,
          result.toOption.get.variables.contains("--bg"),
          result.toOption.get.rules.size == 1,
        )
      },
      test("round-trip: render then parse") {
        val original = Stylesheet(
          variables = Map("--bg" -> CssValue.Color("#fff")),
          rules = List(
            CssRule(
              CssSelector.Class("node-shape"),
              List(
                CssDeclaration("fill", CssValue.Var("--bg", None)),
                CssDeclaration("stroke-width", CssValue.Length(2.0, "px")),
              ),
            )
          ),
        )
        val rendered = CssRenderer.render(original)
        val parsed   = CssParser.parse(rendered)
        assertTrue(
          parsed.isRight,
          parsed.toOption.get.variables == original.variables,
          parsed.toOption.get.rules.size == original.rules.size,
        )
      },
    ),
    suite("integration")(
      test("parsed stylesheet can be used as custom stylesheet") {
        val css    = """.my-highlight { fill: #ff0; stroke: #f00; }"""
        val parsed = CssParser.parse(css)
        assertTrue(parsed.isRight)
        val stylesheet = parsed.toOption.get
        val merged     = Stylesheet.merge(Stylesheet.empty, stylesheet)
        assertTrue(merged.rules.size == 1)
        val rendered = CssRenderer.render(merged)
        assertTrue(
          rendered.contains(".my-highlight"),
          rendered.contains("#ff0"),
        )
      }
    ),
  )
end CssParserSpec
