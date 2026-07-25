package mermoid.docs

import _root_.mermoid.RenderConfig
import _root_.mermoid.css.{CssParser, Stylesheet, ThemeName}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** Supplying your own CSS: parsed from a string, or built as an AST, then merged over a theme. */
object CustomCss extends DocSpecSuite:

  private val pipeline =
    """flowchart LR
      |    classDef io fill:#fff7ed,stroke:#c2410c
      |    In[(Read)] --> Map[Transform]
      |    Map --> Out[(Write)]
      |    class In,Out io
      |""".stripMargin

  private val overrides =
    """:root {
      |  --mermoid-main-bkg: #f8fafc;
      |  --mermoid-node-border: #0f766e;
      |  --mermoid-line: #0f766e;
      |  --mermoid-font-family: ui-monospace, monospace;
      |}
      |.node-shape { stroke-width: 3; }
      |.node-rhombus .node-shape { fill: #fef9c3; }
      |.edge-label { font-size: 11px; }
      |""".stripMargin

  def doc = page("Custom CSS")(
    md"""
`RenderConfig.customStylesheet` is merged over the chosen theme. This is the whole styling story: mermoid has no theme
object to subclass and no per-shape configuration knobs — you write CSS, and it wins.
""",
    section("From a CSS string")(
      md"""
`CssParser.parse` returns `Either[String, Stylesheet]`. It handles `:root` variable blocks, class/id/element/compound/
descendant selectors, pseudo-classes, hex colours, lengths, numbers, quoted strings, `var()` with fallbacks, composite
values, and `/* comments */`.
""",
      example {
        val sheet = CssParser.parse(overrides).getOrElse(throw new AssertionError("bad css"))
        MermoidUi.diagram(pipeline, RenderConfig(customStylesheet = Some(sheet), resolveVariables = false))
      },
      md"""
That is the same diagram as the Default theme renders — only the stylesheet changed. Note `resolveVariables = false`
here: the overridden variables stay as `var()` references so anything further up the cascade can override them again.
""",
      exampleValue {
        CssParser.parse(overrides).map(s => (s.variables.size, s.rules.size))
      }.assert(r => assertTrue(r == Right((4, 3)))),
    ),
    section("Merge semantics")(
      md"""
`Stylesheet.merge(base, overrides)`:

- **variables** — map union, `overrides` winning per key
- **rules** — `base.rules ++ overrides.rules`, in that order

Rules append rather than replace, so a custom rule with the same selector as a built-in one relies on ordinary CSS source
order to win. That is deliberate: it means you can override one declaration without restating the rest of the rule.
""",
      exampleValue {
        import _root_.mermoid.css.*
        val base     = Theme.toStylesheet(ThemeName.Default)
        val mine     = CssParser.parse(".node-shape { stroke-width: 4; }").getOrElse(Stylesheet.empty)
        val merged   = Stylesheet.merge(base, mine)
        val rendered = CssRenderer.render(merged, resolveVariables = false)
        // The built-in .node-shape rule still stands; ours follows it and wins on source order.
        (
          merged.rules.size == base.rules.size + 1,
          rendered.indexOf("stroke-width: 2") < rendered.lastIndexOf("stroke-width: 4"),
        )
      }.assert(r => assertTrue(r == ((true, true)))),
    ),
    section("Building the AST directly")(
      md"""
For CSS generated in Scala, skip the parser and build `Stylesheet` values. The AST is small: `CssValue`, `CssSelector`,
`CssDeclaration`, `CssRule`, `Stylesheet` — all plain case classes and enums, so a stylesheet can be computed, folded
over, or derived from application data.
""",
      example {
        import _root_.mermoid.css.*
        // A per-status palette computed in Scala rather than written as CSS text — one rule per entry,
        // matching the `classDef`-assigned class names in the diagram source.
        val statusColors = List("ok" -> "#16a34a", "warn" -> "#ca8a04", "fail" -> "#dc2626")
        val rules        = statusColors.map { (name, color) =>
          CssRule(
            CssSelector.Descendant(CssSelector.Class(name), CssSelector.Class("node-shape")),
            List(CssDeclaration("stroke", CssValue.Color(color)), CssDeclaration("stroke-width", CssValue.Number(3))),
          )
        }
        MermoidUi.diagram(
          """flowchart LR
            |    A[Healthy] --> B[Degraded]
            |    B --> C[Down]
            |    class A ok
            |    class B warn
            |    class C fail
            |""".stripMargin,
          RenderConfig(customStylesheet = Some(Stylesheet(rules = rules))),
        )
      },
    ),
    section("classDef, class and style")(
      md"""
The three in-diagram styling statements interact with a custom stylesheet like this:

| Statement | Where it lands | Wins against |
|---|---|---|
| `classDef n p:v` | a CSS rule appended after the custom rules | earlier rules with equal specificity |
| `class A n` | the node's `class` attribute | — it selects, it doesn't style |
| `style A p:v` | an inline `style` attribute on the node group | every stylesheet rule |

`style` becoming an inline attribute means it beats your CSS. If you need a diagram whose appearance is fully controlled
from the outside, prefer `class` + `classDef`, or strip `style` statements before rendering.
""",
      exampleValue {
        import _root_.mermoid.*
        MermaidParser
          .parse("flowchart LR\n  classDef hot fill:#f00\n  A[a] --> B[b]\n  class B hot\n")
          .map(SvgRenderer.render(_))
          .map { svg =>
            // classDef rules are appended last, after the theme's and the custom sheet's.
            svg.indexOf(".node-shape {") < svg.indexOf(".hot {")
          }
      }.assert(r => assertTrue(r == Right(true))),
    ),
    section("Styling a diagram already on the page")(
      md"""
Nothing above requires a re-render. Because every element carries a stable class and id
([SVG structure](svg-structure.html)), a stylesheet in the host page reaches into the diagram:

```css
/* dim everything except the critical path */
#chart .node { opacity: 0.4; }
#chart .node.critical { opacity: 1; }
#chart #edge-happy .edge-line { stroke: #16a34a; stroke-width: 4; }

/* respond to the reader's preference — no second render */
@media (prefers-color-scheme: dark) {
  #chart { --mermoid-main-bkg: #1f2020; --mermoid-text: #e0e0e0; }
}
```

The `@media` rule only bites when the diagram was rendered with `resolveVariables = false`. That is the trade-off from
[Theming](theming.html): resolved output is portable, `var()` output is themeable.
"""
    ),
  )
end CustomCss
