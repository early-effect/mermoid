package mermoid.docs

import mermoid.ascent.MermoidAscent
import _root_.mermoid.RenderConfig
import _root_.mermoid.css.{CssParser, PaintClass, Stylesheet}
import specular.*
import specular.ziotest.DocSpecSuite

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
        MermoidAscent.svgDiagram(pipeline, RenderConfig(customStylesheet = Some(sheet), resolveVariables = false))
      },
      md"""
That is the same diagram as the Default theme renders — only the stylesheet changed. Note `resolveVariables = false`
here: the overridden variables stay as `var()` references so anything further up the cascade can override them again.
""",
    ),
    section("Merge semantics")(
      md"""
`Stylesheet.merge(base, overrides)`:

- **variables** — map union, `overrides` winning per key
- **rules** — `base.rules ++ overrides.rules`, in that order

Rules append rather than replace, so a custom rule with the same selector as a built-in one relies on ordinary CSS source
order to win. That is deliberate: it means you can override one declaration without restating the rest of the rule.

The second diagram below only adds `stroke-width: 4` on `.node-shape`. The rest of the Default theme is unchanged.
""",
      example {
        MermoidAscent.svgDiagram(pipeline)
      },
      example {
        val mine = CssParser.parse(".node-shape { stroke-width: 4; }").getOrElse(Stylesheet.empty)
        MermoidAscent.svgDiagram(pipeline, RenderConfig(customStylesheet = Some(mine)))
      },
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
            CssSelector.Descendant(CssSelector.Class(name), PaintClass.NodeShape.selector),
            List(CssDeclaration("stroke", CssValue.Color(color)), CssDeclaration("stroke-width", CssValue.Number(3))),
          )
        }
        MermoidAscent.svgDiagram(
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
The in-diagram styling statements interact with a custom stylesheet like this. Later rows win.

| Layer | Where it lands | What it loses to |
|---|---|---|
| Hybrid chrome layout | `.mermoid-node` and `.mermoid-node .node-shape` (position, inset, z-index) | nothing written as paint. These keep the shape inside the button |
| Hybrid chrome paint | `:where(.mermoid-node)` for `color`, `:where(.mermoid-node) .node-shape` for `background` and `border` | the theme, `classDef`, and `style`. `:where` adds no specificity |
| Theme and `customStylesheet` | the embedded stylesheet, before `classDef` | `classDef` and `style` |
| `classDef n p:v` | SVG: `.n` and `.n .node-shape`. Hybrid also copies those onto `.mermoid-node.n` and `.mermoid-node.n .node-shape`, which outrank `.mermoid-node .node-shape` | `style` |
| `class A n` / `A:::n` | the node's `class` attribute. It selects. It does not paint | |
| `style A p:v` | inline `style`. `fill` and `stroke` land on the shape (`background`, `border-color`). `color` lands on the node, because the label is a sibling of the shape | an `!important` rule in the host page |

These statements work on flowcharts and `stateDiagram-v2`. `fill` becomes `background` on the inner `.node-shape`, and
`stroke` becomes `border-color`, in both SVG and hybrid HTML. Hosts can keep writing SVG paint properties.

Two hybrid shapes do not take `classDef` paint. A rhombus draws its visible fill on `.mermoid-node-diamond-fill`, and
the `[*]` marker keeps `.mermoid-node.start-end .node-shape`. `style` on a rhombus still sets inline background on
`.node-shape`, which is the diamond's stroke plate, not the inner fill.
""",
      example {
        MermoidAscent.diagram(
          """flowchart LR
            |  classDef warn fill:#4a4030,stroke:#e0c070
            |  A[Tired] --> B[Zipx]
            |  class A warn
            |""".stripMargin
        )
      },
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
