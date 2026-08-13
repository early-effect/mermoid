package mermoid.docs

import mermoid.ascent.MermoidAscent
import _root_.mermoid.RenderConfig
import _root_.mermoid.css.{CssValue, ThemeName, ThemeVar}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** The four built-in themes, and the CSS custom properties behind them. */
object Theming extends DocSpecSuite:

  private val sample =
    """flowchart LR
      |    A[(Source)] --> B{Route}
      |    B -->|hot| C([Cache])
      |    B -.->|cold| D[[Compute]]
      |""".stripMargin

  private def themed(theme: ThemeName) = MermoidAscent.svgDiagram(sample, RenderConfig(theme = theme))

  def doc = page("Theming")(
    md"""
`RenderConfig(theme = …)` picks one of ${ThemeName.values.size} palettes (`ThemeName`). Each is a `ThemeColors` record
turned into a stylesheet: one CSS custom property per `ThemeVar` on `:root`, plus the rules that consume them.
""",
    section("Default")(example(themed(ThemeName.Default))),
    section("Dark")(example(themed(ThemeName.Dark))),
    section("Forest")(example(themed(ThemeName.Forest))),
    section("Neutral")(example(themed(ThemeName.Neutral))),
    section("The variables")(
      md"""
Every colour and font in the output comes from one of these, so overriding one variable in your own stylesheet restyles
everything that uses it. The table is `ThemeVar`: each member carries its CSS name and description.

${ThemeVar.markdownTable}

The `primary`/`secondary`/`tertiary` triples (colour, border, text) are not consumed by the built-in rules; they exist
so a custom stylesheet can pick theme-consistent colours without hardcoding hexes.
""",
      exampleValue {
        import _root_.mermoid.css.*
        val sheet = Theme.toStylesheet(ThemeName.Dark)
        (sheet.variables.size, sheet.get(ThemeVar.NodeBorder))
      }.assert(r => assertTrue(r == ((ThemeVar.values.size, Some(CssValue.Color("#81B1DB")))))),
    ),
    section("resolveVariables")(
      md"""
`RenderConfig.resolveVariables` decides whether the rules reference the variables or the substituted values. It defaults
to `true`.

- **`true`** — `fill: #1f2020`. Self-contained: the SVG renders identically wherever it lands, including contexts that
  strip `<style>` or don't cascade (some email clients, some image pipelines). The `:root` block is still emitted.
- **`false`** — `fill: var(--mermoid-main-bkg)`. Overridable: set the variable anywhere up the cascade and the diagram
  follows. This is what you want when the diagram is inline in a page you control.
""",
      exampleValue {
        import _root_.mermoid.*
        val diagram  = MermaidParser.parse(sample).getOrElse(throw new AssertionError("unparseable"))
        val resolved = SvgRenderer.render(diagram, RenderConfig(theme = ThemeName.Dark))
        val varForm  = SvgRenderer.render(diagram, RenderConfig(theme = ThemeName.Dark, resolveVariables = false))
        List(
          s"resolved contains a literal hex fill: ${resolved.contains("fill: #1f2020")}",
          s"resolved contains var(): ${resolved.contains("fill: var(")}",
          s"var form contains var(): ${varForm.contains("fill: var(--mermoid-main-bkg)")}",
        ).mkString("\n")
      }.assert(s =>
        assertTrue(
          s.contains("resolved contains a literal hex fill: true"),
          s.contains("resolved contains var(): false"),
          s.contains("var form contains var(): true"),
        )
      ),
      md"""
Overriding a variable from the page, with `resolveVariables = false`:

```css
.diagram-container {
  --mermoid-main-bkg: #eef6ff;
  --mermoid-node-border: #2b6cb0;
}
```

No re-render — the diagram already on the page restyles.
""",
    ),
    section("Beyond the four")(
      md"""
The built-in themes are a convenience, not a ceiling. `Theme.toStylesheet(colors: ThemeColors)` accepts a palette you
built yourself, and a whole stylesheet can be merged over any theme — see [Custom CSS](custom-css.html).
""",
      exampleValue {
        import _root_.mermoid.css.*
        val mine  = Theme.colors(ThemeName.Neutral).copy(nodeBorder = "#d97706", lineColor = "#92400e")
        val sheet = Theme.toStylesheet(mine)
        sheet.get(ThemeVar.NodeBorder)
      }.assert(v => assertTrue(v == Some(CssValue.Color("#d97706")))),
      example {
        import _root_.mermoid.css.*
        val mine = Theme.colors(ThemeName.Neutral).copy(nodeBorder = "#d97706", lineColor = "#92400e")
        // Theme.toStylesheet(ThemeColors) is the whole extension point: a palette in, a stylesheet out,
        // merged over the chosen theme by customStylesheet.
        MermoidAscent.svgDiagram(
          sample,
          RenderConfig(theme = ThemeName.Neutral, customStylesheet = Some(Theme.toStylesheet(mine))),
        )
      },
    ),
  )
end Theming
