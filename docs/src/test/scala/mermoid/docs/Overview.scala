package mermoid.docs

import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** What mermoid is and why the SVG-with-CSS shape matters. */
object Overview extends DocSpecSuite:

  private val hello =
    """flowchart LR
      |    Source[".mmd source"] --> Parser[MermaidParser]
      |    Parser --> Ast[Diagram AST]
      |    Ast --> Layout[Layout]
      |    Layout --> Tree[SvgNode tree]
      |    Tree --> Svg([SVG string])
      |""".stripMargin

  def doc = page("Overview")(
    md"""
**mermoid** is a Scala 3 library that parses [Mermaid](https://mermaid.js.org) diagram syntax and renders SVG. It
cross-builds for the JVM and Scala.js, and depends on nothing but [fastparse](https://github.com/com-lihaoyi/fastparse).

The diagram below is not a screenshot. It was parsed and rendered by mermoid while this page was being built, and the
same call is asserted by the test suite.
""",
    example {
      MermoidUi.diagram(hello)
    },
    section("Why not mermaid.js")(
      md"""
mermaid.js is excellent, and it is a **runtime**: you ship a JavaScript bundle, the browser parses your diagram source
after load, and the SVG appears when the script has run. That shape costs you a few things.

| | mermaid.js | mermoid |
|---|---|---|
| When rendering happens | in the browser, after load | at build time (or wherever you call it) |
| What ships to the page | JS bundle + diagram source | the finished SVG |
| Styling | theme variables, JS-side | plain CSS on classes and ids |
| Static hosting | needs JS enabled | works with JS off |
| Server-side use | needs headless Chrome or Node | a function call |
| Scala integration | shell out to `mmdc` | a dependency |

The consequence that matters most is **styling**. mermoid emits a `<style>` block built from CSS custom properties and
tags every element with a stable class and id, so restyling a rendered diagram is a stylesheet change — no re-render, no
theme-object rebuild, and it composes with the rest of your site's CSS. See [Theming](theming.html) and
[Custom CSS](custom-css.html).
"""
    ),
    section("What you get back")(
      md"""
`SvgRenderer.render` returns a `String` — the whole SVG document, ready to write to a file or inline in a page.

`SvgRenderer.renderTree` returns the `SvgNode` tree *before* serialization. That is the integration point: a UI
framework can map it to its own element type, a post-processor can rewrite it, a different serializer can take it. This
site does exactly that — its diagrams go through `SvgNode → ascent UI`, not through the string.
""",
      exampleValue {
        import mermoid.*
        MermaidParser
          .parse("flowchart TD\n  A[Hello] --> B((World))")
          .map(SvgRenderer.renderTree(_))
          .map {
            case SvgNode.Element(tag, attrs, children) =>
              s"<$tag> with ${attrs.size} attributes and ${children.size} children"
            case other => other.toString
          }
      }.assert(r => assertTrue(r == Right("<svg> with 4 attributes and 5 children"))),
    ),
    section("Status")(
      md"""
Pre-1.0, on [early-semver](https://www.scala-lang.org/blog/2021/02/16/preventing-version-conflicts-with-versionscheme.html):
`0.x` releases may break binary compatibility. Flowcharts and `stateDiagram-v2` are implemented; sequence, class, ER and
Gantt diagrams are not. The [README](https://github.com/early-effect/mermoid#supported-syntax) has the honest feature
table.
"""
    ),
  )
end Overview
