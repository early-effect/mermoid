package mermoid.docs

import mermoid.ascent.MermoidAscent
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** What mermoid is and why the SVG-with-CSS shape matters. */
object Overview extends DocSpecSuite:

  private val hello =
    """flowchart LR
      |    Source[".mmd source"] --> Parser[MermaidParser]
      |    Parser --> Ast[Diagram AST]
      |    Ast --> Scene[DiagramScene]
      |    Scene --> Tree[SvgNode tree]
      |    Tree --> Svg([SVG string])
      |""".stripMargin

  def doc = page("Overview")(
    md"""
**mermoid** is a Scala 3 library that parses [Mermaid](https://mermaid.js.org) flowchart and `stateDiagram-v2` syntax and
renders SVG. It cross-builds for the JVM and Scala.js. Core depends on nothing but
[fastparse](https://github.com/com-lihaoyi/fastparse).

Two published artifacts:

| Artifact | Use when |
|---|---|
| **`mermoid`** | You want a self-contained SVG string or `SvgNode` tree |
| **`mermoid-ascent`** | You want hybrid HTML nodes + SVG edges, selection, tooltips, and viewport reflow |

The diagram below is not a screenshot. It was parsed and rendered by mermoid while this page was being built, and the
same call is asserted by the test suite.

For hover, selection, tooltips, and **reactive reflow**, open [Interactive](interactive.html) (or run `sbt docsPreview`
and click through that page).
""",
    example {
      MermoidAscent.svgDiagram(hello)
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
Three layers, pick the one that matches your host:

| API | Returns | Typical host |
|---|---|---|
| `SvgRenderer.render` | SVG `String` | files, static pages, emails |
| `SvgRenderer.renderTree` | `SvgNode` tree | frameworks that map trees to DOM |
| `DiagramLayout.scene` | `DiagramScene` | custom painters, metrics, responsive hosts |

This site maps inert SVG trees for structure docs, and uses **`mermoid-ascent`** for hybrid HTML nodes + SVG edges with
selection and viewport reflow; see [Interactive](interactive.html).
""",
      exampleValue {
        import _root_.mermoid.*
        MermaidParser
          .parse("flowchart TD\n  A[Hello] --> B((World))")
          .map(SvgRenderer.renderTree(_))
          .map {
            case SvgNode.Element(tag, attrs, children) =>
              s"<$tag> with ${attrs.size} attributes and ${children.size} children"
            case other => other.toString
          }
      }.assert(r => assertTrue(r == Right("<svg> with 4 attributes and 6 children"))),
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
