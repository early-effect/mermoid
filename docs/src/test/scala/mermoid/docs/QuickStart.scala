package mermoid.docs

import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** Add the dependency, parse, render, write. */
object QuickStart extends DocSpecSuite:

  def doc = page("Quick start")(
    md"""
Add the dependency, parse a diagram, render it. Three calls, no build step, no browser.
""",
    section("Install")(
      md"""
```scala
// build.sbt — JVM
libraryDependencies += "rocks.earlyeffect" %% "mermoid" % "<version>"

// Scala.js (or a cross-built project)
libraryDependencies += "rocks.earlyeffect" %%% "mermoid" % "<version>"
```

Pre-1.0 on early-semver: pin the exact version and read the release notes before bumping the minor.
"""
    ),
    section("Parse and render")(
      md"""
`MermaidParser.parse` returns `Either[String, Diagram]` — the `Left` is the parse error, which you should surface rather
than swallow. `SvgRenderer.render` turns a `Diagram` into the SVG document.
""",
      exampleValue {
        import mermoid.*

        val source = """flowchart LR
                       |    A[Start] --> B{Ready?}
                       |    B -->|yes| C([Ship it])
                       |    B -->|no| A
                       |""".stripMargin

        MermaidParser.parse(source).map(SvgRenderer.render(_)) match
          case Right(svg) => s"${svg.length} characters of SVG, starting ${svg.take(4)}"
          case Left(err)  => s"parse error: $err"
      }.assert(s => assertTrue(s.endsWith("starting <svg"), s.contains("characters of SVG"))),
      md"""
That same diagram, rendered:
""",
      example {
        MermoidUi.diagram("""flowchart LR
                            |    A[Start] --> B{Ready?}
                            |    B -->|yes| C([Ship it])
                            |    B -->|no| A
                            |""".stripMargin)
      },
    ),
    section("Write it to a file")(
      md"""
On the JVM, the whole job is one `Files.writeString`:

```scala
import mermoid.*
import java.nio.file.{Files, Path}

def renderToFile(mmd: Path, svg: Path): Either[String, Unit] =
  MermaidParser
    .parse(Files.readString(mmd))
    .map(d => Files.writeString(svg, SvgRenderer.render(d)))
    .map(_ => ())
```

Or skip the code and use the [CLI](cli.html).
"""
    ),
    section("Render in the browser")(
      md"""
The same artifact cross-builds for Scala.js, so a Scala.js app can render a diagram client-side without pulling in
mermaid.js:

```scala
import mermoid.*
import org.scalajs.dom

MermaidParser.parse(source).foreach { d =>
  dom.document.getElementById("chart").innerHTML = SvgRenderer.render(d)
}
```

If you build a virtual DOM rather than setting `innerHTML`, use `SvgRenderer.renderTree` and map the
[SvgNode tree](svg-structure.html) to your framework's element type — no string round-trip.

Rendering is deterministic and platform-independent: the same source and config produce byte-identical SVG on the JVM
and on Scala.js, which is what lets you render server-side and hydrate client-side without a mismatch.
"""
    ),
    section("Configure")(
      md"""
`RenderConfig` is the one knob:

```scala
RenderConfig(
  layout            = LayoutConfig(),           // spacing, font sizes, shape geometry
  theme             = css.ThemeName.Default,    // Default | Dark | Forest | Neutral
  customStylesheet  = None,                     // merged over the theme
  resolveVariables  = true,                     // false keeps var(--mermoid-*) in the output
)
```

See [Theming](theming.html) for themes and [Custom CSS](custom-css.html) for `customStylesheet` and `resolveVariables`.
"""
    ),
  )
end QuickStart
