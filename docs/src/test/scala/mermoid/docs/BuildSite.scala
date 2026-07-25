package mermoid.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.site.*
import zio.*

import java.nio.file.Path

/** Docs-as-tests site builder (Test classpath; `docs/specularSite`).
  *
  * Every diagram on the site is rendered by the real renderer while the page is built, and asserted by `sbt test` — so
  * a diagram that stops parsing or stops producing the expected structure is a red check, not a broken picture.
  */
object BuildSite extends DocsSite:

  def pages = Vector(
    Overview.doc,
    QuickStart.doc,
    Flowcharts.doc,
    StateDiagrams.doc,
    Theming.doc,
    CustomCss.doc,
    SvgStructure.doc,
    Cli.doc,
  )

  override def site: SiteModel =
    val m       = meta
    val branded = EarlyEffectTheme.brand(super.site)
    branded.copy(
      summaryMarkdown = Some(
        """**mermoid** parses [Mermaid](https://mermaid.js.org) diagram syntax and renders SVG — in Scala 3, on the JVM
and in the browser via Scala.js. No headless Chrome, no Node build step, no JavaScript at page load.

The output is **styled entirely by CSS**. Every element carries a stable class and id (`node-Start`, `edge-a-b-0`,
`note-Idle-0`), and the stylesheet ships in a `<style>` block built from CSS custom properties — so you restyle a
diagram with a stylesheet instead of re-rendering it. Four built-in themes, or bring your own CSS.

`SvgRenderer.render` gives you a `String`; `SvgRenderer.renderTree` gives you the `SvgNode` tree, which is the
integration point for anything that builds its own markup (a UI framework, a post-processor, a different serializer).

fastparse is the only dependency.

Guide: Quick start → Flowcharts → State diagrams → Theming → Custom CSS → SVG structure → CLI.
"""
      ),
      installSnippets = Vector(
        ArtifactKind.defaultInstall(m, ArtifactKind.Library),
        CodeSnippet(
          "Scala.js",
          s"""// the same artifact cross-builds for Scala.js
libraryDependencies += "${m.organization}" %%% "${m.name}" % "${m.version}"""",
        ),
        CodeSnippet(
          "Render a diagram",
          """import mermoid.*

val svg = MermaidParser.parse("flowchart TD\\n  A[Start] --> B[Done]")
  .map(SvgRenderer.render(_))""",
        ),
      ),
      brand = Some(
        Brand(
          name = m.title.getOrElse("mermoid"),
          links = Vector(EarlyEffectTheme.github("https://github.com/early-effect/mermoid")),
        )
      ),
    )
  end site

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    EarlyEffectTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): Task[Unit] =
    val _ = result
    EarlyEffectTheme.writeLogo(out)
end BuildSite
