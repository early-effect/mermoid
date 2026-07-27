package mermoid.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.site.*
import zio.*

import java.nio.file.{Files, Path, Paths, StandardCopyOption}

/** Docs-as-tests site builder (Test classpath; `docs/specularSite`).
  *
  * Every diagram on the site is rendered by the real renderer while the page is built, and asserted by `sbt test` — so
  * a diagram that stops parsing or stops producing the expected structure is a red check, not a broken picture.
  *
  * Interactive remount: `specularJsLink` writes `target/specular-client-js.path`; [[afterBuild]] copies that bundle to
  * `assets/client.js` (Specular dogfood pattern).
  */
object BuildSite extends DocsSite:

  def pages = Vector(
    Overview.doc,
    QuickStart.doc,
    Flowcharts.doc,
    StateDiagrams.doc,
    Interactive.doc,
    Theming.doc,
    CustomCss.doc,
    SvgStructure.doc,
    Cli.doc,
  )

  override def site: SiteModel =
    val m       = meta
    val branded = EarlyEffectTheme.brand(super.site)
    branded.copy(
      clientScript = Some("assets/client.js"),
      summaryMarkdown = Some(
        """**mermoid** parses [Mermaid](https://mermaid.js.org) diagram syntax and renders SVG in Scala 3, on the JVM
and in the browser via Scala.js. No headless Chrome, no Node build step, no JavaScript at page load.

The output is **styled entirely by CSS**. Every element carries a stable class and id (`node-Start`, `edge-a-b-0`,
`note-Idle-0`), and the stylesheet ships in a `<style>` block built from CSS custom properties, so you restyle a
diagram with a stylesheet instead of re-rendering it. Four built-in themes, or bring your own CSS.

`SvgRenderer.render` gives you a `String`; `SvgRenderer.renderTree` / `DiagramLayout.scene` are the paint-agnostic
contracts. **`mermoid-ascent`** paints hybrid HTML+SVG with reactive reflow for Specular and any ascent app.

fastparse is the only dependency of `mermoid` core; `mermoid-ascent` adds ascent.

Guide: Quick start → Flowcharts → State diagrams → Interactive → Theming → Custom CSS → SVG structure → CLI.
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
          "mermoid-ascent (hybrid / interactive)",
          s"""libraryDependencies += "${m.organization}" %% "mermoid-ascent" % "${m.version}"
libraryDependencies += "${m.organization}" %%% "mermoid-ascent" % "${m.version}"""",
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
    EarlyEffectTheme.writeLogo(out) *> copyClientBundle(out)

  private def copyClientBundle(out: Path): Task[Unit] =
    ZIO.attempt {
      val dest = out.resolve("assets/client.js")
      val src  = findClientJs.getOrElse {
        throw new RuntimeException(
          "JS client not linked; run docs/specularSite (or docsJS/fastLinkJS) first. " +
            s"Looked for marker ${clientJsMarker} and under ${repoRoot.resolve("target/out")}"
        )
      }
      Files.createDirectories(dest.getParent)
      Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING)
      ()
    }

  private def clientJsMarker: Path =
    repoRoot.resolve("target/specular-client-js.path")

  private def findClientJs: Option[Path] =
    readMarker.orElse(walkTargetOut)

  private def readMarker: Option[Path] =
    val marker = clientJsMarker
    if !Files.isRegularFile(marker) then None
    else
      val line = Files.readString(marker).nn.trim
      if line.isEmpty then None
      else
        val path = Paths.get(line)
        Option.when(Files.isRegularFile(path))(path)

  private def walkTargetOut: Option[Path] =
    val outRoot = repoRoot.resolve("target/out")
    if !Files.isDirectory(outRoot) then None
    else
      val stream = Files.walk(outRoot)
      try
        val found = stream
          .filter { p =>
            val s = p.toString.replace('\\', '/')
            s.endsWith("mermoid-docs-fastopt/main.js")
          }
          .findFirst()
        if found.isPresent then Some(found.get.nn) else None
      finally stream.close()
    end if
  end walkTargetOut

  private def repoRoot: Path =
    Iterator
      .iterate(Paths.get("").toAbsolutePath.nn)(p => Option(p.getParent).orNull)
      .takeWhile(_ != null)
      .find(p => Files.exists(p.resolve("build.sbt")))
      .getOrElse(Paths.get("").toAbsolutePath.nn)
end BuildSite
