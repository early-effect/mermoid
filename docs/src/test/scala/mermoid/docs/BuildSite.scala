package mermoid.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.site.*
import zio.*

import java.nio.file.{Files, Path, Paths}
import scala.jdk.OptionConverters.*

/** Docs-as-tests site builder (Test classpath; `docs/specularSite`).
  *
  * Every diagram on the site is rendered by the real renderer while the page is built, and asserted by `sbt testFull`,
  * so a diagram that stops parsing or stops producing the expected structure is a red check, not a broken picture.
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
    ClassDiagrams.doc,
    ErDiagrams.doc,
    SequenceDiagrams.doc,
    Interactive.doc,
    Responsive.doc,
    SpecularIllustrations.doc,
    Theming.doc,
    CustomCss.doc,
    SvgStructure.doc,
    Cli.doc,
  )

  override def site(settings: DocsSettings): SiteModel =
    val m       = settings.meta
    val branded = EarlyEffectTheme.brand(super.site(settings))
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

Guide: Quick start → Flowcharts → State diagrams → Sequence diagrams → Interactive → Responsive layout → Theming → Custom CSS → SVG structure → CLI.
"""
      ),
      installSnippets = Vector(
        ArtifactKind.defaultInstall(m, ArtifactKind.Library),
        CodeSnippet(
          "Scala.js",
          s"""// the same artifact cross-builds for Scala.js
libraryDependencies += "${m.organization}" %%% "${m.name}" % "${m.docsVersion}"""",
        ),
        CodeSnippet(
          "mermoid-ascent (hybrid / interactive)",
          s"""libraryDependencies += "${m.organization}" %% "mermoid-ascent" % "${m.docsVersion}"
libraryDependencies += "${m.organization}" %%% "mermoid-ascent" % "${m.docsVersion}"""",
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

  override def afterBuild(out: Path, result: SiteOutput): IO[SiteError, Unit] =
    val _ = result
    EarlyEffectTheme.writeLogo(out) *> copyClientBundle(out)

  /** The linked client named by [[clientJsMarker]], else the first fastopt bundle under `target/out`. */
  private def copyClientBundle(out: Path): IO[SiteError, Unit] =
    ZIO
      .attemptBlocking(findClientJs)
      .orElseSucceed(None)
      .map(_.getOrElse(clientJsMarker))
      .flatMap(SiteAssets.copyFile(_, out.resolve("assets/client.js")))

  private def clientJsMarker: Path =
    repoRoot.resolve("target/specular-client-js.path")

  private def findClientJs: Option[Path] =
    readMarker.orElse(walkTargetOut)

  private def readMarker: Option[Path] =
    val marker = clientJsMarker
    if !Files.isRegularFile(marker) then None
    else
      val line = Files.readString(marker).trim
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
        found.toScala
      finally stream.close()
    end if
  end walkTargetOut

  private def repoRoot: Path =
    Iterator
      .unfold(Option(Paths.get("").toAbsolutePath))(_.map(p => (p, Option(p.getParent))))
      .find(p => Files.exists(p.resolve("build.sbt")))
      .getOrElse(Paths.get("").toAbsolutePath)
end BuildSite
