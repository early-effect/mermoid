package mermoid.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.site.*
import zio.*

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
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

  /** Sidebar order. [[Front]] is the index, not a second copy of this list. */
  private val guide = Vector(
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

  private val frontPage = Front.doc

  def pages = frontPage +: guide

  override def site(settings: DocsSettings): SiteModel =
    val m = settings.meta
    EarlyEffectTheme
      .brand(super.site(settings))
      .copy(
        clientScript = Some("assets/client.js"),
        nav = Some(NavModel.flat(guide)),
        pages = pages,
        summaryMarkdown = None,
        brand = Some(
          Brand(
            name = m.title.getOrElse("mermoid"),
            links = Vector(EarlyEffectTheme.github("https://github.com/early-effect/mermoid")),
          )
        ),
      )

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    EarlyEffectTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): IO[SiteError, Unit] =
    val _ = result
    // Specular always writes index.html as a summary plus a second copy of the nav.
    // The front DocPage is the index. Copy it over that file. It is not a sidebar item.
    val front   = out.resolve(s"${frontPage.slug}.html")
    val index   = out.resolve("index.html")
    val promote =
      ZIO.attemptBlockingIO(Files.copy(front, index, StandardCopyOption.REPLACE_EXISTING)).mapError { err =>
        SiteError.WriteFailed(index, err)
      }
    promote *> EarlyEffectTheme.writeLogo(out) *> copyClientBundle(out)

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

  private def repoRoot: Path =
    Iterator
      .unfold(Option(Paths.get("").toAbsolutePath))(_.map(p => (p, Option(p.getParent))))
      .find(p => Files.exists(p.resolve("build.sbt")))
      .getOrElse(Paths.get("").toAbsolutePath)
end BuildSite
