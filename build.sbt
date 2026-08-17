import org.scalajs.linker.interface.ModuleKind
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

MyVersions.settings

val scala3Version: String = MyVersions.scala

// sbt 2.x scopes bare build.sbt settings to ThisBuild, so these apply build-wide to every module.
organization         := "rocks.earlyeffect"
organizationName     := "Early Effect"
organizationHomepage := Some(url("https://www.earlyeffect.rocks"))
versionScheme        := Some("early-semver")
// No hardcoded version — sbt-dynver-ci derives it: clean tag -> 0.1.0, else <last-tag>-ci (cache-stable).

homepage := Some(url("https://github.com/early-effect/mermoid"))
licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
scmInfo  := Some(
  ScmInfo(
    url("https://github.com/early-effect/mermoid"),
    "scm:git@github.com:early-effect/mermoid.git",
  )
)
developers := List(
  Developer(
    id = "russwyte",
    name = "Russ White",
    email = "356303+russwyte@users.noreply.github.com",
    url = url("https://github.com/russwyte"),
  )
)

// Publishing targets the Sonatype Central Portal, built into sbt 2.x (no sbt-sonatype).
// Snapshots go to Central's snapshot repo; releases stage locally and are promoted by `sonaRelease`.
publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
publishMavenStyle    := true
pomIncludeRepository := { _ => false }

// CI-only publishing: the signing key hex comes from the PGP_KEY_HEX env var (a shared early-effect
// org secret). There is no real key in this file — the MISSING_KEY_HEX sentinel keeps the build
// loadable for local compile/test but makes signing fail loudly if anyone publishes off-CI.
usePgpKeyHex(sys.env.getOrElse("PGP_KEY_HEX", "MISSING_KEY_HEX"))

// zipx CI: builtin fmt / workflow-check / advisories / test (testFull) run in parallel, then Central + Pages.
zipxJavaVersion      := JdkVersion("25")
zipxWorkflowDispatch := true
zipxCapabilities ++= Seq(
  ZipxCentral.release,
  ZipxDocs.pages(),
)

val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-Wunused:all",
)

val scalaVersions = Seq(scala3Version)

/** zio-test deps. `library()` resolves `%%` at each module's platform. ZTestFramework registers itself via zio-test-sbt,
  * so no testFrameworks wiring is needed.
  */
val zioTestSettings = MyVersions.zioTests

lazy val root = (project in file("."))
  .aggregate((core.projectRefs ++ ascent.projectRefs ++ cli.projectRefs ++ docs.projectRefs)*)
  .settings(
    // sbt 2.x derives output directories from `name`, so the aggregate cannot share `core`'s name.
    name           := "mermoid-root",
    publish / skip := true,
    test / skip    := true,
  )

// --- mermoid : the parser, layout and SVG renderer. Published; fastparse only.
lazy val core = (projectMatrix in file("core"))
  .settings(
    name        := "mermoid",
    description := "Mermaid-compatible diagram to SVG renderer for Scala 3, themed with real CSS",
    scalacOptions ++= commonScalacOptions,
    MyVersions.parserLib,
    zioTestSettings,
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(scalaVersions = scalaVersions)

val javaTimePolyfill = MyVersions.javaTime

// --- mermoid-ascent : hybrid HTML+SVG ascent painter with reactive reflow. Published; depends on ascent.
lazy val ascent = (projectMatrix in file("ascent"))
  .dependsOn(core)
  .settings(
    name        := "mermoid-ascent",
    description := "Ascent UI painter for mermoid diagrams (hybrid HTML nodes, SVG edges, reactive reflow)",
    scalacOptions ++= commonScalacOptions,
    MyVersions.ascentLib,
  )
  .jvmPlatform(
    scalaVersions,
    Nil,
    (p: Project) =>
      p.settings(
        MyVersions.ascentHtmlLib,
        zioTestSettings,
      ),
  )
  .jsPlatform(
    scalaVersions,
    Nil,
    (p: Project) =>
      p.settings(
        javaTimePolyfill,
        MyVersions.ascentJsLib,
        // SSR round-trip specs need ascent-html (JVM-only).
        Test / skip    := true,
        Test / sources := Nil,
      ),
  )

// --- mermoid-cli : fat-jar renderer for .mmd files. JVM only (java.nio.file), never published.
lazy val cli = (projectMatrix in file("cli"))
  .dependsOn(core)
  .settings(
    name            := "mermoid-cli",
    publish / skip  := true,
    publishArtifact := false, // zipx derives publish jobs from publishArtifact
    scalacOptions ++= commonScalacOptions,
    MyVersions.zioLib,
    zioTestSettings,
    Compile / mainClass        := Some("mermoid.cli.MermoidCli"),
    assembly / mainClass       := Some("mermoid.cli.MermoidCli"),
    assembly / assemblyJarName := "mermoid-cli.jar",
    // The fix half of SvgOutputSpec's "committed .svg is current" check: that test fails when a
    // renderer change makes the gallery stale, and this task is how you make it pass again.
    // Writing files makes this inherently uncacheable, hence Def.uncached.
    regenerateExamples := Def.taskDyn {
      val dir   = (ThisBuild / baseDirectory).value / "examples"
      val files = (dir * "*.mmd").get().map(_.getAbsolutePath).sorted.mkString(" ")
      Def.uncached((Compile / runMain).toTask(s" mermoid.cli.MermoidCli $files"))
    }.value,
    layoutGallery := Def.taskDyn {
      val base  = (ThisBuild / baseDirectory).value
      val dir   = base / "examples"
      val out   = base / "target" / "layout-gallery"
      val files = (dir * "*.mmd").get().map(_.getAbsolutePath).sorted.mkString(" ")
      Def.uncached((Compile / runMain).toTask(s" mermoid.cli.MermoidCli $files --gallery ${out.getAbsolutePath}"))
    }.value,
  )
  .jvmPlatform(scalaVersions = scalaVersions)

lazy val specularPreview =
  taskKey[Unit]("Build specularSite then serve with sbt-reload (prefer alias: docsPreview)")

lazy val regenerateExamples =
  taskKey[Unit]("Re-render every examples/*.mmd to its sibling .svg (paired with SvgOutputSpec's staleness check)")

lazy val layoutGallery =
  taskKey[Unit]("Re-render examples and write target/layout-gallery/index.html for visual review")

// --- mermoid-docs : Specular docs-as-tests site. Never published; every diagram on the site is
//   rendered and asserted by `sbt test`, so a broken diagram is a red CI check.
//   JVM builds the static site; docsJS remounts `.interactive` examples in the browser (Specular pattern).
lazy val docs = (projectMatrix in file("docs"))
  .dependsOn(core, ascent)
  .settings(
    name            := "mermoid-docs",
    publish / skip  := true,
    publishArtifact := false,
    scalacOptions ++= commonScalacOptions,
  )
  .jvmPlatform(
    scalaVersions,
    Nil,
    (p: Project) =>
      p.enablePlugins(SpecularPlugin)
        .settings(
          MyVersions.docsJvm,
          zioTestSettings,
          Test / mainClass       := Some("specular.site.DocsServe"),
          Test / run / mainClass := (Test / mainClass).value,
          Test / runReloadArgs   := Seq(specularPort.value.toString),
          // runReload forks with the docs project as cwd, so a relative target/site would miss the
          // repo-root site written by specularSite. Point DocsServe at specularSiteDirectory.
          Test / run / javaOptions ++= {
            val dir = specularSiteDirectory.value.getAbsolutePath
            Seq(
              s"-Dspecular.site.dir=$dir",
              s"-Dspecular.site.port=${specularPort.value}",
            )
          },
          specularBuildMain := "mermoid.docs.BuildSite",
          // The JVM row of the core matrix — a bare LocalProject name resolves to it.
          specularMetaProject   := Some(LocalProject("core")),
          specularSiteDirectory := (ThisBuild / baseDirectory).value / "target" / "site",
          // Docs-only (workflow_dispatch) builds are dynver `-ci`; don't advertise that as a Central coord.
          specularDisplayVersion := {
            val v = (ThisBuild / version).value
            if (v.endsWith("-ci") || v.endsWith("-SNAPSHOT")) previousStableVersion.value.getOrElse("<version>")
            else ""
          },
          // Link docsJS then write marker path for BuildSite.afterBuild → assets/client.js.
          specularJsLink := Def
            .uncached(Def.task {
              (LocalProject("docsJS") / Compile / fastLinkJS).value
              val outDir = (LocalProject("docsJS") / Compile / fastLinkJSOutput).value
              val mainJs = outDir / "main.js"
              if (!mainJs.exists)
                sys.error(
                  s"Expected $mainJs after fastLinkJS; directory contains: " +
                    Option(outDir.list).toSeq.flatten.mkString(", ")
                )
              val marker = (ThisBuild / baseDirectory).value / "target" / "specular-client-js.path"
              IO.write(marker, mainJs.getAbsolutePath)
            })
            .value,
          specularPreview := Def
            .uncached(Def.task {
              specularSite.value
              (Test / runReload).value
            })
            .value,
        ),
  )
  .jsPlatform(
    scalaVersions,
    Nil,
    (p: Project) =>
      p.settings(
        javaTimePolyfill,
        MyVersions.docsJs,
        // Share Interactive DocSpec + registry with the JVM Test CP (Specular LibraryAuthors pattern).
        Compile / unmanagedSources ++= {
          val dir = (ThisBuild / baseDirectory).value / "docs" / "src" / "test" / "scala" / "mermoid" / "docs"
          Seq(dir / "Interactive.scala", dir / "ExampleRegistry.scala")
        },
        scalaJSUseMainModuleInitializer := true,
        scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
        Compile / mainClass := Some("mermoid.docs.ClientMain"),
        Test / skip         := true,
        Test / sources      := Nil,
      ),
  )

addCommandAlias("docsPreview", "~docs/specularPreview")
addCommandAlias("release", "; publishSigned; sonaRelease")
