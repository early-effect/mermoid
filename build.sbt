import org.scalajs.linker.interface.ModuleKind
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

MyVersions.settings

ThisBuild / scalaVersion := (MyVersions.scala: String)

val scala3Version: String = MyVersions.scala

// sbt 2.x scopes bare build.sbt settings to ThisBuild, so these apply build-wide to every module.
organization         := "rocks.earlyeffect"
organizationName     := "Early Effect"
organizationHomepage := Some(uri("https://www.earlyeffect.rocks"))
versionScheme        := Some("early-semver")

homepage := Some(uri("https://github.com/early-effect/mermoid"))
licenses := Seq("Apache-2.0" -> uri("http://www.apache.org/licenses/LICENSE-2.0.txt"))
scmInfo  := Some(
  ScmInfo(
    uri("https://github.com/early-effect/mermoid"),
    "scm:git@github.com:early-effect/mermoid.git",
  )
)
developers := List(
  Developer(
    id = "russwyte",
    name = "Russ White",
    email = "356303+russwyte@users.noreply.github.com",
    url = uri("https://github.com/russwyte"),
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
  ZipxCentral.snapshots,
  ZipxCentral.pullRequestSnapshots("snapshots"),
  ZipxDocs.pages(),
)
zipxReleaseWorkflow := Some(ZipxCentral.releases)

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
    // Uncached: the inputs are the example files, which sbt does not see as task inputs here.
    Test / sourceGenerators += Def
      .uncached(Def.task {
        val out = (Test / sourceManaged).value / "mermoid" / "GalleryLiterals.scala"
        GalleryLiterals.write((ThisBuild / baseDirectory).value / "examples", out)
      })
      .taskValue,
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(scalaVersions = scalaVersions)

// ZIO's JS runtime references java.time (FiberRuntime metrics), so a linked app needs the classes. No mermoid code
// resolves a time zone, so the tzdb data stays out.
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

lazy val regenerateExamples =
  taskKey[Unit]("Re-render every examples/*.mmd to its sibling .svg (paired with SvgOutputSpec's staleness check)")

lazy val layoutGallery =
  taskKey[Unit]("Re-render examples and write target/layout-gallery/index.html for visual review")

/** Fast-link the docs client and record where `BuildSite` finds it. */
lazy val linkDocsClient = Def.uncached(Def.task {
  (LocalProject("docsJS") / Compile / fastLinkJS).value
  val outDir = (LocalProject("docsJS") / Compile / fastLinkJSOutput).value
  val mainJs = outDir / "main.js"
  if (!mainJs.exists)
    sys.error(
      s"Expected $mainJs after fastLinkJS; directory contains: " + Option(outDir.list).toSeq.flatten.mkString(", ")
    )
  IO.write((ThisBuild / baseDirectory).value / "target" / "specular-client-js.path", mainJs.getAbsolutePath)
})

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
          specularBuildMain := "mermoid.docs.BuildSite",
          // The JVM row of the core matrix — a bare LocalProject name resolves to it.
          specularMetaProject   := Some(LocalProject("core")),
          specularSiteDirectory := (ThisBuild / baseDirectory).value / "target" / "site",
          // Docs-only (workflow_dispatch) builds are dynver `-ci`; don't advertise that as a Central coord.
          specularDisplayVersion := stripCi,
          // Link docsJS then write marker path for BuildSite.afterBuild → assets/client.js. The site and its
          // preview share one link, and the preview watches docsJS so a client edit reloads the page.
          specularJsLink    := linkDocsClient.value,
          specularJsLinkDev := linkDocsClient.value,
          specularJsProject := Some(LocalProject("docsJS")),
        ),
  )
  .jsPlatform(
    scalaVersions,
    Nil,
    (p: Project) =>
      p.settings(
        MyVersions.docsJs,
        // ascent-core and specular-core JS still publish scala-java-time-tzdb. The client resolves no time zones.
        excludeDependencies += ExclusionRule("io.github.cquiroz", "scala-java-time-tzdb_sjs1_3"),
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

addCommandAlias("release", "; publishSigned; sonaRelease")
