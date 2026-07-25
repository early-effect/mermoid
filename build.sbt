val scala3Version   = "3.8.4"
val zioVersion      = "2.1.26"
val specularVersion = "0.9.0"

// sbt 2.x scopes bare build.sbt settings to ThisBuild, so these apply build-wide to every module.
scalaVersion         := scala3Version
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

// zipx CI: Aggregate verify gated on formatting + workflow-drift, then Central publish and Pages docs.
zipxJavaVersion      := "25"
zipxWorkflowDispatch := true
zipxScalaSteward     := true
// sbt 2.x aliases `test` to `testQuick`, and the CI cache restores `target/` — so on a cache hit
// `test` would skip unchanged suites. CI must run everything.
val ciTestTask = "testFull"
zipxTestTask := ciTestTask
zipxCapabilities ++= Seq(
  Capability.once("fmt", "scalafmtCheckAll; zipxWorkflowCheck"),
  // Overriding the builtin `test` capability by name replaces its command too, and Capability.test's
  // is the literal "test" — so the command has to be restated here or zipxTestTask is silently lost.
  Capability.test.copy(command = _ => ciTestTask, needsCapabilities = List("fmt")),
  ZipxCentral.release,
  ZipxDocs.pages(),
)

val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-Wunused:all",
)

val scalaVersions = Seq(scala3Version)

/** zio-test deps. A Def.settings block (not a bare Seq) so the per-project platform suffix that `%%` appends in sbt 2.x
  * resolves at each module's scope. ZTestFramework registers itself via zio-test-sbt, so no testFrameworks wiring is
  * needed.
  */
val zioTestSettings = Def.settings(
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio-test"     % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
  )
)

lazy val root = (project in file("."))
  .aggregate((core.projectRefs ++ cli.projectRefs ++ docs.projectRefs)*)
  .settings(
    // sbt 2.x derives output directories from `name`, so the aggregate cannot share `core`'s name.
    name           := "mermoid-root",
    publish / skip := true,
    test / skip    := true,
  )

// --- mermoid : the parser, layout and SVG renderer. The ONLY published artifact.
//   fastparse is its only dependency — no ZIO, no ascent, no specular. Integrations with other
//   libraries live in those libraries' repos and consume `SvgNode` as the contract.
lazy val core = (projectMatrix in file("core"))
  .settings(
    name        := "mermoid",
    description := "Mermaid-compatible diagram to SVG renderer for Scala 3, themed with real CSS",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1",
    zioTestSettings,
  )
  .jvmPlatform(scalaVersions = scalaVersions)
  .jsPlatform(scalaVersions = scalaVersions)

// --- mermoid-cli : fat-jar renderer for .mmd files. JVM only (java.nio.file), never published.
lazy val cli = (projectMatrix in file("cli"))
  .dependsOn(core)
  .settings(
    name            := "mermoid-cli",
    publish / skip  := true,
    publishArtifact := false, // zipx derives publish jobs from publishArtifact
    scalacOptions ++= commonScalacOptions,
    libraryDependencies += "dev.zio" %% "zio" % zioVersion,
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
  )
  .jvmPlatform(scalaVersions = scalaVersions)

lazy val specularPreview =
  taskKey[Unit]("Build specularSite then serve with sbt-reload (prefer alias: docsPreview)")

lazy val regenerateExamples =
  taskKey[Unit]("Re-render every examples/*.mmd to its sibling .svg (paired with SvgOutputSpec's staleness check)")

// --- mermoid-docs : Specular docs-as-tests site. Never published; every diagram on the site is
//   rendered and asserted by `sbt test`, so a broken diagram is a red CI check.
lazy val docs = (projectMatrix in file("docs"))
  .dependsOn(core)
  .settings(
    name            := "mermoid-docs",
    publish / skip  := true,
    publishArtifact := false,
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "rocks.earlyeffect" %% "specular-core"           % specularVersion % Test,
      "rocks.earlyeffect" %% "specular-zio-test"       % specularVersion % Test,
      "rocks.earlyeffect" %% "specular-site"           % specularVersion % Test,
      "rocks.earlyeffect" %% "early-effect-docs-theme" % specularVersion % Test,
      "dev.zio"           %% "zio"                     % zioVersion      % Test,
    ),
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
    specularPreview := Def.uncached {
      specularSite.value
      (Test / runReload).value
    },
  )
  .jvmPlatform(scalaVersions = scalaVersions, Nil, (p: Project) => p.enablePlugins(SpecularPlugin))

addCommandAlias("docsPreview", "~docs/specularPreview")
addCommandAlias("release", "; publishSigned; sonaRelease")
