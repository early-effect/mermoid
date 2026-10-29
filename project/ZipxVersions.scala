import zipx.*

/** Typed catalog: every library and plugin this build may use. `zipxDepUpdate` rewrites constructors here.
  *
  * sbt-zipx is not a row: generate emits it from the loaded plugin (`zipxSelfPlugins`). sbt-pgp is not a row: zipx
  * already brings it in. Action pins stay on jar defaults.
  *
  * Parent `Lib` vals used only for `.mod` are catalog rows; they are not `library()`-selected when another selected
  * module already pulls them. The docs stack selects specular-site itself so the coordinate is the catalog row.
  */
object MyVersions extends ZipxVersions:
  val sbt: SbtVersion     = SbtVersion("2.1.0-M3")
  val scala: ScalaVersion = ScalaVersion("3.9.0")

  val release = ShipGroup("mermoid", "0.2.1")("core", "ascent")

  val zio        = Lib("dev.zio", "zio", "2.1.27")
  val zioTest    = zio.mod("zio-test")
  val zioTestSbt = zio.mod("zio-test-sbt")

  val fastparse = Lib("com.lihaoyi", "fastparse", "3.1.1")

  val scalaJavaTime = Lib("io.github.cquiroz", "scala-java-time", "2.8.0")

  val ascent     = Lib("rocks.earlyeffect", "ascent-core", "0.10.2")
  val ascentCss  = ascent.mod("ascent-css")
  val ascentHtml = Lib("rocks.earlyeffect", "ascent-html", "0.10.1")
  // Own row: its line is not the core family's.
  val ascentJs   = Lib("rocks.earlyeffect", "ascent-js", "0.11.1")

  val specular        = Lib("rocks.earlyeffect", "specular-core", "0.21.1")
  val specularZioTest = specular.mod("specular-zio-test").test
  val specularSite    = specular.mod("specular-site").test
  val specularTheme   = specular.mod("early-effect-docs-theme").test

  val scalajs        = Plugin("org.scala-js", "sbt-scalajs", "1.22.0")
  val scalafmt       = Plugin("org.scalameta", "sbt-scalafmt", "2.6.2")
  val assembly       = Plugin("com.eed3si9n", "sbt-assembly", "2.5.0")
  val specularPlugin = Plugin("rocks.earlyeffect", "sbt-specular", "0.21.1")

  def zioTests      = library(zioTest.test, zioTestSbt.test)
  def zioLib        = library(zio)
  def parserLib     = library(fastparse)
  def javaTime      = library(scalaJavaTime)
  def ascentLib     = library(ascent, ascentCss, zio)
  def ascentHtmlLib = library(ascentHtml)
  def ascentJsLib   = library(ascentJs)
  def docsJvm       = library(specularZioTest, specularSite, specularTheme)
  def docsJs        = library(specular, ascentJs, ascentCss, zio)
end MyVersions
