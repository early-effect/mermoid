import zipx.*

/** Typed catalog: every library and plugin this build may use. `zipxDepUpdate` rewrites constructors here.
  *
  * sbt-zipx is not a row: generate emits it from the loaded plugin (`zipxSelfPlugins`). sbt-pgp is not a row: zipx
  * already brings it in. Action pins stay on jar defaults.
  *
  * Parent `Lib` vals used only for `.mod` are catalog rows; they are not `library()`-selected when another selected
  * module already pulls them (specular-site via the docs theme).
  */
object MyVersions extends ZipxVersions:
  val sbt: SbtVersion     = SbtVersion("2.0.6")
  val scala: ScalaVersion = ScalaVersion("3.8.4")

  val zio        = Lib("dev.zio", "zio", "2.1.26")
  val zioTest    = zio.mod("zio-test")
  val zioTestSbt = zio.mod("zio-test-sbt")

  val fastparse = Lib("com.lihaoyi", "fastparse", "3.1.1")

  val scalaJavaTime     = Lib("io.github.cquiroz", "scala-java-time", "2.7.0")
  val scalaJavaTimeTzdb = scalaJavaTime.mod("scala-java-time-tzdb")

  val ascent     = Lib("rocks.earlyeffect", "ascent-core", "0.4.1")
  val ascentCss  = ascent.mod("ascent-css")
  val ascentHtml = ascent.mod("ascent-html")
  val ascentJs   = ascent.mod("ascent-js")

  val specular        = Lib("rocks.earlyeffect", "specular-core", "0.13.0")
  val specularZioTest = specular.mod("specular-zio-test").test
  val specularTheme   = specular.mod("early-effect-docs-theme").test

  val scalajs        = Plugin("org.scala-js", "sbt-scalajs", "1.22.0")
  val scalafmt       = Plugin("org.scalameta", "sbt-scalafmt", "2.6.2")
  val assembly       = Plugin("com.eed3si9n", "sbt-assembly", "2.4.1")
  val sbtReload      = Plugin("com.jamesward", "sbt-reload", "0.0.7")
  val dynverCi       = Plugin("rocks.earlyeffect", "sbt-dynver-ci", "0.2.2")
  val specularPlugin = Plugin("rocks.earlyeffect", "sbt-specular", "0.13.0")

  def zioTests      = library(zioTest.test, zioTestSbt.test)
  def zioLib        = library(zio)
  def parserLib     = library(fastparse)
  def javaTime      = library(scalaJavaTime, scalaJavaTimeTzdb)
  def ascentLib     = library(ascent, ascentCss, zio)
  def ascentHtmlLib = library(ascentHtml)
  def ascentJsLib   = library(ascentJs)
  def docsJvm       = library(specularZioTest, specularTheme)
  def docsJs        = library(specular, ascentJs, ascentCss, zio)
end MyVersions
