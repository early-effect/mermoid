package mermoid.cli

import mermoid.*
import zio.*

import java.io.IOException
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Why the CLI stopped. `main` prints [[message]] and exits non-zero. */
enum CliError:
  case Unreadable(path: Path, cause: IOException)
  case Unparseable(path: Path, error: ParseError)
  case Unwritable(path: Path, cause: IOException)

  def message: String = this match
    case Unreadable(path, cause)  => s"cannot read $path: $cause"
    case Unparseable(path, error) => s"$path: ${error.message}"
    case Unwritable(path, cause)  => s"cannot write $path: $cause"
end CliError

/** Renders `.mmd` files to sibling `.svg` files, and optionally builds a layout review gallery.
  *
  * JVM-only: `java.nio.file` is why the CLI is its own module rather than part of the cross-built core.
  */
object MermoidCli extends ZIOAppDefault:

  private def read(path: Path): IO[CliError, String] =
    ZIO.attemptBlockingIO(Files.readString(path)).mapError(CliError.Unreadable(path, _))

  private def write(path: Path, text: String): IO[CliError, Unit] =
    ZIO.attemptBlockingIO(Files.writeString(path, text)).unit.mapError(CliError.Unwritable(path, _))

  private def say(line: String): IO[CliError, Unit] =
    Console.printLine(line).orDie

  private def processFile(inputPath: String): IO[CliError, Unit] =
    val input  = Path.of(inputPath)
    val output = Path.of(inputPath.replaceAll("\\.mmd$", "") + ".svg")
    for
      text    <- read(input)
      mermaid <- ZIO.fromEither(Mermaid.from(text)).mapError(CliError.Unparseable(input, _))
      _       <- write(output, SvgRenderer.render(mermaid.diagram))
      _       <- say(s"Generated SVG: $output")
    yield ()
  end processFile

  /** Writes `index.html` that embeds every example SVG for visual review (e.g. Playwright). */
  private def writeGallery(examplesDir: Path, outDir: Path): IO[CliError, Unit] =
    for
      _    <- ZIO.attemptBlockingIO(Files.createDirectories(outDir)).mapError(CliError.Unwritable(outDir, _))
      svgs <- ZIO
        .scoped(
          ZIO
            .fromAutoCloseable(ZIO.attemptBlockingIO(Files.list(examplesDir)))
            .flatMap(listing => ZIO.attemptBlockingIO(listing.iterator.asScala.toList))
        )
        .mapError(CliError.Unreadable(examplesDir, _))
        .map(_.filter(_.getFileName.toString.endsWith(".svg")).sortBy(_.getFileName.toString))
      bodies <- ZIO.foreach(svgs)(p => read(p).map(p.getFileName.toString -> _))
      cards = bodies
        .map { (name, body) =>
          s"""<section class="card">
           |  <h2>$name</h2>
           |  <div class="diagram">$body</div>
           |</section>""".stripMargin
        }
        .mkString("\n")
      html =
        s"""<!DOCTYPE html>
           |<html lang="en">
           |<head>
           |  <meta charset="utf-8"/>
           |  <title>mermoid layout gallery</title>
           |  <style>
           |    body { font-family: system-ui, sans-serif; margin: 1.5rem; background: #f6f7f9; color: #1a1a1a; }
           |    h1 { font-weight: 600; }
           |    .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(420px, 1fr)); gap: 1.25rem; }
           |    .card { background: #fff; border: 1px solid #dde1e6; border-radius: 8px; padding: 1rem; }
           |    .card h2 { font-size: 0.95rem; margin: 0 0 0.75rem; font-weight: 600; }
           |    .diagram { overflow: auto; background: #fafbfc; border-radius: 4px; padding: 0.5rem; }
           |    .diagram svg { max-width: 100%; height: auto; display: block; }
           |  </style>
           |</head>
           |<body>
           |  <h1>mermoid layout gallery</h1>
           |  <p>${svgs.size} diagrams from <code>examples/</code></p>
           |  <div class="grid">
           |$cards
           |  </div>
           |</body>
           |</html>""".stripMargin
      out = outDir.resolve("index.html")
      _ <- write(out, html)
      _ <- say(s"Wrote gallery: $out")
    yield ()

  private def parseArgs(args: List[String]): (List[String], Option[Path]) =
    args match
      case Nil                                                 => (Nil, None)
      case "--gallery" :: dir :: rest if !dir.startsWith("--") =>
        val (files, _) = parseArgs(rest)
        (files, Some(Path.of(dir)))
      case "--gallery" :: rest =>
        val (files, _) = parseArgs(rest)
        (files, Some(Path.of("target/layout-gallery")))
      case f :: rest =>
        val (files, gallery) = parseArgs(rest)
        (f :: files, gallery)

  val run =
    render.catchAll(error => Console.printLineError(error.message).orDie *> exit(ExitCode.failure))

  private def render: ZIO[ZIOAppArgs, CliError, Unit] =
    for
      args <- getArgs
      _    <- parseArgs(args.toList) match
        case (Nil, None)         => say("Usage: mermoid <input.mmd> [input2.mmd ...] [--gallery <out-dir>]")
        case (files, galleryOut) =>
          for
            _ <- ZIO.foreach(files)(processFile)
            _ <- galleryOut match
              case Some(out) =>
                val examples = files.headOption
                  .flatMap(f => Option(Path.of(f).toAbsolutePath.getParent))
                  .getOrElse(Path.of("examples"))
                writeGallery(examples, out)
              case None => ZIO.unit
          yield ()
    yield ()
end MermoidCli
