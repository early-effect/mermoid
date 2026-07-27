package mermoid.cli

import mermoid.*
import zio.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Renders `.mmd` files to sibling `.svg` files, and optionally builds a layout review gallery.
  *
  * JVM-only: `java.nio.file` is why the CLI is its own module rather than part of the cross-built core.
  */
object MermoidCli extends ZIOAppDefault:

  private def processFile(inputPath: String): ZIO[Any, Throwable, Unit] =
    val outputPath = inputPath.replaceAll("\\.mmd$", "") + ".svg"
    for
      input   <- ZIO.attempt(Files.readString(Path.of(inputPath)))
      diagram <- ZIO.fromEither(MermaidParser.parse(input)).mapError(msg => new RuntimeException(s"Parse error: $msg"))
      svg = SvgRenderer.render(diagram)
      _ <- ZIO.attempt(Files.writeString(Path.of(outputPath), svg))
      _ <- Console.printLine(s"Generated SVG: $outputPath")
    yield ()

  /** Writes `index.html` that embeds every example SVG for visual review (e.g. Playwright). */
  private def writeGallery(examplesDir: Path, outDir: Path): ZIO[Any, Throwable, Unit] =
    for
      _    <- ZIO.attempt(Files.createDirectories(outDir))
      svgs <- ZIO.attempt {
        Files
          .list(examplesDir)
          .iterator
          .asScala
          .toList
          .filter(_.getFileName.toString.endsWith(".svg"))
          .sortBy(_.getFileName.toString)
      }
      cards = svgs
        .map { p =>
          val name = p.getFileName.toString
          val body = Files.readString(p)
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
      _ <- ZIO.attempt(Files.writeString(out, html))
      _ <- Console.printLine(s"Wrote gallery: $out")
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
    for
      args <- getArgs
      _    <- parseArgs(args.toList) match
        case (Nil, None) => Console.printLine("Usage: mermoid <input.mmd> [input2.mmd ...] [--gallery <out-dir>]")
        case (files, galleryOut) =>
          for
            _ <- ZIO.foreach(files)(processFile)
            _ <- galleryOut match
              case Some(out) =>
                val examples = files.headOption
                  .map(f => Path.of(f).toAbsolutePath.getParent)
                  .getOrElse(Path.of("examples"))
                writeGallery(examples, out)
              case None => ZIO.unit
          yield ()
    yield ()
end MermoidCli
