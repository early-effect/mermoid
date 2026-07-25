package mermoid.cli

import mermoid.*
import zio.*

import java.nio.file.{Files, Path}

/** Renders `.mmd` files to sibling `.svg` files.
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

  val run =
    for
      args <- getArgs
      _    <- args.toList match
        case Nil   => Console.printLine("Usage: mermoid <input.mmd> [input2.mmd ...]")
        case files => ZIO.foreach(files)(processFile)
    yield ()
end MermoidCli
