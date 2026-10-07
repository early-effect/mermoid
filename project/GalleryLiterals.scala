import sbt.*

/** Writes every `.mmd` file in `examples` as a `Mermaid("...")` literal, so a core test can compare each compile-time
  * lift with a runtime parse of the same text.
  */
object GalleryLiterals:

  def write(examples: File, out: File): Seq[File] =
    val files  = (examples * "*.mmd").get().sortBy(_.getName)
    val rows   = files.map(f => s"    ${literal(f.getName)} -> Mermaid(${literal(IO.read(f))}),")
    val source =
      s"""package mermoid
         |
         |// Generated from the .mmd files in examples by build.sbt.
         |object GalleryLiterals:
         |  val all: List[(String, Mermaid)] = List(
         |${rows.mkString("\n")}
         |  )
         |""".stripMargin
    if !out.exists || IO.read(out) != source then IO.write(out, source)
    Seq(out)
  end write

  private def literal(raw: String): String =
    raw
      .flatMap {
        case '\\'         => "\\\\"
        case '"'          => "\\\""
        case '\n'         => "\\n"
        case '\r'         => "\\r"
        case '\t'         => "\\t"
        case c if c < ' ' => f"\\u${c.toInt}%04x"
        case c            => c.toString
      }
      .mkString("\"", "", "\"")
end GalleryLiterals
