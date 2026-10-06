package mermoid

import zio.test.*

object SequenceRenderSpec extends ZIOSpecDefault:

  private def svgOf(src: String): Option[String] =
    SequenceFixture.laid(src).map(scene => SvgSerializer.render(SvgRenderer.paint(Scene.Sequence(scene))))

  private def count(svg: String, needle: String): Int =
    svg.sliding(needle.length).count(_ == needle)

  def spec = suite("SequenceRenderer")(
    test("the span diagram paints names, solid requests, and dashed replies") {
      svgOf(SequenceFixture.span) match
        case None      => assertTrue(false)
        case Some(svg) =>
          assertTrue(
            svg.contains("Alice"),
            svg.contains("Bob"),
            svg.contains("Carol"),
            svg.contains("Dave"),
            svg.contains("Eve"),
            svg.contains("place order"),
            svg.contains("accepted or pending"),
            svg.contains("snapshot plus version"),
            count(svg, "message message-solid") == 7,
            count(svg, "message message-dashed") == 2,
            count(svg, "class=\"lifeline\"") == 5,
            svg.contains("id=\"seq-head\""),
            svg.contains("id=\"seq-tail\""),
            svg.contains("id=\"seq-open\""),
            svg.contains("id=\"seq-cross\""),
            svg.contains("marker-end=\"url(#seq-head)\""),
            !svg.contains("id=\"arrowhead\""),
          )
    },
    test("a self message, a note, an actor, and an alt all land in the svg") {
      val src =
        """sequenceDiagram
          |actor Alice
          |participant Bob
          |Alice->>Alice: again
          |alt ready
          |  Alice->>Bob: yes
          |else later
          |  Alice->>Bob: no
          |end
          |note right of Bob: held the lock
          |""".stripMargin
      svgOf(src) match
        case None      => assertTrue(false)
        case Some(svg) =>
          assertTrue(
            svg.contains("actor-person"),
            svg.contains("class=\"actor-figure\""),
            svg.contains("again"),
            svg.contains(">alt ready<"),
            svg.contains(">else later<"),
            svg.contains("held the lock"),
            svg.contains("id=\"note-0\""),
            svg.contains("id=\"fragment-0\""),
          )
      end match
    },
  )
end SequenceRenderSpec
