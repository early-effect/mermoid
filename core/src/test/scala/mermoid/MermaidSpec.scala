package mermoid

import zio.test.*
import zio.test.Assertion.*

object MermaidSpec extends ZIOSpecDefault:

  private val flow = Mermaid("""flowchart LR
    A[Start] --> B{Ok?}
    B -->|yes| C((Done))
    style A fill:#f9f,stroke:#333
    click C href "https://example.com" "Docs" _blank""")

  private val margined = Mermaid("""stateDiagram-v2
      |  [*] --> Idle
      |  Idle --> Busy: go
      |  note right of Busy
      |    working
      |  end note""".stripMargin)

  def spec = suite("Mermaid")(
    suite("literal")(
      test("keeps its source and lifts the diagram the parser builds") {
        assertTrue(MermaidParser.parse(flow.source) == Right(flow.diagram))
      },
      test("applies stripMargin before it parses") {
        assertTrue(
          margined.source.startsWith("stateDiagram-v2\n  [*] --> Idle"),
          MermaidParser.parse(margined.source) == Right(margined.diagram),
        )
      },
      test("lifts sequence diagrams, including opaque participant ids") {
        val seq = Mermaid("""sequenceDiagram
          participant A as Alice
          A->>+B: hi
          rect rgba(0, 0, 255, 0.1)
            B-->>-A: bye
          end""")
        assertTrue(MermaidParser.parse(seq.source) == Right(seq.diagram))
      },
      test("every examples/*.mmd literal lifts to the diagram its source parses to") {
        assertTrue(GalleryLiterals.all.nonEmpty) &&
        TestResult.allSuccesses(GalleryLiterals.all.map { case (name, m) =>
          assertTrue(name.nonEmpty, MermaidParser.parse(m.source) == Right(m.diagram))
        })
      },
      test("a diagram that does not parse is a compile error") {
        assertZIO(typeCheck("""mermoid.Mermaid("flowchart LR\n  A -->")"""))(
          isLeft(containsString("Not a Mermaid diagram: parse error at"))
        )
      },
      test("a non-literal argument is a compile error") {
        assertZIO(typeCheck("""val s = "flowchart LR\n  A"; mermoid.Mermaid(s)"""))(
          isLeft(containsString("Mermaid(...) takes a string literal"))
        )
      },
    ),
    suite("from")(
      test("round-trips a source through the parser") {
        val src = "flowchart TD\n  A --> B"
        assertTrue(
          Mermaid.from(src).map(_.source) == Right(src),
          Mermaid.from(src).map(_.diagram) == MermaidParser.parse(src),
          Mermaid.from(flow.source) == Right(flow),
        )
      },
      test("fails with the parser's typed error") {
        val bad      = "not a diagram at all"
        val expected = MermaidParser.parse(bad).swap.toOption
        assertTrue(expected.nonEmpty, Mermaid.from(bad).swap.toOption == expected)
      },
      test("reports a sequence alias conflict as ParseError.ConflictingAlias") {
        val src = "sequenceDiagram\n  participant A as One\n  participant A as Two"
        assertTrue(Mermaid.from(src) match
          case Left(ParseError.ConflictingAlias(id, "One", "Two")) => id.value == "A"
          case _                                                   => false)
      },
    ),
  )
end MermaidSpec
