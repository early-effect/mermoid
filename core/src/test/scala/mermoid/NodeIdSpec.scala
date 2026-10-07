package mermoid

import zio.test.*
import zio.test.Assertion.*

object NodeIdSpec extends ZIOSpecDefault:

  def spec = suite("NodeId")(
    test("a literal keeps its text") {
      assertTrue(NodeId("Idle").value == "Idle", NodeId("[*]") == NodeId.stateMarker)
    },
    test("from accepts Mermaid identifiers and the state marker") {
      assertTrue(
        NodeId.from("Order_2").map(_.value) == Right("Order_2"),
        NodeId.from("[*]") == Right(NodeId.stateMarker),
      )
    },
    test("from rejects empty text and characters outside the id grammar") {
      assertTrue(
        NodeId.from("") == Left(NodeIdError.Empty),
        NodeId.from("a-b") == Left(NodeIdError.IllegalCharacter("a-b", 1, '-')),
        NodeId.from("[*]-end") == Left(NodeIdError.IllegalCharacter("[*]-end", 0, '[')),
      )
    },
    test("the parser's ids are the ids from accepts") {
      val parsed = MermaidParser.parse("flowchart LR\n  A_1 --> b2").toOption.toList.flatMap {
        case Diagram.Flowchart(_, stmts) => StyleResolver.collectNodes(stmts).keys.toList
        case _                           => Nil
      }
      assertTrue(parsed.nonEmpty, parsed.forall(id => NodeId.from(id.value) == Right(id)))
    },
    test("a literal outside the grammar is a compile error") {
      assertZIO(typeCheck("""mermoid.NodeId("a b")"""))(isLeft(containsString("is not allowed in a node id")))
    },
  )
end NodeIdSpec
