package mermoid

import zio.test.*
import fastparse.*

object ParserSpec extends ZIOSpecDefault:

  def spec = suite("MermaidParser")(
    suite("helpers")(
      test("identifier parses alphanumeric strings") {
        val result = fastparse.parse("hello123", MermaidParser.identifier(using _))
        assertTrue(result.get.value == "hello123")
      },
      test("quotedString parses double-quoted strings") {
        val result = fastparse.parse("\"hello world\"", MermaidParser.quotedString(using _))
        assertTrue(result.get.value == "hello world")
      },
    ),
    suite("direction")(
      test("parses all directions") {
        val cases = List(
          "TB" -> Direction.TB,
          "TD" -> Direction.TD,
          "BT" -> Direction.BT,
          "LR" -> Direction.LR,
          "RL" -> Direction.RL,
        )
        assertTrue(cases.forall { case (input, expected) =>
          fastparse.parse(input, MermaidParser.direction(using _)).get.value == expected
        })
      }
    ),
    suite("nodeShape")(
      test("parses rect shape") {
        val result = fastparse.parse("[hello]", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.Rect))
      },
      test("parses round shape") {
        val result = fastparse.parse("(hello)", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.Round))
      },
      test("parses stadium shape") {
        val result = fastparse.parse("([hello])", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.Stadium))
      },
      test("parses rhombus shape") {
        val result = fastparse.parse("{hello}", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.Rhombus))
      },
      test("parses circle shape") {
        val result = fastparse.parse("((hello))", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.Circle))
      },
      test("parses double circle shape") {
        val result = fastparse.parse("(((hello)))", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.DoubleCircle))
      },
      test("parses hexagon shape") {
        val result = fastparse.parse("{{hello}}", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.Hexagon))
      },
      test("parses subroutine shape") {
        val result = fastparse.parse("[[hello]]", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.Subroutine))
      },
      test("parses cylinder shape") {
        val result = fastparse.parse("[(hello)]", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("hello", NodeShape.Cylinder))
      },
      test("parses rect with quoted slash") {
        val result = fastparse.parse("""["a / b"]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Rect))
      },
      test("parses rect with unquoted slash") {
        val result = fastparse.parse("[a / b]", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Rect))
      },
      test("parses round with quoted slash") {
        val result = fastparse.parse("""("a / b")""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Round))
      },
      test("parses round with unquoted slash") {
        val result = fastparse.parse("(a / b)", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Round))
      },
      test("parses rhombus with quoted slash") {
        val result = fastparse.parse("""{"a / b"}""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Rhombus))
      },
      test("parses stadium with quoted slash") {
        val result = fastparse.parse("""(["a / b"])""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Stadium))
      },
      test("parses circle with quoted slash") {
        val result = fastparse.parse("""(("a / b"))""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Circle))
      },
      test("parses double circle with quoted slash") {
        val result = fastparse.parse("""((("a / b")))""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.DoubleCircle))
      },
      test("parses hexagon with quoted slash") {
        val result = fastparse.parse("""{{"a / b"}}""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Hexagon))
      },
      test("parses subroutine with quoted slash") {
        val result = fastparse.parse("""[["a / b"]]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Subroutine))
      },
      test("parses cylinder with quoted slash") {
        val result = fastparse.parse("""[("a / b")]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Cylinder))
      },
      test("parses trapezoid with quoted slash") {
        val result = fastparse.parse("""[/"a / b"\]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Trapezoid))
      },
      test("parses parallelogram with quoted slash") {
        val result = fastparse.parse("""[/"a / b"/]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a / b", NodeShape.Parallelogram))
      },
      test("parses rect with quoted backslash") {
        val result = fastparse.parse("""["a \ b"]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a \\ b", NodeShape.Rect))
      },
      test("parses rect with unquoted backslash") {
        val result = fastparse.parse("""[a \ b]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a \\ b", NodeShape.Rect))
      },
      test("parses rect with quoted pipe") {
        val result = fastparse.parse("""["a | b"]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a | b", NodeShape.Rect))
      },
      test("parses rect with unquoted pipe") {
        val result = fastparse.parse("[a | b]", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a | b", NodeShape.Rect))
      },
      test("parses rect with quoted parens") {
        val result = fastparse.parse("""["a (b)"]""", MermaidParser.nodeShape(using _))
        assertTrue(result.get.value == ("a (b)", NodeShape.Rect))
      },
    ),
    suite("nodeDef")(
      test("parses bare identifier as rect node") {
        val result = fastparse.parse("A", MermaidParser.nodeDef(using _))
        assertTrue(result.get.value == NodeDef("A", None, NodeShape.Rect))
      },
      test("parses node with label") {
        val result = fastparse.parse("A[Hello World]", MermaidParser.nodeDef(using _))
        assertTrue(result.get.value == NodeDef("A", Some("Hello World"), NodeShape.Rect))
      },
      test("parses node with round shape") {
        val result = fastparse.parse("B(Round Node)", MermaidParser.nodeDef(using _))
        assertTrue(result.get.value == NodeDef("B", Some("Round Node"), NodeShape.Round))
      },
      test("parses node with quoted slash in label") {
        val result = fastparse.parse("""A["a / b"]""", MermaidParser.nodeDef(using _))
        assertTrue(result.get.value == NodeDef("A", Some("a / b"), NodeShape.Rect))
      },
      test("parses node with unquoted slash in label") {
        val result = fastparse.parse("A[a / b]", MermaidParser.nodeDef(using _))
        assertTrue(result.get.value == NodeDef("A", Some("a / b"), NodeShape.Rect))
      },
    ),
    suite("edgeStyle")(
      test("parses arrow -->") {
        val result = fastparse.parse("-->", MermaidParser.edgeStyle(using _))
        assertTrue(result.get.value == (EdgeStyle.Arrow, None))
      },
      test("parses open ---") {
        val result = fastparse.parse("---", MermaidParser.edgeStyle(using _))
        assertTrue(result.get.value == (EdgeStyle.Open, None))
      },
      test("parses dotted -.->") {
        val result = fastparse.parse("-.->", MermaidParser.edgeStyle(using _))
        assertTrue(result.get.value == (EdgeStyle.Dotted, None))
      },
      test("parses thick ==>") {
        val result = fastparse.parse("==>", MermaidParser.edgeStyle(using _))
        assertTrue(result.get.value == (EdgeStyle.Thick, None))
      },
      test("parses arrow with pipe label --> |label|") {
        val result = fastparse.parse("--> |yes|", MermaidParser.edgeStyle(using _))
        assertTrue(result.get.value == (EdgeStyle.Arrow, Some("yes")))
      },
      test("parses arrow with inline label -- text -->") {
        val result = fastparse.parse("-- text -->", MermaidParser.edgeStyle(using _))
        assertTrue(result.get.value == (EdgeStyle.Arrow, Some("text")))
      },
    ),
    suite("edgeSt")(
      test("parses simple edge") {
        val result   = fastparse.parse("A --> B", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge("A", "B", EdgeStyle.Arrow, None),
          NodeDef("A", None, NodeShape.Rect),
          NodeDef("B", None, NodeShape.Rect),
        )
        assertTrue(result.get.value == expected)
      },
      test("parses edge with labeled target node") {
        val result   = fastparse.parse("A --> B[Hello]", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge("A", "B", EdgeStyle.Arrow, None),
          NodeDef("A", None, NodeShape.Rect),
          NodeDef("B", Some("Hello"), NodeShape.Rect),
        )
        assertTrue(result.get.value == expected)
      },
      test("parses edge with label") {
        val result   = fastparse.parse("A --> |yes| B", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge("A", "B", EdgeStyle.Arrow, Some("yes")),
          NodeDef("A", None, NodeShape.Rect),
          NodeDef("B", None, NodeShape.Rect),
        )
        assertTrue(result.get.value == expected)
      },
      test("parses edge with labeled source node") {
        val result   = fastparse.parse("A[Start] --> B[End]", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge("A", "B", EdgeStyle.Arrow, None),
          NodeDef("A", Some("Start"), NodeShape.Rect),
          NodeDef("B", Some("End"), NodeShape.Rect),
        )
        assertTrue(result.get.value == expected)
      },
      test("parses edge with alias") {
        val result   = fastparse.parse("A --> B as myEdge", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge("A", "B", EdgeStyle.Arrow, None, Some("myEdge")),
          NodeDef("A", None, NodeShape.Rect),
          NodeDef("B", None, NodeShape.Rect),
        )
        assertTrue(result.get.value == expected)
      },
      test("parses edge with label and alias") {
        val result   = fastparse.parse("A -->|fast| B as fastEdge", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge("A", "B", EdgeStyle.Arrow, Some("fast"), Some("fastEdge")),
          NodeDef("A", None, NodeShape.Rect),
          NodeDef("B", None, NodeShape.Rect),
        )
        assertTrue(result.get.value == expected)
      },
      test("edge alias is optional") {
        val result = fastparse.parse("A --> B", MermaidParser.edgeSt(using _))
        val edge   = result.get.value.edge
        assertTrue(edge.alias.isEmpty)
      },
    ),
    suite("full diagram")(
      test("parses simple flowchart") {
        val input =
          """flowchart LR
            |  A[Start] --> B[End]
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
      },
      test("parses flowchart with multiple statements") {
        val input =
          """flowchart TD
            |  A[Start] --> B{Decision}
            |  B --> |yes| C[OK]
            |  B --> |no| D[Fail]
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(
          result.isRight,
          result.toOption.get.asInstanceOf[Diagram.Flowchart].statements.size == 3,
        )
      },
      test("parses graph keyword as alias for flowchart") {
        val input =
          """graph TD
            |  A --> B
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
      },
      test("defaults to TB direction when none specified") {
        val input =
          """flowchart
            |  A --> B
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(
          result.isRight,
          result.toOption.get.asInstanceOf[Diagram.Flowchart].direction == Direction.TB,
        )
      },
      test("parses flowchart with unquoted slashes in node labels") {
        val input =
          """flowchart TD
            |  Shell[zipx-shell · Script / Command / Word / ShTest] --> Steps2[Step.run]
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
        val nodes = StyleResolver.collectNodes(result.toOption.get.asInstanceOf[Diagram.Flowchart].statements)
        assertTrue(
          nodes("Shell").label.contains("zipx-shell · Script / Command / Word / ShTest"),
          nodes("Steps2").label.contains("Step.run"),
        )
      },
      test("parses flowchart with quoted slashes and pipes in node labels") {
        val input =
          """flowchart LR
            |  A["a / b"] --> B["c | d"]
            |  C[a \ b] --> D(a / b)
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
        val nodes = StyleResolver.collectNodes(result.toOption.get.asInstanceOf[Diagram.Flowchart].statements)
        assertTrue(
          nodes("A").label.contains("a / b"),
          nodes("B").label.contains("c | d"),
          nodes("C").label.contains("a \\ b"),
          nodes("D").label.contains("a / b"),
        )
      },
      test("parse failure after a valid header points past line 1") {
        val input =
          """flowchart LR
            |  A[broken
            |""".stripMargin
        val err = MermaidParser.parse(input).swap.toOption.get
        assertTrue(!err.contains("Position 1:1"))
      },
    ),
    suite("state diagram")(
      test("parses simple state transition") {
        val result = fastparse.parse("Created --> PaymentProcessing", MermaidParser.stateTransition(using _))
        val t      = result.get.value.transition
        assertTrue(t.from == "Created", t.to == "PaymentProcessing", t.label.isEmpty)
      },
      test("parses state transition with label") {
        val result =
          fastparse.parse("Created --> PaymentProcessing: InitiatePayment", MermaidParser.stateTransition(using _))
        val t = result.get.value.transition
        assertTrue(t.from == "Created", t.to == "PaymentProcessing", t.label == Some("InitiatePayment"))
      },
      test("parses [*] start state") {
        val result = fastparse.parse("[*] --> Created", MermaidParser.stateTransition(using _))
        val t      = result.get.value.transition
        assertTrue(t.from == "[*]", t.to == "Created")
      },
      test("parses full state diagram") {
        val input =
          """stateDiagram-v2
            |    [*] --> Created
            |    Created --> Processing: Start
            |    Processing --> Done: Finish
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(
          result.isRight,
          result.toOption.get.asInstanceOf[Diagram.StateDiagram].statements.size == 3,
        )
      },
      test("parses state diagram with notes") {
        val input =
          """stateDiagram-v2
            |    [*] --> Created
            |    Created --> Processing: Start
            |    note right of Processing
            |      timeout: 5m
            |    end note
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(
          result.isRight,
          result.toOption.get.asInstanceOf[Diagram.StateDiagram].statements.size == 3,
        )
      },
      test("parses self-transition") {
        val result = fastparse.parse("Processing --> Processing: Retry", MermaidParser.stateTransition(using _))
        val t      = result.get.value.transition
        assertTrue(t.from == "Processing", t.to == "Processing", t.label == Some("Retry"))
      },
      test("parses note with alias") {
        val input =
          """note right of Processing as procNote
            |  timeout: 5m
            |end note""".stripMargin
        val result = fastparse.parse(input, MermaidParser.noteSt(using _))
        val note   = result.get.value
        assertTrue(
          note.stateId == "Processing",
          note.alias == Some("procNote"),
          note.text == "timeout: 5m",
        )
      },
      test("note alias is optional") {
        val input =
          """note right of Processing
            |  some text
            |end note""".stripMargin
        val result = fastparse.parse(input, MermaidParser.noteSt(using _))
        val note   = result.get.value
        assertTrue(note.alias.isEmpty)
      },
      test("parses state diagram with aliased note") {
        val input =
          """stateDiagram-v2
            |    [*] --> Created
            |    note right of Created as createdNote
            |      hello
            |    end note
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
        val stmts = result.toOption.get.asInstanceOf[Diagram.StateDiagram].statements
        val notes = stmts.collect { case n: StateStatement.NoteSt => n }
        assertTrue(notes.size == 1, notes.head.alias == Some("createdNote"))
      },
    ),
    suite("subgraphs")(
      test("parses a subgraph with a label and inner edge") {
        val input =
          """flowchart TD
            |    subgraph ingest [Ingest]
            |        Fetch[Fetch] --> Parse[Parse]
            |    end
            |    Parse --> Store[(Store)]
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
        val stmts = result.toOption.get.asInstanceOf[Diagram.Flowchart].statements
        val subs  = stmts.collect { case s: FlowStatement.SubgraphSt => s }
        assertTrue(
          subs.size == 1,
          subs.head.id == "ingest",
          subs.head.label == Some("Ingest"),
          subs.head.statements.size == 1,
          // The statement AFTER the subgraph must survive — if `end` were consumed as a node id the
          // subgraph would fail to close and this edge would land inside it (or not parse at all).
          stmts.collect { case FlowStatement.EdgeSt(e, _, _) => e.from -> e.to } == List("Parse" -> "Store"),
        )
      },
      test("parses a subgraph with an inner direction") {
        val input =
          """flowchart TD
            |    subgraph inner
            |        direction LR
            |        A[a] --> B[b]
            |    end
            |""".stripMargin
        val result = MermaidParser.parse(input)
        val subs   = result.toOption.get
          .asInstanceOf[Diagram.Flowchart]
          .statements
          .collect { case s: FlowStatement.SubgraphSt => s }
        assertTrue(subs.size == 1, subs.head.direction == Some(Direction.LR), subs.head.label.isEmpty)
      },
      test("`end` closes a subgraph, but `endpoint` is still a node id") {
        // The guard is on the bare keyword; an identifier that merely starts with "end" is a node.
        val input =
          """flowchart TD
            |    subgraph s
            |        endpoint[Endpoint]
            |    end
            |    endpoint --> Done[Done]
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
        val stmts = result.toOption.get.asInstanceOf[Diagram.Flowchart].statements
        val subs  = stmts.collect { case s: FlowStatement.SubgraphSt => s }
        assertTrue(
          subs.size == 1,
          subs.head.statements == List(FlowStatement.NodeSt(NodeDef("endpoint", Some("Endpoint"), NodeShape.Rect))),
        )
      },
      test("parses nested subgraphs") {
        val input =
          """flowchart TD
            |    subgraph outer [Outer]
            |        subgraph inner [Inner]
            |            A[a] --> B[b]
            |        end
            |        B --> C[c]
            |    end
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
        val outer = result.toOption.get
          .asInstanceOf[Diagram.Flowchart]
          .statements
          .collect { case s: FlowStatement.SubgraphSt => s }
        assertTrue(
          outer.size == 1,
          outer.head.id == "outer",
          outer.head.statements.collect { case s: FlowStatement.SubgraphSt => s.id } == List("inner"),
        )
      },
      test("parses a subgraph with a slash in its label") {
        val input =
          """flowchart TD
            |    subgraph s ["Script / Command"]
            |        A[a] --> B[b]
            |    end
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
        val subs = result.toOption.get
          .asInstanceOf[Diagram.Flowchart]
          .statements
          .collect { case s: FlowStatement.SubgraphSt => s }
        assertTrue(subs.size == 1, subs.head.label.contains("Script / Command"))
      },
      test("parses a subgraph with an unquoted slash in its label") {
        val input =
          """flowchart TD
            |    subgraph s [Script / Command]
            |        A[a] --> B[b]
            |    end
            |""".stripMargin
        val result = MermaidParser.parse(input)
        assertTrue(result.isRight)
        val subs = result.toOption.get
          .asInstanceOf[Diagram.Flowchart]
          .statements
          .collect { case s: FlowStatement.SubgraphSt => s }
        assertTrue(subs.size == 1, subs.head.label.contains("Script / Command"))
      },
      test("a subgraph's nodes and edges are collected for layout") {
        val input =
          """flowchart TD
            |    subgraph s [S]
            |        A[a] --> B[b]
            |    end
            |""".stripMargin
        val stmts = MermaidParser.parse(input).toOption.get.asInstanceOf[Diagram.Flowchart].statements
        assertTrue(
          StyleResolver.collectNodes(stmts).keySet == Set("A", "B"),
          StyleResolver.collectEdges(stmts).size == 1,
          StyleResolver.collectSubgraphs(stmts).map(_.nodeIds) == List(Set("A", "B")),
        )
      },
    ),
    suite("click")(
      test("parses callback with tooltip") {
        val input =
          """flowchart LR
            |  A --> B
            |  click A callback "Tip A"
            |""".stripMargin
        MermaidParser.parse(input) match
          case Left(err)                          => assertTrue(err.isEmpty)
          case Right(Diagram.Flowchart(_, stmts)) =>
            val click = stmts.collect { case FlowStatement.ClickSt(b) => b }.head
            assertTrue(
              click.nodeId == "A",
              click.callbackName.contains("callback"),
              click.tooltip.contains("Tip A"),
            )
          case Right(other) => assertTrue(other.isInstanceOf[Diagram.Flowchart])
        end match
      },
      test("parses call callback() form") {
        val input =
          """flowchart LR
            |  A --> B
            |  click A call myFn() "Hi"
            |""".stripMargin
        val stmts = MermaidParser.parse(input).toOption.get.asInstanceOf[Diagram.Flowchart].statements
        val click = stmts.collect { case FlowStatement.ClickSt(b) => b }.head
        assertTrue(click.callbackName.contains("myFn"), click.tooltip.contains("Hi"))
      },
      test("parses href with tooltip and target") {
        val input =
          """flowchart LR
            |  A --> B
            |  click B href "https://example.com" "Go" _blank
            |""".stripMargin
        val stmts = MermaidParser.parse(input).toOption.get.asInstanceOf[Diagram.Flowchart].statements
        val click = stmts.collect { case FlowStatement.ClickSt(b) => b }.head
        assertTrue(
          click.href.contains("https://example.com"),
          click.tooltip.contains("Go"),
          click.linkTarget.contains("_blank"),
        )
      },
      test("parses bare quoted URL as href") {
        val input =
          """flowchart LR
            |  A --> B
            |  click B "https://example.com" "Go"
            |""".stripMargin
        val stmts = MermaidParser.parse(input).toOption.get.asInstanceOf[Diagram.Flowchart].statements
        val click = stmts.collect { case FlowStatement.ClickSt(b) => b }.head
        assertTrue(click.href.contains("https://example.com"), click.tooltip.contains("Go"))
      },
    ),
  )
end ParserSpec
