package mermoid

import zio.test.*
import fastparse.*

object ParserSpec extends ZIOSpecDefault:

  private def parsed[T](result: Parsed[T]): Option[T] = result match
    case Parsed.Success(value, _) => Some(value)
    case _: Parsed.Failure        => None

  private def parsedOf[T](result: Parsed[T])(check: T => TestResult): TestResult = result match
    case Parsed.Success(value, _) => check(value)
    case failure: Parsed.Failure  => assertNever(s"parse failed: ${failure.msg}")

  private def flowchartOf(src: String)(check: Diagram.Flowchart => TestResult): TestResult =
    MermaidParser.parse(src) match
      case Right(diagram: Diagram.Flowchart) => check(diagram)
      case other                             => assertNever(s"expected a flowchart, got $other")

  private def stateDiagramOf(src: String)(check: Diagram.StateDiagram => TestResult): TestResult =
    MermaidParser.parse(src) match
      case Right(diagram: Diagram.StateDiagram) => check(diagram)
      case other                                => assertNever(s"expected a state diagram, got $other")

  def spec = suite("MermaidParser")(
    suite("helpers")(
      test("identifier parses alphanumeric strings") {
        val result = fastparse.parse("hello123", MermaidParser.identifier(using _))
        assertTrue(parsed(result).contains("hello123"))
      },
      test("quotedString parses double-quoted strings") {
        val result = fastparse.parse("\"hello world\"", MermaidParser.quotedString(using _))
        assertTrue(parsed(result).contains("hello world"))
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
          parsed(fastparse.parse(input, MermaidParser.direction(using _))).contains(expected)
        })
      }
    ),
    suite("nodeShape")(
      test("parses rect shape") {
        val result = fastparse.parse("[hello]", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.Rect)))
      },
      test("parses round shape") {
        val result = fastparse.parse("(hello)", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.Round)))
      },
      test("parses stadium shape") {
        val result = fastparse.parse("([hello])", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.Stadium)))
      },
      test("parses rhombus shape") {
        val result = fastparse.parse("{hello}", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.Rhombus)))
      },
      test("parses circle shape") {
        val result = fastparse.parse("((hello))", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.Circle)))
      },
      test("parses double circle shape") {
        val result = fastparse.parse("(((hello)))", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.DoubleCircle)))
      },
      test("parses hexagon shape") {
        val result = fastparse.parse("{{hello}}", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.Hexagon)))
      },
      test("parses subroutine shape") {
        val result = fastparse.parse("[[hello]]", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.Subroutine)))
      },
      test("parses cylinder shape") {
        val result = fastparse.parse("[(hello)]", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("hello", NodeShape.Cylinder)))
      },
      test("parses rect with quoted slash") {
        val result = fastparse.parse("""["a / b"]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Rect)))
      },
      test("parses rect with unquoted slash") {
        val result = fastparse.parse("[a / b]", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Rect)))
      },
      test("parses round with quoted slash") {
        val result = fastparse.parse("""("a / b")""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Round)))
      },
      test("parses round with unquoted slash") {
        val result = fastparse.parse("(a / b)", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Round)))
      },
      test("parses rhombus with quoted slash") {
        val result = fastparse.parse("""{"a / b"}""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Rhombus)))
      },
      test("parses stadium with quoted slash") {
        val result = fastparse.parse("""(["a / b"])""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Stadium)))
      },
      test("parses circle with quoted slash") {
        val result = fastparse.parse("""(("a / b"))""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Circle)))
      },
      test("parses double circle with quoted slash") {
        val result = fastparse.parse("""((("a / b")))""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.DoubleCircle)))
      },
      test("parses hexagon with quoted slash") {
        val result = fastparse.parse("""{{"a / b"}}""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Hexagon)))
      },
      test("parses subroutine with quoted slash") {
        val result = fastparse.parse("""[["a / b"]]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Subroutine)))
      },
      test("parses cylinder with quoted slash") {
        val result = fastparse.parse("""[("a / b")]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Cylinder)))
      },
      test("parses trapezoid with quoted slash") {
        val result = fastparse.parse("""[/"a / b"\]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Trapezoid)))
      },
      test("parses parallelogram with quoted slash") {
        val result = fastparse.parse("""[/"a / b"/]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a / b", NodeShape.Parallelogram)))
      },
      test("parses rect with quoted backslash") {
        val result = fastparse.parse("""["a \ b"]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a \\ b", NodeShape.Rect)))
      },
      test("parses rect with unquoted backslash") {
        val result = fastparse.parse("""[a \ b]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a \\ b", NodeShape.Rect)))
      },
      test("parses rect with quoted pipe") {
        val result = fastparse.parse("""["a | b"]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a | b", NodeShape.Rect)))
      },
      test("parses rect with unquoted pipe") {
        val result = fastparse.parse("[a | b]", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a | b", NodeShape.Rect)))
      },
      test("parses rect with quoted parens") {
        val result = fastparse.parse("""["a (b)"]""", MermaidParser.nodeShape(using _))
        assertTrue(parsed(result).contains(("a (b)", NodeShape.Rect)))
      },
    ),
    suite("nodeDef")(
      test("parses bare identifier as rect node") {
        val result = fastparse.parse("A", MermaidParser.nodeDef(using _))
        assertTrue(parsed(result).contains(NodeDef(NodeId("A"), None, NodeShape.Rect)))
      },
      test("parses node with label") {
        val result = fastparse.parse("A[Hello World]", MermaidParser.nodeDef(using _))
        assertTrue(parsed(result).contains(NodeDef(NodeId("A"), Some("Hello World"), NodeShape.Rect)))
      },
      test("parses node with round shape") {
        val result = fastparse.parse("B(Round Node)", MermaidParser.nodeDef(using _))
        assertTrue(parsed(result).contains(NodeDef(NodeId("B"), Some("Round Node"), NodeShape.Round)))
      },
      test("parses node with quoted slash in label") {
        val result = fastparse.parse("""A["a / b"]""", MermaidParser.nodeDef(using _))
        assertTrue(parsed(result).contains(NodeDef(NodeId("A"), Some("a / b"), NodeShape.Rect)))
      },
      test("parses node with unquoted slash in label") {
        val result = fastparse.parse("A[a / b]", MermaidParser.nodeDef(using _))
        assertTrue(parsed(result).contains(NodeDef(NodeId("A"), Some("a / b"), NodeShape.Rect)))
      },
      test("parses ::: class suffix on a bare id") {
        val result = fastparse.parse("A:::hot", MermaidParser.nodeDef(using _))
        assertTrue(parsed(result).contains(NodeDef(NodeId("A"), None, NodeShape.Rect, List("hot"))))
      },
      test("parses ::: class suffix after a labelled shape") {
        val result = fastparse.parse("A[Hello]:::hot,cold", MermaidParser.nodeDef(using _))
        assertTrue(parsed(result).contains(NodeDef(NodeId("A"), Some("Hello"), NodeShape.Rect, List("hot", "cold"))))
      },
    ),
    suite("edgeStyle")(
      test("parses arrow -->") {
        val result = fastparse.parse("-->", MermaidParser.edgeStyle(using _))
        assertTrue(parsed(result).contains((EdgeStyle.Arrow, None)))
      },
      test("parses open ---") {
        val result = fastparse.parse("---", MermaidParser.edgeStyle(using _))
        assertTrue(parsed(result).contains((EdgeStyle.Open, None)))
      },
      test("parses dotted -.->") {
        val result = fastparse.parse("-.->", MermaidParser.edgeStyle(using _))
        assertTrue(parsed(result).contains((EdgeStyle.Dotted, None)))
      },
      test("parses thick ==>") {
        val result = fastparse.parse("==>", MermaidParser.edgeStyle(using _))
        assertTrue(parsed(result).contains((EdgeStyle.Thick, None)))
      },
      test("parses arrow with pipe label --> |label|") {
        val result = fastparse.parse("--> |yes|", MermaidParser.edgeStyle(using _))
        assertTrue(parsed(result).contains((EdgeStyle.Arrow, Some("yes"))))
      },
      test("parses arrow with inline label -- text -->") {
        val result = fastparse.parse("-- text -->", MermaidParser.edgeStyle(using _))
        assertTrue(parsed(result).contains((EdgeStyle.Arrow, Some("text"))))
      },
    ),
    suite("edgeSt")(
      test("parses simple edge") {
        val result   = fastparse.parse("A --> B", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
          NodeDef(NodeId("A"), None, NodeShape.Rect),
          NodeDef(NodeId("B"), None, NodeShape.Rect),
        )
        assertTrue(parsed(result).contains(expected))
      },
      test("parses edge with labeled target node") {
        val result   = fastparse.parse("A --> B[Hello]", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
          NodeDef(NodeId("A"), None, NodeShape.Rect),
          NodeDef(NodeId("B"), Some("Hello"), NodeShape.Rect),
        )
        assertTrue(parsed(result).contains(expected))
      },
      test("parses edge with label") {
        val result   = fastparse.parse("A --> |yes| B", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, Some("yes")),
          NodeDef(NodeId("A"), None, NodeShape.Rect),
          NodeDef(NodeId("B"), None, NodeShape.Rect),
        )
        assertTrue(parsed(result).contains(expected))
      },
      test("parses edge with labeled source node") {
        val result   = fastparse.parse("A[Start] --> B[End]", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None),
          NodeDef(NodeId("A"), Some("Start"), NodeShape.Rect),
          NodeDef(NodeId("B"), Some("End"), NodeShape.Rect),
        )
        assertTrue(parsed(result).contains(expected))
      },
      test("parses edge with alias") {
        val result   = fastparse.parse("A --> B as myEdge", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, None, Some("myEdge")),
          NodeDef(NodeId("A"), None, NodeShape.Rect),
          NodeDef(NodeId("B"), None, NodeShape.Rect),
        )
        assertTrue(parsed(result).contains(expected))
      },
      test("parses edge with label and alias") {
        val result   = fastparse.parse("A -->|fast| B as fastEdge", MermaidParser.edgeSt(using _))
        val expected = FlowStatement.EdgeSt(
          Edge(NodeId("A"), NodeId("B"), EdgeStyle.Arrow, Some("fast"), Some("fastEdge")),
          NodeDef(NodeId("A"), None, NodeShape.Rect),
          NodeDef(NodeId("B"), None, NodeShape.Rect),
        )
        assertTrue(parsed(result).contains(expected))
      },
      test("edge alias is optional") {
        val result = fastparse.parse("A --> B", MermaidParser.edgeSt(using _))
        assertTrue(parsed(result).exists(_.edge.alias.isEmpty))
      },
      test("parses a chained edge into one hop per pair") {
        val result    = fastparse.parse("A --> B --> C", MermaidParser.edgeChain(using _))
        val hops      = parsed(result).map(_.map(_.edge).map(e => (e.from, e.to, e.style)))
        val (a, b, c) = (NodeId("A"), NodeId("B"), NodeId("C"))
        assertTrue(hops.contains(List((a, b, EdgeStyle.Arrow), (b, c, EdgeStyle.Arrow))))
      },
      test("chained edge alias lands on the last hop") {
        val result  = fastparse.parse("A --> B --> C as last", MermaidParser.edgeChain(using _))
        val aliases = parsed(result).map(_.map(_.edge.alias))
        assertTrue(aliases.contains(List(None, Some("last"))))
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
      test("parses flowchart ::: class suffixes") {
        val input =
          """flowchart LR
            |  classDef hot fill:#ffdddd,stroke:#cc0000
            |  A[Start]:::hot --> B[End]
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val classes = diagram.statements.collect { case FlowStatement.EdgeSt(_, from, _) => from.cssClasses }
          assertTrue(classes.headOption.contains(List("hot")))
        }
      },
      test("parses flowchart with multiple statements") {
        val input =
          """flowchart TD
            |  A[Start] --> B{Decision}
            |  B --> |yes| C[OK]
            |  B --> |no| D[Fail]
            |""".stripMargin
        flowchartOf(input)(diagram => assertTrue(diagram.statements.size == 3))
      },
      test("parses chained edges in a flowchart") {
        flowchartOf("flowchart LR\n  A --> B --> C\n") { diagram =>
          val edges = diagram.statements.collect { case FlowStatement.EdgeSt(e, _, _) => (e.from, e.to) }
          assertTrue(edges == List((NodeId("A"), NodeId("B")), (NodeId("B"), NodeId("C"))))
        }
      },
      test("ignores %% comments and init directives") {
        val input =
          """%% a comment
            |%%{init: {'theme': 'dark'}}%%
            |flowchart LR
            |  A --> B
            |  %% another
            |  B --> C
            |""".stripMargin
        flowchartOf(input)(diagram => assertTrue(diagram.statements.size == 2))
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
        flowchartOf(input)(diagram => assertTrue(diagram.direction == Direction.TB))
      },
      test("parses flowchart with unquoted slashes in node labels") {
        val input =
          """flowchart TD
            |  Shell[zipx-shell · Script / Command / Word / ShTest] --> Steps2[Step.run]
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val labels = StyleResolver.collectNodes(diagram.statements).map((id, node) => id -> node.label)
          assertTrue(
            labels.get(NodeId("Shell")).flatten.contains("zipx-shell · Script / Command / Word / ShTest"),
            labels.get(NodeId("Steps2")).flatten.contains("Step.run"),
          )
        }
      },
      test("parses flowchart with quoted slashes and pipes in node labels") {
        val input =
          """flowchart LR
            |  A["a / b"] --> B["c | d"]
            |  C[a \ b] --> D(a / b)
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val labels = StyleResolver.collectNodes(diagram.statements).map((id, node) => id -> node.label)
          assertTrue(
            labels.get(NodeId("A")).flatten.contains("a / b"),
            labels.get(NodeId("B")).flatten.contains("c | d"),
            labels.get(NodeId("C")).flatten.contains("a \\ b"),
            labels.get(NodeId("D")).flatten.contains("a / b"),
          )
        }
      },
      test("parse failure after a valid header points past line 1") {
        val input =
          """flowchart LR
            |  A[broken
            |""".stripMargin
        val err = MermaidParser.parse(input).swap.toOption
        assertTrue(err.nonEmpty, !err.exists(_.message.contains("Position 1:1")))
      },
    ),
    suite("state diagram")(
      test("parses simple state transition") {
        val result = fastparse.parse("Created --> PaymentProcessing", MermaidParser.stateTransition(using _))
        parsedOf(result) { st =>
          val t = st.transition
          assertTrue(t.from == NodeId("Created"), t.to == NodeId("PaymentProcessing"), t.label.isEmpty)
        }
      },
      test("parses state transition with label") {
        val result =
          fastparse.parse("Created --> PaymentProcessing: InitiatePayment", MermaidParser.stateTransition(using _))
        parsedOf(result) { st =>
          val t = st.transition
          assertTrue(
            t.from == NodeId("Created"),
            t.to == NodeId("PaymentProcessing"),
            t.label == Some("InitiatePayment"),
          )
        }
      },
      test("parses [*] start state") {
        val result = fastparse.parse("[*] --> Created", MermaidParser.stateTransition(using _))
        parsedOf(result) { st =>
          assertTrue(st.transition.from == NodeId("[*]"), st.transition.to == NodeId("Created"))
        }
      },
      test("parses full state diagram") {
        val input =
          """stateDiagram-v2
            |    [*] --> Created
            |    Created --> Processing: Start
            |    Processing --> Done: Finish
            |""".stripMargin
        stateDiagramOf(input)(diagram => assertTrue(diagram.statements.size == 3))
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
        stateDiagramOf(input)(diagram => assertTrue(diagram.statements.size == 3))
      },
      test("parses self-transition") {
        val result = fastparse.parse("Processing --> Processing: Retry", MermaidParser.stateTransition(using _))
        parsedOf(result) { st =>
          val t = st.transition
          assertTrue(t.from == NodeId("Processing"), t.to == NodeId("Processing"), t.label == Some("Retry"))
        }
      },
      test("parses note with alias") {
        val input =
          """note right of Processing as procNote
            |  timeout: 5m
            |end note""".stripMargin
        val result = fastparse.parse(input, MermaidParser.noteSt(using _))
        parsedOf(result) { note =>
          assertTrue(
            note.stateId == NodeId("Processing"),
            note.alias == Some("procNote"),
            note.text == "timeout: 5m",
          )
        }
      },
      test("note alias is optional") {
        val input =
          """note right of Processing
            |  some text
            |end note""".stripMargin
        val result = fastparse.parse(input, MermaidParser.noteSt(using _))
        assertTrue(parsed(result).exists(_.alias.isEmpty))
      },
      test("parses state diagram with aliased note") {
        val input =
          """stateDiagram-v2
            |    [*] --> Created
            |    note right of Created as createdNote
            |      hello
            |    end note
            |""".stripMargin
        stateDiagramOf(input) { diagram =>
          val aliases = diagram.statements.collect { case n: StateStatement.NoteSt => n.alias }
          assertTrue(aliases == List(Some("createdNote")))
        }
      },
      test("parses classDef, class, and ::: on a state diagram") {
        val input =
          """stateDiagram-v2
            |    classDef happy fill:#1f4a35,stroke:#7dcea0
            |    [*] --> Green:::happy
            |    Green --> Yellow: Timer
            |    class Yellow warn
            |""".stripMargin
        stateDiagramOf(input) { diagram =>
          val stmts = diagram.statements
          val defs  = stmts.collect { case StateStatement.ClassDefSt(name, styles) =>
            name -> styles.get(css.CssProperty.Fill)
          }
          val cls   = stmts.collect { case StateStatement.ClassSt(ids, name) => ids -> name }
          val green = stmts.collect { case StateStatement.TransitionSt(t) if t.to == NodeId("Green") => t.toClasses }
          assertTrue(
            defs == List("happy" -> Some("#1f4a35")),
            cls == List(List(NodeId("Yellow")) -> "warn"),
            green == List(List("happy")),
          )
        }
      },
      test("parses style fill and noteAlign on a state") {
        val input =
          """stateDiagram-v2
            |    [*] --> Ready
            |    style Ready fill:#ddeeff,noteAlign:center
            |""".stripMargin
        stateDiagramOf(input) { diagram =>
          val styles = diagram.statements.collect { case StateStatement.StyleSt(id, s) =>
            (id, s.noteAlign, s.paint.get(css.CssProperty.Fill))
          }
          assertTrue(styles == List((NodeId("Ready"), Some(NoteTextAlign.Center), Some("#ddeeff"))))
        }
      },
      test("parses direction LR and keeps classDef on the statement list") {
        val input =
          """stateDiagram-v2
            |    direction LR
            |    [*] --> Draft
            |    Draft --> JourneyPreparing: Launch
            |    JourneyPreparing --> PredictionsRequested: JourneyPublished
            |    JourneyPreparing --> JourneyPreparationFaulted: DsmlBridgeFailed
            |    JourneyPreparationFaulted --> JourneyPreparing: Retry
            |    classDef fault fill:#3a2a10,stroke:#f59e0b
            |    class JourneyPreparationFaulted fault
            |""".stripMargin
        stateDiagramOf(input) { diagram =>
          val defs = diagram.statements.collect { case StateStatement.ClassDefSt(name, _) => name }
          assertTrue(
            diagram.direction == Direction.LR,
            defs == List("fault"),
            diagram.statements.size == 7,
          )
        }
      },
      test("direction between transitions wins over an earlier one, and a missing direction stays TB") {
        val between =
          """stateDiagram-v2
            |    [*] --> Draft
            |    direction TB
            |    Draft --> Live: go
            |    direction LR
            |""".stripMargin
        val plain =
          """stateDiagram-v2
            |    [*] --> Draft
            |""".stripMargin
        stateDiagramOf(between) { mid =>
          val edges = mid.statements.collect { case StateStatement.TransitionSt(t) => t.from -> t.to }
          assertTrue(
            mid.direction == Direction.LR,
            edges == List(NodeId("[*]") -> NodeId("Draft"), NodeId("Draft") -> NodeId("Live")),
          )
        } && stateDiagramOf(plain)(bare => assertTrue(bare.direction == Direction.TB))
      },
      test("a state id named direction is still a transition") {
        val input =
          """stateDiagram-v2
            |    direction --> Next: go
            |""".stripMargin
        stateDiagramOf(input) { diagram =>
          val edges = diagram.statements.collect { case StateStatement.TransitionSt(t) => (t.from, t.to, t.label) }
          assertTrue(
            diagram.direction == Direction.TB,
            edges == List((NodeId("direction"), NodeId("Next"), Some("go"))),
          )
        }
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
        flowchartOf(input) { diagram =>
          val stmts = diagram.statements
          val subs  = stmts.collect { case s: FlowStatement.SubgraphSt => (s.id, s.label, s.statements.size) }
          assertTrue(
            subs == List(("ingest", Some("Ingest"), 1)),
            // The statement AFTER the subgraph must survive — if `end` were consumed as a node id the
            // subgraph would fail to close and this edge would land inside it (or not parse at all).
            stmts.collect { case FlowStatement.EdgeSt(e, _, _) => e.from -> e.to } == List(
              NodeId("Parse") -> NodeId("Store")
            ),
          )
        }
      },
      test("parses a subgraph with an inner direction") {
        val input =
          """flowchart TD
            |    subgraph inner
            |        direction LR
            |        A[a] --> B[b]
            |    end
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val subs = diagram.statements.collect { case s: FlowStatement.SubgraphSt => (s.direction, s.label) }
          assertTrue(subs == List((Some(Direction.LR), None)))
        }
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
        flowchartOf(input) { diagram =>
          val subs = diagram.statements.collect { case s: FlowStatement.SubgraphSt => s.statements }
          assertTrue(
            subs == List(List(FlowStatement.NodeSt(NodeDef(NodeId("endpoint"), Some("Endpoint"), NodeShape.Rect))))
          )
        }
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
        flowchartOf(input) { diagram =>
          val outer = diagram.statements.collect { case s: FlowStatement.SubgraphSt =>
            s.id -> s.statements.collect { case inner: FlowStatement.SubgraphSt => inner.id }
          }
          assertTrue(outer == List("outer" -> List("inner")))
        }
      },
      test("parses a subgraph with a slash in its label") {
        val input =
          """flowchart TD
            |    subgraph s ["Script / Command"]
            |        A[a] --> B[b]
            |    end
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val labels = diagram.statements.collect { case s: FlowStatement.SubgraphSt => s.label }
          assertTrue(labels == List(Some("Script / Command")))
        }
      },
      test("parses a subgraph with an unquoted slash in its label") {
        val input =
          """flowchart TD
            |    subgraph s [Script / Command]
            |        A[a] --> B[b]
            |    end
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val labels = diagram.statements.collect { case s: FlowStatement.SubgraphSt => s.label }
          assertTrue(labels == List(Some("Script / Command")))
        }
      },
      test("a subgraph's nodes and edges are collected for layout") {
        val input =
          """flowchart TD
            |    subgraph s [S]
            |        A[a] --> B[b]
            |    end
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val stmts = diagram.statements
          assertTrue(
            StyleResolver.collectNodes(stmts).keySet == Set(NodeId("A"), NodeId("B")),
            StyleResolver.collectEdges(stmts).size == 1,
            StyleResolver.collectSubgraphs(stmts).map(_.nodeIds) == List(Set(NodeId("A"), NodeId("B"))),
          )
        }
      },
    ),
    suite("click")(
      test("parses callback with tooltip") {
        val input =
          """flowchart LR
            |  A --> B
            |  click A callback "Tip A"
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val clicks = diagram.statements.collect { case FlowStatement.ClickSt(b) => b }
          assertTrue(
            clicks == List(ClickBinding(NodeId("A"), tooltip = Some("Tip A"), callbackName = Some("callback")))
          )
        }
      },
      test("parses call callback() form") {
        val input =
          """flowchart LR
            |  A --> B
            |  click A call myFn() "Hi"
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val clicks = diagram.statements.collect { case FlowStatement.ClickSt(b) => (b.callbackName, b.tooltip) }
          assertTrue(clicks == List((Some("myFn"), Some("Hi"))))
        }
      },
      test("parses href with tooltip and target") {
        val input =
          """flowchart LR
            |  A --> B
            |  click B href "https://example.com" "Go" _blank
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val clicks = diagram.statements.collect { case FlowStatement.ClickSt(b) => (b.href, b.tooltip, b.linkTarget) }
          assertTrue(clicks == List((Some("https://example.com"), Some("Go"), Some("_blank"))))
        }
      },
      test("parses bare quoted URL as href") {
        val input =
          """flowchart LR
            |  A --> B
            |  click B "https://example.com" "Go"
            |""".stripMargin
        flowchartOf(input) { diagram =>
          val clicks = diagram.statements.collect { case FlowStatement.ClickSt(b) => (b.href, b.tooltip) }
          assertTrue(clicks == List((Some("https://example.com"), Some("Go"))))
        }
      },
    ),
  )
end ParserSpec
