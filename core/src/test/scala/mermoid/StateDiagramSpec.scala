package mermoid

import zio.*
import zio.test.*

object StateDiagramSpec extends ZIOSpecDefault:

  private def parsed(src: String): Either[ParseError, Diagram.StateDiagram] =
    MermaidParser.parse(src).flatMap {
      case diagram: Diagram.StateDiagram => Right(diagram)
      case _                             => Left(ParseError.Failed(0, List("state diagram")))
    }

  private def machine(src: String): Either[ParseError, StateMachine] =
    parsed(src).flatMap(StateModel.resolve)

  private def ranked(src: String): IO[String, DiagramScene] =
    ZIO.fromEither(parsed(src)).mapError(_.message).flatMap { diagram =>
      DiagramLayout.scene(diagram) match
        case Scene.Ranked(scene) => ZIO.succeed(scene)
        case other               => ZIO.fail(other.toString)
    }

  private def inside(node: LayoutNode, frame: PlacedFrame): Boolean =
    val left   = node.center.x - node.width / 2
    val right  = node.center.x + node.width / 2
    val top    = node.center.y - node.height / 2
    val bottom = node.center.y + node.height / 2
    left >= frame.rect.x && right <= frame.rect.x + frame.rect.w &&
    top >= frame.rect.y && bottom <= frame.rect.y + frame.rect.h

  def spec = suite("state diagrams")(
    test("parses the Mermaid state surface") {
      val samples = List(
        """stateDiagram-v2
          |  [*] --> Still
          |  Still --> [*]
          |""".stripMargin,
        """stateDiagram
          |  s2 : This is a state description
          |  state "This is a state description" as s3
          |""".stripMargin,
        """stateDiagram-v2
          |  state First {
          |    [*] --> second
          |    second --> [*]
          |  }
          |""".stripMargin,
        """stateDiagram-v2
          |  accTitle: accessible
          |  hide empty description
          |  note left of Still : one line
          |  [*] --> Still
          |""".stripMargin,
      )
      assertTrue(samples.forall(parsed(_).isRight))
    },
    test("a flat machine keeps its edges and direction") {
      val src =
        """stateDiagram-v2
          |  direction LR
          |  [*] --> Draft
          |  Draft --> Live: go
          |""".stripMargin
      val resolved = machine(src)
      val edges    = resolved.map(_.regions.flatMap(_.edges).map(e => e.from.value -> e.to.value))
      assertTrue(
        resolved.exists(StateModel.isLegacy),
        edges == Right(List("[*]" -> "Draft", "Draft" -> "Live")),
        resolved.exists(_.direction == Direction.LR),
      )
    },
    test("one id in two composites names both parents") {
      val src =
        """stateDiagram-v2
          |  state A { 1 --> 2 }
          |  state B { 2 --> 3 }
          |""".stripMargin
      val message = parsed(src).left.map(_.message)
      assertTrue(message.swap.exists(text => text.contains("2") && text.contains("A") && text.contains("B")))
    },
    test("a divider outside a composite is an error") {
      val err = parsed("stateDiagram-v2\n  --\n").left.map(_.message)
      assertTrue(err.swap.exists(_.contains("divider")))
    },
    test("history stereotypes resolve and an unknown stereotype is named") {
      val ok = machine(
        """stateDiagram-v2
          |  state h <<history>>
          |  state d <<deepHistory>>
          |  [*] --> [H]
          |  [*] --> [H*]
          |""".stripMargin
      )
      val forms = ok.map(_.regions.flatMap(_.nodes).collect { case StateNode.Atom(_, _, form, _, _, _) => form }.toSet)
      val bad   = parsed("stateDiagram-v2\n  state x <<nope>>\n").left.map(_.message)
      assertTrue(
        forms.exists(_.contains(StateForm.ShallowHistory)),
        forms.exists(_.contains(StateForm.DeepHistory)),
        bad.swap.exists(_.contains("nope")),
      )
    },
    test("classDef inside a composite stays on the member") {
      val src =
        """stateDiagram-v2
          |  state Box {
          |    classDef hot fill:#f00
          |    a --> b
          |    class a hot
          |  }
          |""".stripMargin
      val resolved = machine(src)
      val member   = resolved.toOption.flatMap { m =>
        m.regions
          .flatMap(_.nodes)
          .collectFirst { case group: StateNode.Composite =>
            group.regions.flatMap(_.nodes).collectFirst {
              case StateNode.Atom(id, _, _, classes, _, _) if id.value == "a" => classes
            }
          }
          .flatten
      }
      assertTrue(resolved.exists(_.classDefRules.nonEmpty), member.exists(_.contains("hot")))
    },
    test("a nested child stays inside both frames") {
      val src =
        """stateDiagram-v2
          |  direction LR
          |  state Wrapper {
          |    state Parent {
          |      state Child
          |    }
          |    [*] --> Child
          |  }
          |""".stripMargin
      for scene <- ranked(src)
      yield
        val frames = scene.subgraphs
        val child  = scene.visibleNodes.find(_.id.value == "Child")
        val parent = frames.find(_.id == "Parent")
        val outer  = frames.find(_.id == "Wrapper")
        val held   = child.zip(parent).zip(outer).exists { case ((node, inner), wrap) =>
          inside(node, inner) &&
          inner.rect.x >= wrap.rect.x &&
          inner.rect.x + inner.rect.w <= wrap.rect.x + wrap.rect.w
        }
        assertTrue(held)
      end for
    },
    test("an exit from the second region reaches the outside state") {
      val src =
        """stateDiagram-v2
          |  [*] --> Active
          |  state Active {
          |    [*] --> A
          |    A --> B
          |    --
          |    [*] --> OK
          |    OK --> Overheat
          |  }
          |  Overheat --> Offline
          |""".stripMargin
      for scene <- ranked(src)
      yield
        val frame  = scene.subgraphs.find(_.id == "Active")
        val hot    = scene.visibleNodes.find(_.id.value == "Overheat")
        val edge   = scene.edges.exists(e => e.from.value == "Overheat" && e.to.value == "Offline")
        val held   = frame.zip(hot).exists((box, node) => inside(node, box))
        val starts = scene.visibleNodes.filter(_.cssClasses.contains(css.PaintClass.StartEnd.cssName))
        assertTrue(edge, held, starts.size >= 3)
    },
    test("a composite direction does not flip its parent") {
      val src =
        """stateDiagram-v2
          |  direction LR
          |  [*] --> A
          |  A --> B
          |  state B {
          |    direction TB
          |    a --> b
          |  }
          |""".stripMargin
      for scene <- ranked(src)
      yield
        val aNode            = scene.visibleNodes.find(_.id.value == "a")
        val bNode            = scene.visibleNodes.find(_.id.value == "b")
        val outerA           = scene.visibleNodes.find(_.id.value == "A")
        val frame            = scene.subgraphs.find(_.id == "B")
        val vertical         = aNode.zip(bNode).exists((a, b) => a.center.y < b.center.y)
        val parentHorizontal = outerA.zip(frame).exists((a, box) => a.center.x < box.rect.x)
        val held = aNode.zip(frame).zip(bNode).exists { case ((a, box), b) => inside(a, box) && inside(b, box) }
        assertTrue(scene.direction == Direction.LR, vertical, parentHorizontal, held)
      end for
    },
    test("a fork bar covers its successor centers") {
      val src =
        """stateDiagram-v2
          |  state fork_state <<fork>>
          |  [*] --> fork_state
          |  fork_state --> State2
          |  fork_state --> State3
          |""".stripMargin
      for scene <- ranked(src)
      yield
        val bar    = scene.visibleNodes.find(_.id.value == "fork_state")
        val lefts  = scene.visibleNodes.filter(n => n.id.value == "State2" || n.id.value == "State3")
        val covers = bar.exists { node =>
          val x0 = node.center.x - node.width / 2
          val x1 = node.center.x + node.width / 2
          lefts.forall(other => other.center.x >= x0 && other.center.x <= x1)
        }
        assertTrue(covers, lefts.size == 2)
      end for
    },
    test("the end marker is a bullseye and the start marker is a circle") {
      val src =
        """stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> [*]
          |""".stripMargin
      for scene <- ranked(src)
      yield
        val svg   = parsed(src).fold(_.message, diagram => SvgRenderer.render(diagram))
        val start = scene.visibleNodes.find(_.id == NodeId.stateMarker)
        val end   = scene.visibleNodes.find(_.id == NodeId.stateEnd)
        assertTrue(
          start.exists(_.shape == NodeShape.Circle),
          start.exists(!_.cssClasses.contains(css.PaintClass.StateEnd.cssName)),
          end.exists(_.cssClasses.contains(css.PaintClass.StateEnd.cssName)),
          svg.contains("state-end"),
        )
      end for
    },
    test("a flowchart subgraph ranks on its own direction inside its frame") {
      val src =
        """flowchart TD
          |  subgraph ingest [Ingest]
          |    direction LR
          |    Fetch[Fetch] --> Parse[Parse]
          |  end
          |  Parse --> Store[(Store)]
          |""".stripMargin
      for scene <- rankedFlow(src)
      yield
        val fetch = scene.visibleNodes.find(_.id.value == "Fetch")
        val parse = scene.visibleNodes.find(_.id.value == "Parse")
        val frame = scene.subgraphs.find(_.id == "ingest")
        val held  = fetch.zip(parse).zip(frame).exists { case ((left, right), box) =>
          left.center.x < right.center.x && inside(left, box) && inside(right, box)
        }
        assertTrue(held)
    },
  )

  private def rankedFlow(src: String): IO[String, DiagramScene] =
    ZIO.fromEither(MermaidParser.parse(src)).mapError(_.message).flatMap { diagram =>
      DiagramLayout.scene(diagram) match
        case Scene.Ranked(scene) => ZIO.succeed(scene)
        case other               => ZIO.fail(other.toString)
    }
end StateDiagramSpec
