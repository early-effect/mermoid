package mermoid

import zio.test.*

object DiagramLayoutSpec extends ZIOSpecDefault:

  private val simpleFlow =
    """flowchart LR
      |  A[Start] --> B[End]
      |""".stripMargin

  /** A long LR chain so viewport tests have room to compress. */
  private val wideFlow =
    """flowchart LR
      |  A --> B
      |  B --> C
      |  C --> D
      |  D --> E
      |  E --> F
      |""".stripMargin

  private def parse(src: String): Diagram =
    MermaidParser.parse(src) match
      case Right(d)  => d
      case Left(err) => throw new IllegalArgumentException(err)

  def spec = suite("DiagramLayout")(
    test("unconstrained scene matches SvgRenderer tree size") {
      val d     = parse(simpleFlow)
      val scene = DiagramLayout.scene(d)
      val svg   = SvgRenderer.paint(scene)
      assertTrue(
        scene.visibleNodes.size == 2,
        scene.edges.size == 1,
        scene.width > 0,
        scene.height > 0,
        svg match
          case SvgNode.Element("svg", _, _) => true
          case _                            => false,
      )
    },
    test("narrow viewport compresses spacing or flips direction") {
      val d      = parse(wideFlow)
      val wide   = DiagramLayout.scene(d, viewport = None)
      val narrow = DiagramLayout.scene(d, viewport = Some(Viewport(280)))
      assertTrue(
        narrow.config.layout.hSpacing < wide.config.layout.hSpacing ||
          narrow.direction != wide.direction,
        narrow.width > 0,
      )
    },
    test("flipDirectionBelow swaps LR to TB when narrow") {
      val d     = parse(wideFlow)
      val scene = DiagramLayout.scene(
        d,
        RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
        Some(Viewport(400)),
      )
      assertTrue(scene.direction == Direction.TB)
    },
    test("narrow keeps vertical authors vertical") {
      val src =
        """stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin
      val scene = DiagramLayout.scene(parse(src), viewport = Some(Viewport(360)))
      assertTrue(scene.direction == Direction.TB, scene.height > scene.width * 0.6)
    },
    test("wide flips vertical authors to horizontal") {
      val src =
        """stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin
      val scene = DiagramLayout.scene(parse(src), viewport = Some(Viewport(720)))
      assertTrue(scene.direction == Direction.LR, scene.width > scene.height * 0.6)
    },
    test("wide expands spacing vs medium for the same orientation") {
      val src =
        """stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin
      val d      = parse(src)
      val medium = DiagramLayout.scene(d, viewport = Some(Viewport(640)))
      val wide   = DiagramLayout.scene(d, viewport = Some(Viewport(900)))
      assertTrue(
        medium.direction == Direction.LR,
        wide.direction == Direction.LR,
        wide.config.layout.hSpacing > medium.config.layout.hSpacing,
        wide.width > medium.width,
      )
    },
    test("state note dodges the next node in LR") {
      val src =
        """stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Active: start
          |  Active --> Done: finish
          |  Done --> [*]
          |  note right of Idle
          |    Waiting for input
          |  end note
          |""".stripMargin
      val scene  = DiagramLayout.scene(parse(src), viewport = Some(Viewport(900)))
      val idle   = scene.nodeMap("Idle")
      val active = scene.nodeMap("Active")
      val note   = scene.notes.head
      val box    = NoteRenderer.placeNote(scene.config, note, idle, scene.visibleNodes)
      val gap    = 10.0
      assertTrue(
        scene.direction == Direction.LR,
        !box.overlaps(active, gap),
        !box.overlaps(idle, gap),
      )
    },
    test("click tooltips land on the scene") {
      val src =
        """flowchart LR
          |  A --> B
          |  click A callback "Hello A"
          |  click B href "https://example.com" "Go B" _blank
          |""".stripMargin
      val scene = DiagramLayout.scene(parse(src))
      assertTrue(
        scene.interactions("A").tooltip.contains("Hello A"),
        scene.interactions("A").callbackName.contains("callback"),
        scene.interactions("B").href.contains("https://example.com"),
        scene.interactions("B").tooltip.contains("Go B"),
        scene.interactions("B").linkTarget.contains("_blank"),
      )
    },
    test("SVG paint emits title for tooltips") {
      val src =
        """flowchart LR
          |  A --> B
          |  click A callback "Tip"
          |""".stripMargin
      val svg = SvgSerializer.render(SvgRenderer.renderTree(parse(src)))
      assertTrue(svg.contains("<title>Tip</title>"), svg.contains("node-A"))
    },
  )
end DiagramLayoutSpec
