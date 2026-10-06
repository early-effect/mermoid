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
      case Left(err) => throw new IllegalArgumentException(err.message)

  private def ranked(
      d: Diagram,
      config: RenderConfig = RenderConfig(),
      viewport: Option[Viewport] = None,
  ): DiagramScene =
    DiagramLayout.scene(d, config, viewport) match
      case Scene.Ranked(scene) => scene
      case Scene.Sequence(_)   => throw new IllegalArgumentException("expected a ranked scene")

  def spec = suite("DiagramLayout")(
    test("unconstrained scene matches SvgRenderer tree size") {
      val d     = parse(simpleFlow)
      val scene = ranked(d)
      val svg   = SvgRenderer.paint(Scene.Ranked(scene))
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
      val wide   = ranked(d, viewport = None)
      val narrow = ranked(d, viewport = Some(Viewport(280)))
      assertTrue(
        narrow.config.layout.hSpacing < wide.config.layout.hSpacing ||
          narrow.direction != wide.direction,
        narrow.width > 0,
      )
    },
    test("flipDirectionBelow swaps LR to TB when narrow") {
      val d     = parse(wideFlow)
      val scene = ranked(
        d,
        RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
        Some(Viewport(400)),
      )
      assertTrue(scene.direction == Direction.TB)
    },
    test("default config keeps authored TD in a column wider than 640") {
      val src =
        """flowchart TD
          |  A --> B
          |  B --> C
          |""".stripMargin
      val scene = ranked(parse(src), viewport = Some(Viewport(832)))
      assertTrue(scene.direction == Direction.TD)
    },
    test("opt-in flip turns a wide TD into LR") {
      val src =
        """flowchart TD
          |  A --> B
          |  B --> C
          |""".stripMargin
      val scene = ranked(
        parse(src),
        RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
        Some(Viewport(832)),
      )
      assertTrue(scene.direction == Direction.LR)
    },
    test("fitScale does not shrink a wide scene below one half") {
      val wide  = Scene.Ranked(ranked(parse(simpleFlow)).copy(width = 3037))
      val scale = wide.fitScale(961)
      assertTrue(scale == 0.5)
    },
    test("fitScale stays at 1 when the scene is narrower than the column") {
      val scene = Scene.Ranked(ranked(parse(simpleFlow)).copy(width = 400))
      assertTrue(scene.fitScale(961) == 1.0)
    },
    test("scale-to-fit off keeps scale at 1 for a wider scene") {
      val scene = Scene.Ranked(
        ranked(parse(simpleFlow)).copy(
          width = 3037,
          config = RenderConfig(responsive = ResponsiveConfig(fit = ContainerFit.Off)),
        )
      )
      assertTrue(scene.fitScale(961) == 1.0)
    },
    test("narrow keeps vertical authors vertical") {
      val src =
        """stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin
      val scene = ranked(parse(src), viewport = Some(Viewport(360)))
      assertTrue(scene.direction == Direction.TB, scene.height > scene.width * 0.6)
    },
    test("opt-in flip turns a wide vertical state diagram horizontal") {
      val src =
        """stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin
      val scene = ranked(
        parse(src),
        RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
        Some(Viewport(720)),
      )
      assertTrue(scene.direction == Direction.LR, scene.width > scene.height * 0.6)
    },
    test("a wider column keeps TD and gets more vertical spacing") {
      val src =
        """flowchart TD
          |  A --> B
          |  B --> C
          |  C --> D
          |  D --> E
          |  E --> F
          |""".stripMargin
      val d      = parse(src)
      val medium = ranked(d, viewport = Some(Viewport(640)))
      val wide   = ranked(d, viewport = Some(Viewport(900)))
      assertTrue(
        medium.direction == Direction.TD,
        wide.direction == Direction.TD,
        wide.config.layout.vSpacing > medium.config.layout.vSpacing,
      )
    },
    test("opt-in flip expands horizontal spacing for the same orientation") {
      val src =
        """stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin
      val d      = parse(src)
      val flip   = RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640)))
      val medium = ranked(d, flip, Some(Viewport(640)))
      val wide   = ranked(d, flip, Some(Viewport(900)))
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
      val scene = ranked(
        parse(src),
        RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
        Some(Viewport(900)),
      )
      val gap   = 10.0
      val clear = (scene.nodeMap.get("Idle"), scene.nodeMap.get("Active"), scene.notes) match
        case (Some(idle), Some(active), note :: Nil) =>
          val box = NoteRenderer.placeNote(scene.config, note, idle, scene.visibleNodes)
          !box.overlaps(active, gap) && !box.overlaps(idle, gap)
        case _ => false
      assertTrue(scene.direction == Direction.LR, scene.notes.size == 1, clear)
    },
    test("click tooltips land on the scene") {
      val src =
        """flowchart LR
          |  A --> B
          |  click A callback "Hello A"
          |  click B href "https://example.com" "Go B" _blank
          |""".stripMargin
      val scene = ranked(parse(src))
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
    test("SVG paint wraps href clicks and omits callback attributes") {
      val src =
        """flowchart LR
          |  A --> B
          |  click B href "https://example.com" "go" _blank
          |""".stripMargin
      val svg = SvgSerializer.render(SvgRenderer.renderTree(parse(src)))
      assertTrue(
        svg.contains("""href="https://example.com""""),
        svg.contains("""target="_blank""""),
        !svg.contains("data-callback"),
      )
    },
  )
end DiagramLayoutSpec
