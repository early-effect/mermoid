package mermoid

import zio.*
import zio.test.*

object DiagramLayoutSpec extends ZIOSpecDefault:

  private val simpleFlow =
    Mermaid("""flowchart LR
      |  A[Start] --> B[End]
      |""".stripMargin)

  /** A long LR chain so viewport tests have room to compress. */
  private val wideFlow =
    Mermaid("""flowchart LR
      |  A --> B
      |  B --> C
      |  C --> D
      |  D --> E
      |  E --> F
      |""".stripMargin)

  private def ranked(
      source: Mermaid,
      config: RenderConfig = RenderConfig(),
      viewport: Option[Viewport] = None,
  ): IO[Scene, DiagramScene] =
    DiagramLayout.scene(source.diagram, config, viewport) match
      case Scene.Ranked(scene) => ZIO.succeed(scene)
      case other               => ZIO.fail(other)

  def spec = suite("DiagramLayout")(
    test("unconstrained scene matches SvgRenderer tree size") {
      for scene <- ranked(simpleFlow)
      yield
        val svg = SvgRenderer.paint(Scene.Ranked(scene))
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
      for
        wide   <- ranked(wideFlow, viewport = None)
        narrow <- ranked(wideFlow, viewport = Some(Viewport(280)))
      yield assertTrue(
        narrow.config.layout.hSpacing < wide.config.layout.hSpacing ||
          narrow.direction != wide.direction,
        narrow.width > 0,
      )
    },
    test("flipDirectionBelow swaps LR to TB when narrow") {
      for scene <- ranked(
          wideFlow,
          RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
          Some(Viewport(400)),
        )
      yield assertTrue(scene.direction == Direction.TB)
    },
    test("default config keeps authored TD in a column wider than 640") {
      val src =
        Mermaid("""flowchart TD
          |  A --> B
          |  B --> C
          |""".stripMargin)
      for scene <- ranked(src, viewport = Some(Viewport(832)))
      yield assertTrue(scene.direction == Direction.TD)
    },
    test("opt-in flip turns a wide TD into LR") {
      val src =
        Mermaid("""flowchart TD
          |  A --> B
          |  B --> C
          |""".stripMargin)
      for scene <- ranked(
          src,
          RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
          Some(Viewport(832)),
        )
      yield assertTrue(scene.direction == Direction.LR)
    },
    test("fitScale does not shrink a wide scene below one half") {
      for scene <- ranked(simpleFlow)
      yield assertTrue(Scene.Ranked(scene.copy(width = 3037)).fitScale(961) == 0.5)
    },
    test("fitScale stays at 1 when the scene is narrower than the column") {
      for scene <- ranked(simpleFlow)
      yield assertTrue(Scene.Ranked(scene.copy(width = 400)).fitScale(961) == 1.0)
    },
    test("scale-to-fit off keeps scale at 1 for a wider scene") {
      for scene <- ranked(simpleFlow)
      yield
        val off = scene.copy(width = 3037, config = RenderConfig(responsive = ResponsiveConfig(fit = ContainerFit.Off)))
        assertTrue(Scene.Ranked(off).fitScale(961) == 1.0)
    },
    test("narrow keeps vertical authors vertical") {
      val src =
        Mermaid("""stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin)
      for scene <- ranked(src, viewport = Some(Viewport(360)))
      yield assertTrue(scene.direction == Direction.TB, scene.height > scene.width * 0.6)
    },
    test("opt-in flip turns a wide vertical state diagram horizontal") {
      val src =
        Mermaid("""stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin)
      for scene <- ranked(
          src,
          RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
          Some(Viewport(720)),
        )
      yield assertTrue(scene.direction == Direction.LR, scene.width > scene.height * 0.6)
    },
    test("a wider column keeps TD and gets more vertical spacing") {
      val src =
        Mermaid("""flowchart TD
          |  A --> B
          |  B --> C
          |  C --> D
          |  D --> E
          |  E --> F
          |""".stripMargin)
      for
        medium <- ranked(src, viewport = Some(Viewport(640)))
        wide   <- ranked(src, viewport = Some(Viewport(900)))
      yield assertTrue(
        medium.direction == Direction.TD,
        wide.direction == Direction.TD,
        wide.config.layout.vSpacing > medium.config.layout.vSpacing,
      )
    },
    test("opt-in flip expands horizontal spacing for the same orientation") {
      val src =
        Mermaid("""stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Done
          |  Done --> [*]
          |""".stripMargin)
      val flip = RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640)))
      for
        medium <- ranked(src, flip, Some(Viewport(640)))
        wide   <- ranked(src, flip, Some(Viewport(900)))
      yield assertTrue(
        medium.direction == Direction.LR,
        wide.direction == Direction.LR,
        wide.config.layout.hSpacing > medium.config.layout.hSpacing,
        wide.width > medium.width,
      )
    },
    test("state note dodges the next node in LR") {
      val src =
        Mermaid("""stateDiagram-v2
          |  [*] --> Idle
          |  Idle --> Active: start
          |  Active --> Done: finish
          |  Done --> [*]
          |  note right of Idle
          |    Waiting for input
          |  end note
          |""".stripMargin)
      for scene <- ranked(
          src,
          RenderConfig(responsive = ResponsiveConfig(flipDirectionBelow = Some(640))),
          Some(Viewport(900)),
        )
      yield
        val gap   = 10.0
        val clear = (scene.nodeMap.get(NodeId("Idle")), scene.nodeMap.get(NodeId("Active")), scene.notes) match
          case (Some(idle), Some(active), note :: Nil) =>
            val box = NoteRenderer.placeNote(scene.config, note, idle, scene.visibleNodes)
            !box.overlaps(active, gap) && !box.overlaps(idle, gap)
          case _ => false
        assertTrue(scene.direction == Direction.LR, scene.notes.size == 1, clear)
      end for
    },
    test("click tooltips land on the scene") {
      val src =
        Mermaid("""flowchart LR
          |  A --> B
          |  click A callback "Hello A"
          |  click B href "https://example.com" "Go B" _blank
          |""".stripMargin)
      for scene <- ranked(src)
      yield
        val a = scene.interactions.get(NodeId("A"))
        val b = scene.interactions.get(NodeId("B"))
        assertTrue(
          a.flatMap(_.tooltip).contains("Hello A"),
          a.flatMap(_.callbackName).contains("callback"),
          b.flatMap(_.href).contains("https://example.com"),
          b.flatMap(_.tooltip).contains("Go B"),
          b.flatMap(_.linkTarget).contains("_blank"),
        )
      end for
    },
    test("SVG paint emits title for tooltips") {
      val src =
        Mermaid("""flowchart LR
          |  A --> B
          |  click A callback "Tip"
          |""".stripMargin)
      val svg = SvgSerializer.render(SvgRenderer.renderTree(src.diagram))
      assertTrue(svg.contains("<title>Tip</title>"), svg.contains("node-A"))
    },
    test("SVG paint wraps href clicks and omits callback attributes") {
      val src =
        Mermaid("""flowchart LR
          |  A --> B
          |  click B href "https://example.com" "go" _blank
          |""".stripMargin)
      val svg = SvgSerializer.render(SvgRenderer.renderTree(src.diagram))
      assertTrue(
        svg.contains("""href="https://example.com""""),
        svg.contains("""target="_blank""""),
        !svg.contains("data-callback"),
      )
    },
  )
end DiagramLayoutSpec
