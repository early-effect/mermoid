package mermoid

import mermoid.css.ThemeName
import zio.test.*

object SequenceSurfaceSpec extends ZIOSpecDefault:

  private val life =
    """sequenceDiagram
      |participant Alice
      |create participant Bob
      |Alice->>Bob: hi
      |destroy Bob
      |""".stripMargin

  private def sequence(source: String): Option[SequenceScene] =
    Mermaid.from(source) match
      case Right(mermaid) =>
        DiagramLayout.scene(mermaid.diagram) match
          case Scene.Sequence(scene) => Some(scene)
          case Scene.Ranked(_)       => None
      case Left(_) => None

  private def ranked(source: String, config: RenderConfig = RenderConfig()): Option[DiagramScene] =
    Mermaid.from(source) match
      case Right(mermaid) =>
        DiagramLayout.scene(mermaid.diagram, InitDirective.apply(mermaid.source, config)) match
          case Scene.Ranked(scene) => Some(scene)
          case Scene.Sequence(_)   => None
      case Left(_) => None

  def spec = suite("sequence surface")(
    test("create starts a lifeline and destroy ends it") {
      sequence(life) match
        case None        => assertTrue(false)
        case Some(scene) =>
          (scene.participants.find(_.id.value == "Alice"), scene.participants.find(_.id.value == "Bob")) match
            case (Some(alice), Some(bob)) =>
              (scene.lifelines.find(_.id == alice.id), scene.lifelines.find(_.id == bob.id)) match
                case (Some(aliceLine), Some(bobLine)) =>
                  assertTrue(
                    aliceLine.y0 < bobLine.y0,
                    bob.box.y > alice.box.y,
                    bobLine.destroyed,
                    !aliceLine.destroyed,
                    bobLine.y1 < aliceLine.y1,
                    aliceLine.y0 < aliceLine.y1,
                  )
                case _ => assertTrue(false)
            case _ => assertTrue(false)
    },
    test("an actor is a stick figure with arms and legs") {
      val src =
        """sequenceDiagram
          |actor Alice
          |participant Bob
          |Alice->>Bob: hi
          |""".stripMargin
      Mermaid.from(src) match
        case Right(mermaid) =>
          val svg = SvgRenderer.render(mermaid.diagram)
          assertTrue(svg.split("actor-figure").length >= 6)
        case Left(_) => assertTrue(false)
    },
    test("a boundary stereotype draws an icon") {
      val src =
        """sequenceDiagram
          |participant Bob@{"type": "boundary"}
          |Bob->>Bob: ping
          |""".stripMargin
      Mermaid.from(src) match
        case Right(mermaid) =>
          val svg = SvgRenderer.render(mermaid.diagram)
          assertTrue(svg.contains("actor-stereotype"), svg.contains("id=\"actor-Bob\""), svg.contains("<circle"))
        case Left(_) => assertTrue(false)
    },
    test("an alt fragment has a tab in the top left") {
      val src =
        """sequenceDiagram
          |participant Alice
          |participant Bob
          |alt ready
          |Alice->>Bob: yes
          |else later
          |Alice->>Bob: wait
          |end
          |""".stripMargin
      sequence(src) match
        case None        => assertTrue(false)
        case Some(scene) =>
          scene.groups.headOption match
            case Some(group) =>
              val svg = Mermaid.from(src) match
                case Right(mermaid) => SvgRenderer.render(mermaid.diagram)
                case Left(_)        => ""
              group.tabBox match
                case Some(tab) =>
                  assertTrue(tab.x == group.frame.x, tab.y == group.frame.y, svg.contains("fragment-tab"))
                case None => assertTrue(false)
            case None => assertTrue(false)
      end match
    },
    test("a note is drawn with a folded corner") {
      val src =
        """sequenceDiagram
          |participant Alice
          |note right of Alice: held the lock
          |""".stripMargin
      Mermaid.from(src) match
        case Right(mermaid) =>
          val svg = SvgRenderer.render(mermaid.diagram)
          assertTrue(svg.contains("<path class=\"note-rect\""))
        case Left(_) => assertTrue(false)
    },
    test("linkStyle paints the nth edge") {
      val src =
        """flowchart LR
          |A --> B
          |B --> C
          |linkStyle 1 stroke:#ff00aa
          |""".stripMargin
      Mermaid.from(src) match
        case Right(mermaid) =>
          val svg = SvgRenderer.render(mermaid.diagram)
          assertTrue(svg.split("stroke: #ff00aa").length == 2)
        case Left(error) => assertTrue(false, error.message.isEmpty)
    },
    test("accTitle is the svg title on a flowchart and a sequence") {
      val flow =
        """flowchart LR
          |accTitle: The pipeline
          |A --> B
          |""".stripMargin
      val seq =
        """sequenceDiagram
          |accTitle: The conversation
          |Alice->>Bob: hi
          |""".stripMargin
      (Mermaid.from(flow), Mermaid.from(seq)) match
        case (Right(flowMermaid), Right(seqMermaid)) =>
          val flowSvg = SvgRenderer.render(flowMermaid.diagram)
          val seqSvg  = SvgRenderer.render(seqMermaid.diagram)
          assertTrue(
            flowSvg.contains("<title>The pipeline</title>"),
            seqSvg.contains("<title>The conversation</title>"),
          )
        case _ => assertTrue(false)
    },
    test("an injected measure widens the node") {
      val src =
        """flowchart LR
          |A[Hello]
          |""".stripMargin
      val wide = new TextMeasure:
        def width(text: String, fontSizePx: Double, fontFamily: String): Double = 400
      (ranked(src), ranked(src, RenderConfig(textMeasure = Some(wide)))) match
        case (Some(plain), Some(grown)) =>
          (plain.nodes.find(_.id.value == "A"), grown.nodes.find(_.id.value == "A")) match
            case (Some(before), Some(after)) => assertTrue(after.width > before.width, after.width > 400)
            case _                           => assertTrue(false)
        case _ => assertTrue(false)
    },
    test("init theme dark applies only when the caller left the theme at Default") {
      val src =
        """%%{init: {'theme': 'dark'}}%%
          |flowchart LR
          |A --> B
          |""".stripMargin
      Mermaid.from(src) match
        case Right(mermaid) =>
          val dark   = SvgRenderer.render(mermaid.diagram, InitDirective.apply(mermaid.source, RenderConfig()))
          val forest = SvgRenderer.render(
            mermaid.diagram,
            InitDirective.apply(mermaid.source, RenderConfig(theme = ThemeName.Forest)),
          )
          assertTrue(dark.contains("#1f2020"), forest.contains("#cde498"), !forest.contains("#1f2020"))
        case Left(_) => assertTrue(false)
    },
    test("an unknown stereotype names the raw value") {
      val src = "sequenceDiagram\nparticipant Alice@{\"type\": \"nope\"}\n"
      assertTrue(Mermaid.from(src) == Left(ParseError.UnknownStereotype("nope")))
    },
  )
end SequenceSurfaceSpec
