package mermoid

import zio.test.*

object SequenceParserSpec extends ZIOSpecDefault:

  private val arrows =
    """sequenceDiagram
      |A->B: solid
      |A-->B: dashed
      |A->>B: solid head
      |A-->>B: dashed head
      |A<<->>B: solid both
      |A<<-->>B: dashed both
      |A-xB: solid cross
      |A--xB: dashed cross
      |A-)B: solid open
      |A--)B: dashed open
      |""".stripMargin

  private def messages(src: String): Option[List[SequenceStatement.Message]] =
    MermaidParser.parse(src).toOption.collect { case Diagram.Sequence(stmts) =>
      stmts.collect { case m: SequenceStatement.Message => m }
    }

  def spec = suite("SequenceParser")(
    test("the ten arrows keep line, head, and tail apart") {
      val got = messages(arrows).map(_.map(_.arrow))
      assertTrue(
        got == Some(
          List(
            SequenceArrow.Solid,
            SequenceArrow.Dashed,
            SequenceArrow.SolidHead,
            SequenceArrow.DashedHead,
            SequenceArrow.SolidBoth,
            SequenceArrow.DashedBoth,
            SequenceArrow.SolidCross,
            SequenceArrow.DashedCross,
            SequenceArrow.SolidOpen,
            SequenceArrow.DashedOpen,
          )
        )
      )
    },
    test("span fixture names five participants and nine messages in source order") {
      val parsed = MermaidParser.parse(SequenceFixture.span)
      val ids    = parsed match
        case Right(Diagram.Sequence(stmts)) => SequenceModel.participantOrder(stmts).map(_.value)
        case _                              => Nil
      val arrows = parsed match
        case Right(Diagram.Sequence(stmts)) =>
          stmts.collect { case m: SequenceStatement.Message => (m.from.value, m.arrow, m.to.value, m.text) }
        case _ => Nil
      val detail = parsed match
        case Left(err) => err.message
        case Right(_)  => ""
      assertTrue(
        detail.isEmpty,
        ids == List("Alice", "Bob", "Carol", "Dave", "Eve"),
        arrows.lift(6).contains(("Dave", SequenceArrow.DashedHead, "Alice", Some("accepted or pending"))),
        arrows.lift(8).contains(("Dave", SequenceArrow.DashedHead, "Eve", Some("snapshot plus version"))),
        arrows.lift(4).contains(("Alice", SequenceArrow.SolidHead, "Dave", Some("ask status"))),
      )
    },
    test("a repeated declare with the same label is kept") {
      val src =
        """sequenceDiagram
          |participant Alice as One
          |participant Alice as One
          |participant Alice
          |Alice->>Bob: hi
          |""".stripMargin
      val labels = MermaidParser.parse(src) match
        case Right(Diagram.Sequence(stmts)) =>
          stmts.collect { case SequenceStatement.Declare(id, label, _, _) if id.value == "Alice" => label }
        case _ => Nil
      assertTrue(labels == List(Some("One"), Some("One"), None))
    },
    test("a second label for the same id is a conflicting alias") {
      val src =
        """sequenceDiagram
          |participant Alice as One
          |participant Alice as Two
          |""".stripMargin
      assertTrue(
        MermaidParser.parse(src) == Left(ParseError.ConflictingAlias(NodeId("Alice"), "One", "Two"))
      )
    },
    test("changing participant to actor is a conflicting alias") {
      val src =
        """sequenceDiagram
          |participant Alice
          |actor Alice
          |""".stripMargin
      assertTrue(
        MermaidParser.parse(src) == Left(
          ParseError.ConflictingAlias(NodeId("Alice"), "a participant", "an actor")
        )
      )
    },
    test("rect rgb outside 0 to 255 is a bad color") {
      val src =
        """sequenceDiagram
          |rect rgb(999, 0, 0)
          |Alice->>Bob: hi
          |end
          |""".stripMargin
      assertTrue(MermaidParser.parse(src) == Left(ParseError.BadColor("rgb(999, 0, 0)")))
    },
    test("rect rgba alpha outside 0 to 1 is a bad color") {
      val src =
        """sequenceDiagram
          |rect rgba(0, 0, 0, 2)
          |end
          |""".stripMargin
      assertTrue(MermaidParser.parse(src) == Left(ParseError.BadColor("rgba(0, 0, 0, 2)")))
    },
    test("else outside alt is a parse error") {
      val got = MermaidParser.parse(
        """sequenceDiagram
          |Alice->>Bob: hi
          |else no
          |""".stripMargin
      ) match
        case Left(err) => err.message
        case Right(_)  => "parsed"
      assertTrue(got.startsWith("parse error"))
    },
    test("and outside par is a parse error") {
      val got = MermaidParser.parse(
        """sequenceDiagram
          |Alice->>Bob: hi
          |and no
          |""".stripMargin
      ) match
        case Left(err) => err.message
        case Right(_)  => "parsed"
      assertTrue(got.startsWith("parse error"))
    },
    test("autonumber off and a zero step become a legal mode") {
      val off =
        """sequenceDiagram
          |autonumber
          |autonumber off
          |Alice->>Bob: hi
          |""".stripMargin
      val stepped =
        """sequenceDiagram
          |autonumber 3 0
          |Alice->>Bob: hi
          |""".stripMargin
      val offModes = MermaidParser.parse(off) match
        case Right(Diagram.Sequence(stmts)) => stmts.collect { case SequenceStatement.Autonumber(mode) => mode }
        case _                              => Nil
      val stepModes = MermaidParser.parse(stepped) match
        case Right(Diagram.Sequence(stmts)) => stmts.collect { case SequenceStatement.Autonumber(mode) => mode }
        case _                              => Nil
      assertTrue(
        offModes == List(Numbering.On(1, 1), Numbering.Off),
        stepModes == List(Numbering.On(3, 1)),
      )
    },
    test("plus and minus after an arrow are activate and deactivate") {
      val src =
        """sequenceDiagram
          |Alice->>+Bob: open
          |Bob-->>-Alice: close
          |""".stripMargin
      val got = messages(src).map(_.map(_.control))
      assertTrue(got == Some(List(MessageControl.Activate, MessageControl.Deactivate)))
    },
    test("alt else and par and are sections, and a note names its place") {
      val src =
        """sequenceDiagram
          |alt ready
          |  Alice->>Bob: yes
          |else later
          |  Alice->>Bob: no
          |end
          |par left
          |  Alice->>Bob: one
          |and right
          |  Alice->>Carol: two
          |end
          |note right of Bob: held
          |""".stripMargin
      val kinds = MermaidParser.parse(src) match
        case Right(Diagram.Sequence(stmts)) =>
          stmts.collect {
            case SequenceStatement.Group(kind, sections) =>
              kind.toString -> sections.flatMap(_.label)
            case SequenceStatement.Note(place, text) =>
              place.toString -> List(text)
          }
        case other => List(other.toString -> Nil)
      assertTrue(
        kinds.lift(0).exists((k, labels) => k == "Alt" && labels == List("ready", "later")),
        kinds.lift(1).exists((k, labels) => k == "Par" && labels == List("left", "right")),
        kinds.lift(2).exists((k, texts) => k.startsWith("RightOf") && texts == List("held")),
      )
    },
  )
end SequenceParserSpec
