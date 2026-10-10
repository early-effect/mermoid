package mermoid

import zio.test.*

object SequenceLayoutSpec extends ZIOSpecDefault:

  private def person(scene: SequenceScene, name: String): Option[PlacedParticipant] =
    scene.participants.find(_.id.value == name)

  def spec = suite("SequenceLayout")(
    test("ask status runs from Alice to Dave across Bob and Carol") {
      SequenceFixture.laid(SequenceFixture.span) match
        case None        => assertTrue(false)
        case Some(scene) =>
          val ask = scene.messages.find(_.lines == List("ask status"))
          (person(scene, "Alice"), person(scene, "Bob"), person(scene, "Carol"), person(scene, "Dave"), ask) match
            case (Some(alice), Some(bob), Some(carol), Some(dave), Some(message)) =>
              message.path match
                case MessagePath.Straight(from, to) =>
                  assertTrue(
                    from.x == alice.centerX,
                    to.x == dave.centerX,
                    from.y == to.y,
                    alice.centerX < bob.centerX,
                    bob.centerX < carol.centerX,
                    carol.centerX < dave.centerX,
                  )
                case MessagePath.Hook(_, _, _, _) => assertTrue(false)
            case _ => assertTrue(false)
          end match
    },
    test("the two replies are dashed and point back toward the caller") {
      SequenceFixture.laid(SequenceFixture.span) match
        case None        => assertTrue(false)
        case Some(scene) =>
          val pending  = scene.messages.lift(6)
          val snapshot = scene.messages.lift(8)
          assertTrue(
            pending.exists(m =>
              m.arrow == SequenceArrow.DashedHead && m.from.value == "Dave" && m.to.value == "Alice" &&
                m.lines == List("accepted or pending")
            ),
            snapshot.exists(m =>
              m.arrow == SequenceArrow.DashedHead && m.from.value == "Dave" && m.to.value == "Eve" &&
                m.lines == List("snapshot plus version")
            ),
          )
    },
    test("a long label widens the Bob to Carol gap past columnGap") {
      SequenceFixture.laid(SequenceFixture.span) match
        case None        => assertTrue(false)
        case Some(scene) =>
          (person(scene, "Bob"), person(scene, "Carol")) match
            case (Some(bob), Some(carol)) =>
              val min =
                scene.config.sequence.columnGap + bob.box.w / 2 + carol.box.w / 2
              assertTrue(carol.centerX - bob.centerX > min)
            case _ => assertTrue(false)
    },
    test("lifelines share the header baseline") {
      SequenceFixture.laid(SequenceFixture.span) match
        case None        => assertTrue(false)
        case Some(scene) =>
          val tops = scene.lifelines.map(_.y0)
          assertTrue(scene.lifelines.size == 5, tops.distinct.size == 1)
    },
    test("an actor header is taller and bottom-aligned with a participant") {
      val src =
        """sequenceDiagram
          |actor Alice
          |participant Bob
          |Alice->>Bob: hi
          |""".stripMargin
      SequenceFixture.laid(src) match
        case None        => assertTrue(false)
        case Some(scene) =>
          (person(scene, "Alice"), person(scene, "Bob"), scene.lifelines.headOption) match
            case (Some(alice), Some(bob), Some(line)) =>
              assertTrue(
                alice.kind == ParticipantKind.Actor,
                bob.kind == ParticipantKind.Participant,
                alice.box.h > bob.box.h,
                math.abs((alice.box.y + alice.box.h) - (bob.box.y + bob.box.h)) < 0.01,
                math.abs(line.y0 - (alice.box.y + alice.box.h)) < 0.01,
              )
            case _ => assertTrue(false)
      end match
    },
    test("stacked activation bars step to the right") {
      val src =
        """sequenceDiagram
          |participant Alice
          |participant Bob
          |Alice->>+Bob: open
          |Bob->>+Bob: nest
          |Bob-->>-Bob: inner
          |Bob-->>-Alice: outer
          |""".stripMargin
      SequenceFixture.laid(src) match
        case None        => assertTrue(false)
        case Some(scene) =>
          val bars = scene.activations.filter(_.id.value == "Bob")
          (person(scene, "Bob"), bars.find(_.depth == 0), bars.find(_.depth == 1)) match
            case (Some(bob), Some(outer), Some(inner)) =>
              val w    = scene.config.sequence.activationWidth
              val endY = scene.lifelines.find(_.id.value == "Bob").map(_.y1)
              assertTrue(
                math.abs(outer.rect.x - (bob.centerX - w / 2)) < 0.01,
                math.abs(inner.rect.x - bob.centerX) < 0.01,
                inner.rect.x > outer.rect.x,
                endY.exists(y1 => outer.rect.y + outer.rect.h < y1),
              )
            case _ => assertTrue(false)
          end match
      end match
    },
    test("a minus after the arrow closes the sender") {
      val src =
        """sequenceDiagram
          |participant Alice
          |participant Bob
          |Alice->>+Bob: ask
          |Bob-->>-Alice: done
          |note right of Bob: held the lock
          |""".stripMargin
      SequenceFixture.laid(src) match
        case None        => assertTrue(false)
        case Some(scene) =>
          val bob   = scene.activations.filter(_.id.value == "Bob")
          val alice = scene.activations.filter(_.id.value == "Alice")
          (bob, scene.messages.lift(1), scene.notes.headOption) match
            case (bar :: Nil, Some(done), Some(note)) =>
              val end   = bar.rect.y + bar.rect.h
              val shaft = done.path match
                case MessagePath.Straight(from, _)   => from.y
                case MessagePath.Hook(_, _, _, head) => head.y
              assertTrue(
                alice.isEmpty,
                math.abs(end - shaft) < 0.01,
                end < note.box.y,
              )
            case _ => assertTrue(false)
          end match
      end match
    },
    test("a self message hooks out and back to the same lifeline") {
      val src =
        """sequenceDiagram
          |participant Alice
          |Alice->>Alice: again
          |""".stripMargin
      SequenceFixture.laid(src) match
        case None        => assertTrue(false)
        case Some(scene) =>
          (scene.messages.headOption, scene.lifelines.headOption) match
            case (Some(message), Some(line)) =>
              message.path match
                case MessagePath.Hook(out, down, _, head) =>
                  assertTrue(
                    out.x == line.x,
                    head.x == line.x,
                    math.abs(down.x - (line.x + scene.config.sequence.selfHookWidth)) < 0.01,
                    head.y > out.y,
                  )
                case MessagePath.Straight(_, _) => assertTrue(false)
            case _ => assertTrue(false)
      end match
    },
    test("an empty sequence still has a canvas") {
      SequenceFixture.laid("sequenceDiagram\n") match
        case None        => assertTrue(false)
        case Some(scene) =>
          assertTrue(scene.participants.isEmpty, scene.width > 0, scene.height > 0)
    },
    test("an empty loop still places a frame") {
      val src =
        """sequenceDiagram
          |loop
          |end
          |""".stripMargin
      SequenceFixture.laid(src) match
        case None        => assertTrue(false)
        case Some(scene) =>
          scene.groups.headOption match
            case Some(group) =>
              assertTrue(group.tab.contains("loop"), group.frame.h > 0, group.frame.w > 0)
            case None => assertTrue(false)
    },
    test("autonumber prefix is what widens a tight column") {
      val tight = RenderConfig(sequence = SequenceConfig(columnGap = 8, actorMinWidth = 40))
      val plain =
        """sequenceDiagram
          |participant A
          |participant B
          |A->>B: hi
          |""".stripMargin
      val numbered =
        """sequenceDiagram
          |autonumber
          |participant A
          |participant B
          |A->>B: hi
          |""".stripMargin
      def span(scene: SequenceScene): Option[Double] =
        (scene.participants.headOption, scene.participants.lift(1)) match
          case (Some(left), Some(right)) => Some(right.centerX - left.centerX)
          case _                         => None
      (SequenceFixture.laid(plain, tight), SequenceFixture.laid(numbered, tight)) match
        case (Some(plainScene), Some(numberedScene)) =>
          (span(plainScene), span(numberedScene), numberedScene.messages.headOption.map(_.shown)) match
            case (Some(plainGap), Some(numberedGap), Some(shown)) =>
              assertTrue(numberedGap > plainGap, shown == List("1 hi"))
            case _ => assertTrue(false)
        case _ => assertTrue(false)
    },
    test("a box title sits above the actor's head") {
      val src =
        """sequenceDiagram
          |box rgb(220, 232, 246) People
          |  actor Alice
          |end
          |""".stripMargin
      SequenceFixture.laid(src) match
        case None        => assertTrue(false)
        case Some(scene) =>
          (person(scene, "Alice"), scene.bands.headOption) match
            case (Some(alice), Some(band)) =>
              val lineH = scene.config.layout.lineHeight
              val gap   = alice.box.y - band.rect.y
              assertTrue(gap >= lineH + 14)
            case _ => assertTrue(false)
      end match
    },
  )
end SequenceLayoutSpec
