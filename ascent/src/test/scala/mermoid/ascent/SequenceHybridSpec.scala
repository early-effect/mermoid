package mermoid.ascent

import ascent.html.Html
import mermoid.*
import zio.*
import zio.test.*

object SequenceHybridSpec extends ZIOSpecDefault:

  private val span =
    Mermaid("""sequenceDiagram
      |    participant Alice
      |    participant Bob
      |    participant Carol
      |    participant Dave
      |    participant Eve
      |    Alice->>Bob: place order
      |    Bob->>Carol: reserve stock and write the durable record
      |    Bob->>Carol: confirm the row after reserve completes
      |    Bob->>Alice: order accepted
      |    Alice->>Dave: ask status
      |    Dave->>Carol: read hold
      |    Dave-->>Alice: accepted or pending
      |    Eve->>Dave: fetch snapshot
      |    Dave-->>Eve: snapshot plus version
      |""".stripMargin)

  private val mixed =
    Mermaid("""sequenceDiagram
      |    actor Alice
      |    participant Bob
      |    Alice->>+Bob: ask
      |    note right of Bob: held the lock
      |    Bob-->>-Alice: yes
      |""".stripMargin)

  def spec = suite("sequence hybrid")(
    test("headers are buttons and the message text is in the page") {
      for html <- Html.render(MermoidAscent.diagram(span))
      yield assertTrue(
        html.contains("mermoid-actor"),
        html.contains(">Alice<"),
        html.contains(">Bob<"),
        html.contains(">Carol<"),
        html.contains(">Dave<"),
        html.contains(">Eve<"),
        html.contains("place order"),
        html.contains("accepted or pending"),
        html.contains("snapshot plus version"),
        html.contains("message-dashed"),
        html.contains("class=\"lifeline\""),
        !html.contains("data-mermoid-direction"),
      )
    },
    test("an actor is a stick figure and a note is a card") {
      for html <- Html.render(MermoidAscent.diagram(mixed))
      yield assertTrue(
        html.contains("mermoid-actor-person"),
        html.contains("mermoid-actor-head"),
        html.contains("mermoid-actor-box"),
        html.contains("mermoid-note"),
        html.contains("held the lock"),
        html.contains("id=\"actor-Alice\""),
      )
    },
  )
end SequenceHybridSpec
