package mermoid.docs

import mermoid.ascent.MermoidAscent
import specular.*
import specular.ziotest.DocSpecSuite

/** Sequence diagrams: participants are columns, statements are time. */
object SequenceDiagrams extends DocSpecSuite:

  private val span =
    """sequenceDiagram
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
      |""".stripMargin

  private val fragments =
    """sequenceDiagram
      |    actor Alice
      |    participant Bob
      |    autonumber
      |    Alice->>+Bob: ask
      |    alt ready
      |        Bob-->>Alice: yes
      |    else later
      |        Bob-->>Alice: not yet
      |    end
      |    Bob-->>-Alice: done
      |    note right of Bob: held the lock
      |""".stripMargin

  def doc = page("Sequence diagrams")(
    md"""
A sequence diagram is a timeline. Each participant is a column, in the order it is first named. Each message, note, or
fragment is a row, in source order. There is no ranker, no feedback arc, and no direction flip: `TB` and `LR` are
flowchart words. A narrow viewport compresses `SequenceConfig.columnGap` and `rowPitch`, then the same container fit
used by other diagrams scales the picture. Below the floor, the row scrolls.

Solid arrows are requests. Dashed arrows are replies. A label sits above the shaft. A message may cross columns that
are not its endpoints, and a long label widens the gaps it crosses.
""",
    example {
      MermoidAscent.diagram(span)
    },
    section("Participants and arrows")(
      md"""
`sequenceDiagram` opens the diagram. `participant` is a rounded header. `actor` is a stick figure in the same column.
A message may name someone who was never declared; that id becomes a participant at the end of the column order.

| Arrow | Line | Head | Tail |
|---|---|---|---|
| `->` | solid | none | none |
| `-->` | dashed | none | none |
| `->>` | solid | filled | none |
| `-->>` | dashed | filled | none |
| `<<->>` | solid | filled | filled |
| `<<-->>` | dashed | filled | filled |
| `-x` / `--x` | solid / dashed | cross | none |
| `-)` / `--)` | solid / dashed | open | none |

`A->>+B` activates B at the arrowhead. `B-->>-A` deactivates B, the sender. A second activation on the same lifeline sits to the
right of the bar already open. An extra `deactivate` is a no-op. A bar that is still open at the end of the diagram
runs to the last row.

`autonumber` prefixes messages. `autonumber off` stops. `autonumber 3` starts at 3. `autonumber 3 2` steps by 2. A step
of 0 is read as 1. The number is painted, and it counts toward the width of the label.
""",
      example {
        MermoidAscent.diagram(fragments)
      },
    ),
    section("Notes and fragments")(
      md"""
`note left of A`, `note right of A`, and `note over A,B` each take a row. The text is one line after the colon.

`loop`, `opt`, `critical`, and `break` wrap one block and close with `end`. `alt` / `else` and `par` / `and` add
sections. `else` is only legal inside `alt`. `and` is only legal inside `par`. `rect rgb(r, g, b)` and
`rect rgba(r, g, b, a)` paint a highlight behind the rows they wrap. Channels are 0 to 255. Alpha is 0 to 1.

A later `participant A as "other"` for an id that already has a different label, or a switch between `participant` and
`actor`, is a parse error. Declaring the same id with the same label again changes nothing.

Message text may break a line with a `<br>` or `<br/>` tag, written in the source as those five or six characters. No other HTML is read.
"""
    ),
    section("What is not sequence syntax")(
      md"""
`box` bands, `create` / `destroy`, stereotypes (`@{"type": ...}`), `link` / `links`, and `click` / `style` /
`classDef` inside a sequence diagram are not parsed. A lifeline already runs from the header to the last row, so
create and destroy would only punch a hole in a line that is otherwise one span.
"""
    ),
  )
end SequenceDiagrams
