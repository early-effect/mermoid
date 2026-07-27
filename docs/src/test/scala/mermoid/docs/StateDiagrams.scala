package mermoid.docs

import mermoid.ascent.MermoidAscent
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** `stateDiagram-v2`: transitions, start/end markers, notes. */
object StateDiagrams extends DocSpecSuite:

  private val orderFsm =
    """stateDiagram-v2
      |    [*] --> Pending
      |    Pending --> Paid: payment captured
      |    Pending --> Cancelled: customer cancels
      |    Paid --> Shipped: carrier accepts
      |    Shipped --> Delivered: scan
      |    Delivered --> [*]
      |    Cancelled --> [*]
      |""".stripMargin

  def doc = page("State diagrams")(
    md"""
`stateDiagram-v2` opens a state diagram. There is no in-diagram `direction` keyword; the author default is top-to-bottom.
With a `Viewport`, responsive layout may flip wide diagrams to horizontal so they use available width (same rules as
flowcharts). Without a viewport, layout stays vertical.
""",
    example {
      MermoidAscent.svgDiagram(orderFsm)
    },
    section("Transitions")(
      md"""
`From --> To` declares a transition; `: label` names it. States do not need declaring — every id mentioned by a
transition becomes a state, rendered as a `Round` node labelled with its own id.
""",
      example {
        MermoidAscent.svgDiagram("""stateDiagram-v2
                            |    Idle --> Running: start
                            |    Running --> Idle: stop
                            |""".stripMargin)
      },
      md"""
That is a two-state cycle, and it lays out rather than looping forever — layering breaks cycles.
""",
    ),
    section("Start and end")(
      md"""
`[*]` is the start/end pseudo-state: a filled 16×16 circle carrying the `start-end` class, with no label. Whether it
reads as start or end is positional — `[*] --> A` versus `A --> [*]`.

Both directions in one diagram share the single `[*]` node, because states are collected by id.
""",
      exampleValue {
        import _root_.mermoid.*
        MermaidParser.parse("stateDiagram-v2\n  [*] --> A\n  A --> [*]\n").map(SvgRenderer.render(_)) match
          case Right(svg) => svg.sliding("start-end".length).count(_ == "start-end")
          case Left(_)    => -1
      }.assert(n =>
        // Two occurrences: the CSS rule in the <style> block, and one node's class list —
        // proving both `[*]` mentions collapsed into a single rendered node.
        assertTrue(n == 2)
      ),
    ),
    section("Notes")(
      md"""
```
note right of Idle
  waiting for work
end note
```

`right of` and `left of` are both supported. Note text is multi-line; each line is trimmed and blank lines dropped. A
note renders as a dashed box joined to its state by a dashed connector, and the diagram's bounding box grows to hold it,
including shifting the whole diagram right when a `left of` note would otherwise fall outside the canvas.

When the preferred side would overlap another node (common in horizontal / flipped layouts), the placer tries the other
side and then a vertical offset before settling.
""",
      example {
        MermoidAscent.svgDiagram("""stateDiagram-v2
                            |    [*] --> Idle
                            |    Idle --> Running: start
                            |    Running --> Idle: finish
                            |    note right of Idle
                            |      no work in flight
                            |      polls every 5s
                            |    end note
                            |    note left of Running
                            |      at most one job
                            |    end note
                            |""".stripMargin)
      },
    ),
    section("Note text alignment")(
      md"""
`style <state> noteAlign: left | center | right` sets how that state's note text is aligned. The default is `left`.
""",
      example {
        MermoidAscent.svgDiagram("""stateDiagram-v2
                            |    [*] --> Ready
                            |    Ready --> Done: go
                            |    style Ready noteAlign: center
                            |    note right of Ready
                            |      centered
                            |      note text
                            |    end note
                            |""".stripMargin)
      },
    ),
    section("Note aliases")(
      md"""
Like edges, notes take `as <name>` to pin their element id. Without it a note is `note-{stateId}-{index}`, so adding an
earlier note on the same state renumbers the later ones.
""",
      exampleValue {
        import _root_.mermoid.*
        MermaidParser
          .parse("stateDiagram-v2\n  A --> B\n  note right of A as caveat\n    careful\n  end note\n")
          .map(SvgRenderer.render(_))
          .map(_.contains("""id="note-caveat""""))
      }.assert(r => assertTrue(r == Right(true))),
    ),
    section("Self-transitions")(
      md"""
A state can transition to itself, and stacked self-transitions stack their labels. The diagram's height accounts for the
loops, and for notes pushed below them.
""",
      example {
        MermoidAscent.svgDiagram("""stateDiagram-v2
                            |    [*] --> Retrying
                            |    Retrying --> Retrying: attempt failed
                            |    Retrying --> Retrying: backoff elapsed
                            |    Retrying --> Done: succeeded
                            |""".stripMargin)
      },
    ),
    section("Not yet implemented")(
      md"""
Composite (nested) states, concurrency (`--`), an in-diagram `direction`, and `state X as "long name"` declarations are
not implemented. `click` is flowchart-only; state diagrams have no click statement. A state diagram that needs nesting
can be expressed as a [flowchart](flowcharts.html) with subgraphs today.
"""
    ),
  )
end StateDiagrams
