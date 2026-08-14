package mermoid.docs

import mermoid.ascent.MermoidAscent
import specular.*
import specular.ziotest.DocSpecSuite

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
reads as start or end is positional: `[*] --> A` versus `A --> [*]`.

When a diagram uses both, mermoid paints **two** markers (start keeps id `[*]`, end is `[*]-end`) so ranking does not
cycle through a shared node and flip the layout. A diagram that only has one role still uses a single `[*]` node.
""",
      example {
        MermoidAscent.svgDiagram("""stateDiagram-v2
                            |    [*] --> Active
                            |    Active --> [*]
                            |""".stripMargin)
      },
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
    section("Styling from the diagram source")(
      md"""
`classDef`, `class`, `:::`, and `style` are the same statements as on [flowcharts](flowcharts.html). `classDef` becomes
a CSS rule, `class` / `:::` put the name on the state's node, and `style` can still set `noteAlign` as well as fill and
stroke.
""",
      example {
        MermoidAscent.svgDiagram("""stateDiagram-v2
                            |    classDef happy fill:#1f4a35,stroke:#7dcea0
                            |    classDef warn fill:#4a4030,stroke:#e0c070
                            |    classDef sad fill:#5c2a2a,stroke:#f0a0a0
                            |    [*] --> Green
                            |    Green --> Yellow: Timer
                            |    Yellow --> Red: Timer
                            |    Red --> Green: Timer
                            |    class Green happy
                            |    class Yellow warn
                            |    class Red sad
                            |""".stripMargin)
      },
      md"""
`Green:::happy --> Yellow:::warn` is the same assignment written on the transition.
""",
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
      example {
        MermoidAscent.svgDiagram("""stateDiagram-v2
                            |    [*] --> Idle
                            |    Idle --> Done: go
                            |    note right of Idle as caveat
                            |      do not skip idle
                            |    end note
                            |""".stripMargin)
      },
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
