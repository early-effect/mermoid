package mermoid.docs

import mermoid.ascent.MermoidAscent
import _root_.mermoid.Mermaid
import specular.*
import specular.ziotest.DocSpecSuite

/** `stateDiagram-v2`: transitions, start/end markers, notes. */
object StateDiagrams extends DocSpecSuite:

  private val orderFsm =
    Mermaid("""stateDiagram-v2
      |    [*] --> Pending
      |    Pending --> Paid: payment captured
      |    Pending --> Cancelled: customer cancels
      |    Paid --> Shipped: carrier accepts
      |    Shipped --> Delivered: scan
      |    Delivered --> [*]
      |    Cancelled --> [*]
      |""".stripMargin)

  def doc = page("State diagrams")(
    md"""
`stateDiagram-v2` opens a state diagram. `direction TB`, `TD`, `BT`, `LR`, or `RL` may appear on any statement line.
The default is top to bottom. A later `direction` replaces an earlier one. With a `Viewport`, responsive layout may
still flip a vertical author to horizontal so a wide column is used (same rules as flowcharts). Without a viewport,
the diagram keeps the direction it names.
""",
    example {
      MermoidAscent.svgDiagram(orderFsm)
    },
    section("Transitions")(
      md"""
`From --> To` declares a transition; `: label` names it. An id that has not been declared becomes a state, drawn as a
rounded box. The label is the id until a description replaces it.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    Idle --> Running: start
                            |    Running --> Idle: stop
                            |""".stripMargin))
      },
      md"""
That is a two-state cycle, and it lays out rather than looping forever — layering breaks cycles.
""",
    ),
    section("Start and end")(
      md"""
`[*]` is the start or the end, depending on which side of the arrow it sits on. The start is a filled circle. The end
is a bullseye: a ring and a filled center, class `state-end`. Both carry `start-end` and neither has a label.

When a diagram uses both, mermoid paints two markers (start keeps id `[*]`, end is `[*]-end`) so ranking does not
cycle through a shared node. Each composite and each concurrent region gets its own pair. A diagram that only has one
role still uses a single `[*]` node.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    [*] --> Active
                            |    Active --> [*]
                            |""".stripMargin))
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
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
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
                            |""".stripMargin))
      },
    ),
    section("Styling from the diagram source")(
      md"""
`classDef`, `class`, `:::`, and `style` are the same statements as on [flowcharts](flowcharts.html). `classDef` becomes
a CSS rule, `class` / `:::` put the name on the state's node, and `style` can still set `noteAlign` as well as fill and
stroke.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
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
                            |""".stripMargin))
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
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    [*] --> Ready
                            |    Ready --> Done: go
                            |    style Ready noteAlign: center
                            |    note right of Ready
                            |      centered
                            |      note text
                            |    end note
                            |""".stripMargin))
      },
    ),
    section("Note aliases")(
      md"""
Like edges, notes take `as <name>` to pin their element id. Without it a note is `note-{stateId}-{index}`, so adding an
earlier note on the same state renumbers the later ones.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    [*] --> Idle
                            |    Idle --> Done: go
                            |    note right of Idle as caveat
                            |      do not skip idle
                            |    end note
                            |""".stripMargin))
      },
    ),
    section("Self-transitions")(
      md"""
A state can transition to itself, and stacked self-transitions stack their labels. The diagram's height accounts for the
loops, and for notes pushed below them.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    [*] --> Retrying
                            |    Retrying --> Retrying: attempt failed
                            |    Retrying --> Retrying: backoff elapsed
                            |    Retrying --> Done: succeeded
                            |""".stripMargin))
      },
    ),
    section("Direction")(
      md"""
`direction LR` lays the machine out left to right. `TB` and `TD` are top to bottom, `BT` bottom to top, `RL` right to
left. The line can sit above the transitions or among them.

```
stateDiagram-v2
    direction LR
    [*] --> Draft
    Draft --> Live: publish
    Live --> [*]
```
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    direction LR
                            |    [*] --> Draft
                            |    Draft --> Preparing: Launch
                            |    Preparing --> Live: Published
                            |    Live --> [*]
                            |""".stripMargin))
      },
    ),
    section("Back edges")(
      md"""
A retry (`Faulted --> Preparing`) is a back edge. Ranking follows a depth-first search from `[*]` and reverses back
edges, so the fault sits one rank after the hop it returns to, not above the start. The edge is still drawn from the
fault back to the hop. A second forward parent, such as two states that both archive, stays a forward edge and the
sink lands on the last rank.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    [*] --> Preparing
                            |    Preparing --> Live: published
                            |    Preparing --> Faulted: failed
                            |    Faulted --> Preparing: Retry
                            |    Live --> Archived: archive
                            |    Preparing --> Archived: archive
                            |""".stripMargin))
      },
    ),
    section("Descriptions")(
      md"""
The id stays the id. The words on the box are the description.

```
state "Waiting for a worker" as Idle
Idle : no job yet
```

`id : text` and `state "text" as id` are the same fact. A second, different description is a parse error that names
both strings. `hide empty description` paints no label on a state that was never described, and no title band on a
composite that was never described.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    state "Waiting for a worker" as Idle
                            |    Running : a job is in flight
                            |    [*] --> Idle
                            |    Idle --> Running: start
                            |    Running --> Idle: stop
                            |""".stripMargin))
      },
    ),
    section("Composite states")(
      md"""
`state Name { ... }` draws a frame. The frame is a node in the parent diagram, and the statements inside are a diagram
of their own. A `direction` line inside applies only there. An edge that names the composite stops on the frame. An
edge that names a member ends on that member and crosses the frame; the member stays inside.

The same id cannot belong to two composites. The error names the id and both places. An edge between two composites,
or from a member out to a sibling of the composite, is drawn. Mermaid's renderer often pulls the child out of the box.
This one does not.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    direction LR
                            |    [*] --> Review
                            |    state Review {
                            |        [*] --> Screening
                            |        Screening --> Decision
                            |    }
                            |    Review --> Published: approved
                            |    Decision --> Draft: rejected
                            |    Published --> [*]
                            |""".stripMargin))
      },
    ),
    section("Concurrency")(
      md"""
`--` on its own line, inside a composite, splits the body into regions. Each region is ranked on its own and stacked
in the frame, with a dashed divider between them. An edge whose ends sit in two regions of the same composite is a
parse error. An edge from a state in any region out to a state outside the composite is an exit, including from a
region that is not the first.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    [*] --> Active
                            |    state Active {
                            |        [*] --> NumLockOff
                            |        NumLockOff --> NumLockOn : toggle
                            |        --
                            |        [*] --> CapsLockOff
                            |        CapsLockOff --> CapsLockOn : toggle
                            |    }
                            |    Active --> [*]
                            |""".stripMargin))
      },
    ),
    section("Choice, fork, and join")(
      md"""
`state Id <<choice>>` is a diamond. `<<fork>>` and `<<join>>` are bars. A vertical flow draws a horizontal bar, and a
horizontal flow draws a vertical bar. The bar stretches to cover the centers of the states it synchronizes. `[[choice]]`,
`[[fork]]`, and `[[join]]` are the same three forms.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    state if_state <<choice>>
                            |    [*] --> IsPositive
                            |    IsPositive --> if_state
                            |    if_state --> False: if n < 0
                            |    if_state --> True: if n >= 0
                            |    state fork_state <<fork>>
                            |    True --> fork_state
                            |    fork_state --> Left
                            |    fork_state --> Right
                            |    state join_state <<join>>
                            |    Left --> join_state
                            |    Right --> join_state
                            |    join_state --> [*]
                            |""".stripMargin))
      },
    ),
    section("History")(
      md"""
`state H <<history>>` and `state H <<deepHistory>>` are pseudostates, drawn as a circle labelled `H` or `H*`. `[H]` and
`[H*]` are the same two nodes written as a transition endpoint. The diagram shows the pseudostate. It does not remember
which substate was last active.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    [*] --> Active
                            |    state Active {
                            |        [*] --> Playing
                            |        Playing --> Paused
                            |        state back <<history>>
                            |        Paused --> back
                            |    }
                            |""".stripMargin))
      },
    ),
    section("Click and the accessible name")(
      md"""
`click` is the same binding as on a flowchart: an href, a tooltip, or a callback name. `accTitle` and `accDescr` become
the SVG `<title>` and `<desc>`, and the accessible name of the hybrid root. `classDef`, `class`, `:::`, and `style` apply inside a composite, on the frame, on a
choice, on a bar, and on a marker. `classDef default` is the paint for a state that has no other class.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""stateDiagram-v2
                            |    accTitle: review
                            |    classDef hot fill:#5c2a2a,stroke:#f0a0a0
                            |    [*] --> Draft
                            |    Draft --> Live: publish
                            |    class Live hot
                            |    click Live href "https://example.com" "Open"
                            |""".stripMargin))
      },
    ),
  )
end StateDiagrams
