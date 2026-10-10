# mermoid

[![CI](https://github.com/early-effect/mermoid/actions/workflows/ci.yml/badge.svg)](https://github.com/early-effect/mermoid/actions/workflows/ci.yml)
[![Docs](https://img.shields.io/badge/docs-earlyeffect.rocks-blue)](https://www.earlyeffect.rocks/mermoid/)
[![Maven Central](https://img.shields.io/maven-central/v/rocks.earlyeffect/mermoid_3?logo=apachemaven)](https://central.sonatype.com/artifact/rocks.earlyeffect/mermoid_3)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

**[Mermaid](https://mermaid.js.org) → SVG (and optional interactive HTML) in Scala 3.** Parse flowchart,
`stateDiagram-v2`, `classDiagram`, and `erDiagram` source, lay it out, and paint a self-contained SVG on the JVM and
in the browser via Scala.js.
No headless Chrome, no Node build step, no JavaScript required at page load for static embeds.

The SVG is **styled by CSS**: stable classes and ids, plus a `<style>` block built from `--mermoid-*` custom
properties. Restyle with a stylesheet instead of re-rendering.

Two published artifacts:

| Artifact | Role | Dependencies |
|---|---|---|
| **`mermoid`** | Parser, layout (`Scene`), SVG painter | **fastparse only** |
| **`mermoid-ascent`** | Hybrid HTML nodes + SVG edges, selection, tooltips, reactive reflow | `mermoid` + [ascent](https://github.com/early-effect/ascent) + ZIO |

> **Status: early / pre-1.0.** Published under [early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme)
> (`versionScheme := "early-semver"`). The API can change between minor versions until `1.0`.

## Install

```scala
libraryDependencies += "rocks.earlyeffect" %% "mermoid" % "<version>"          // JVM
libraryDependencies += "rocks.earlyeffect" %%% "mermoid" % "<version>"         // Scala.js

// Optional: hybrid HTML + SVG for Specular / ascent apps
libraryDependencies += "rocks.earlyeffect" %% "mermoid-ascent" % "<version>"   // JVM
libraryDependencies += "rocks.earlyeffect" %%% "mermoid-ascent" % "<version>"  // Scala.js
```

The **`mermoid` SVG** uses a character-width estimate, so the JVM and Scala.js builds produce the same SVG for the same
input and config. Render server-side and hydrate that markup in the browser. `mermoid-ascent` in a browser with a
document measures text from the canvas when the caller did not pass a measure, and `diagramFitting` follows the
container width. Those two pictures are allowed to differ from the estimate.

## Quick start (SVG)

```scala
import mermoid.*

val svg: Either[ParseError, String] =
  MermaidParser.parse("""flowchart TD
      |  A[Start] --> B{OK?}
      |  B -->|yes| C((Done))
      |""".stripMargin)
    .map(SvgRenderer.render(_))
```

`parse` returns `Either[ParseError, Diagram]`. Read `ParseError.message` when you surface a failure. `render` returns a self-contained SVG string.

For the paint-agnostic tree (UI frameworks, post-processors):

```scala
val tree: Either[ParseError, SvgNode] =
  MermaidParser.parse(source).map(SvgRenderer.renderTree(_))
```

A diagram written in your code can be checked while it compiles. `Mermaid("""...""")` parses at compile time, so a
typo is a compile error at the literal and the parsed `Diagram` is built into the call site. Text that arrives at
runtime goes through `Mermaid.from`, which returns the same `Either[ParseError, Mermaid]`:

```scala
val hello: Mermaid = Mermaid("""flowchart LR
  A[Hello] --> B[World]""")

val svg: String = SvgRenderer.render(hello.diagram)

val fromFile: Either[ParseError, Mermaid] = Mermaid.from(Files.readString(path))
```

For layout without painting (metrics, custom painters, responsive hosts):

```scala
val scene: Either[ParseError, Scene] =
  MermaidParser.parse(source).map(d => DiagramLayout.scene(d, RenderConfig(), Some(Viewport(640))))
```

## Interactive / hybrid (`mermoid-ascent`)

Every entry point takes a `Mermaid`, so none of them can fail on bad source:

```scala
import mermoid.ascent.MermoidAscent
import mermoid.{Mermaid, RenderConfig, Viewport}

val source = Mermaid("""flowchart LR
  A[Start] --> B{Ready?}
  B -->|yes| C([Ship it])""")

// Static hybrid (SSR-friendly HTML nodes + SVG edges)
val ui = MermoidAscent.diagram(source, viewport = Some(Viewport(640)))

// Inert SVG mapped into ascent UI (byte-stable structure demos)
val inert = MermoidAscent.svgDiagram(source)

// Selection + Narrow/Wide reflow (recomputes routes; preserves selection id)
val interactive = MermoidAscent.diagramInteractive(source, initialWidth = 720)
```

Mermaid `click` lines become tooltips, optional `href` links, and stored callback names for the host. See the
[Interactive](https://www.earlyeffect.rocks/mermoid/interactive.html) docs page (`sbt docs/specularPreview`).

In a [Specular](https://www.earlyeffect.rocks/specular/) site, `MermoidAscent.diagram(Mermaid("..."))` goes straight into
`illustration { ... }`, with no adapter on either side. See
[Specular illustrations](https://www.earlyeffect.rocks/mermoid/specular-illustrations.html).

## Why not mermaid.js

|  | mermaid.js | mermoid |
|---|---|---|
| Runtime | JavaScript (browser or headless Chrome) | Scala 3: JVM or Scala.js |
| When it runs | page load or a Puppeteer build step | wherever you call it; output is a string / tree |
| Styling | theme object + inline attributes | real CSS: classes, ids, custom properties |
| Restyling | re-render | ship a different stylesheet |
| Output | DOM it manages | `String`, `SvgNode`, or ascent `UI` |
| Diagram coverage | complete | flowcharts, state, class, ER, and sequence diagrams (see below) |

Gantt, pie, journey, and git graphs are out of scope. If you need one of those, use mermaid.js.

## Supported syntax

### Flowcharts: `flowchart` / `graph`

Directions: `TB` `TD` `BT` `LR` `RL`.

| Feature | Syntax |
|---|---|
| 13 node shapes | `[rect]` `(round)` `([stadium])` `{rhombus}` `((circle))` `(((double)))` `{{hex}}` `[[subroutine]]` `[(cylinder)]` `[/trapezoid\]` `[\trapezoid-alt/]` `[/parallelogram/]` `[\parallelogram-alt\]` |
| Bare ids | `A --> B` (rect labelled with the id) |
| 5 edge styles | `-->` `---` `-.->` `-.-` `==>` |
| Edge labels | `A -->\|label\| B` and `A -- label --> B` |
| Subgraphs | `subgraph id [Label] … end`, nestable, optional inner `direction` |
| Styling | `style`, `classDef`, `class`, `A:::className` |
| Edge aliases | `A --> B as myEdge` pins `#edge-myEdge` |
| Clicks | `click A callback "tooltip"`, `click A href "https://…" "tip" _blank` |

### State diagrams: `stateDiagram-v2`

`direction TB`, `TD`, `BT`, `LR`, or `RL` may appear on any statement line. The default is `TB`. A later `direction`
replaces an earlier one. With a [`Viewport`](#layout-and-responsive), narrow widths keep a vertical layout and wide
widths may flip a vertical author to horizontal, the same rules as flowcharts. An explicit `direction LR` with no
viewport stays left to right.

| Feature | Syntax |
|---|---|
| Direction | `direction LR` (`TB` `TD` `BT` `LR` `RL`; default `TB`) |
| Transitions | `A --> B: label` |
| Start / end | `[*] --> A`, `A --> [*]` (separate markers when both roles appear) |
| Self-transitions | `A --> A: retry` (labels stack) |
| Descriptions | `state "long name" as id`, `id : long name` |
| Composites | `state Name { ... }`, nested, with `direction` inside |
| Concurrency | `--` splits a composite into regions |
| Choice, fork, join | `state id <<choice>>`, `<<fork>>`, `<<join>>` |
| History | `<<history>>`, `<<deepHistory>>`, `[H]`, `[H*]` |
| Notes | `note right of A` / `note left of A` … `end note`, or `note left of A: text` |
| Styling | `style`, `classDef`, `class`, `A:::className` (same as flowcharts) |
| Note alignment | `style A noteAlign:center` (`left` / `center` / `right`) |
| Note aliases | `note right of A as myNote` |
| Clicks | `click`, same binding as a flowchart. The callback name is stored. JS is not run. |
| Accessible name | `accTitle`, `accDescr` |

### Class diagrams: `classDiagram`

A class is three bands: name, attributes, operations. `()` makes a member an operation. `+` `-` `#` `~` are visibility.
`<|--` `*--` `o--` `-->` `..>` `..|>` are the UML heads. `namespace Id { ... }` is a frame. `hideEmptyMembersBox` drops
empty bands. `accTitle` and `accDescr` name the diagram.

### ER diagrams: `erDiagram`

`CUSTOMER ||--o{ ORDER : places` is exactly-one to zero-or-more. `|o` is zero or one, `}|` is one or more. `PK` and
`FK` mark attributes. `..` is the same feet on a dashed line. `accTitle` and `accDescr` name the diagram.

### Sequence diagrams: `sequenceDiagram`

Columns are participants in first-seen order. Rows are statements in source order. There is no ranker and no direction
flip. A narrow viewport compresses `SequenceConfig.columnGap` and `rowPitch`, then container fit scales the picture.

| Feature | Syntax |
|---|---|
| Headers | `participant Alice`, `actor Bob`, optional `as "Label"` |
| Requests | `->>` solid head, `->` solid shaft, `-x` cross, `-)` open |
| Replies | `-->>` dashed head, `-->` dashed shaft, `--x` cross, `--)` open |
| Both directions | `<<->>` solid, `<<-->>` dashed |
| Activation | `A->>+B` opens a bar on B, `B-->>-A` closes the bar on B |
| Numbering | `autonumber`, `autonumber off`, `autonumber 3`, `autonumber 3 2` |
| Notes | `note left of A: text`, `note right of A: text`, `note over A,B: text` |
| Fragments | `loop`, `opt`, `critical`, `break`, `alt` / `else`, `par` / `and`, `rect rgb()` / `rgba()` |
| Bands | `box <color> <title>` … `end` |
| Lifetime | `create participant Carol`, `destroy Carol` |
| Stereotypes | `@{"type": "boundary"}` and `control`, `entity`, `database`, `collections`, `queue`, `actor` |
| Links and paint | `link` / `links`, `click`, `style`, `classDef`, `class`, `accTitle`, `accDescr` |

A second declare of the same id with a different label, or a switch between `participant` and `actor`, fails with
`ParseError.ConflictingAlias`. `else` belongs to `alt`. `and` belongs to `par`.

### Special cases and limitations

Documented so adopters are not surprised:

| Case | Behaviour |
|---|---|
| Chained edges `A --> B --> C` | One hop per pair, same as writing each edge on its own line. |
| Mermaid `%%` comments | Ignored. `%%{init:…}%%` contributes only a theme name (`default`, `dark`, `forest`, `neutral`), and only when `RenderConfig.theme` is still `Default`. |
| Parallel edges `A --> B` twice | Both render; offset so they do not sit on top of each other. Use `as` if you CSS-select one. |
| Cycles / back-edges | A greedy feedback arc set reverses back edges for ranking and draws them in the original direction, so a retry sits on the next rank instead of ranking as a root. Barycenter ordering cuts crossings. Long edges route through waypoints. |
| Self-loops | Attach right (vertical flow) or top (horizontal); stacked labels get room in the bbox. |
| Nested subgraphs | A subgraph is its own ranked diagram. `direction` inside it does not change the parent. Members stay in the frame. |
| State notes vs neighbours | Notes dodge other nodes when the preferred side would overlap (especially in LR). |
| Decision diamonds (hybrid) | HTML uses the same AABB diamond polygon as SVG (`clip-path`), not a CSS-rotated square. |
| `click` callbacks | Names and tooltips are stored; **JS is not executed**. Hosts decide what `callbackName` means. |
| `click` on state diagrams | Same binding as a flowchart: href, tooltip, callback name. JS is not executed. |
| Semicolon separators | OK as statement separators (alongside newlines). |
| `end` vs `endpoint` | Bare `end` closes a subgraph; ids that start with `end` (e.g. `endpoint`) parse as ids. |
| `securityLevel` / Mermaid JS click | Out of scope. Callback names are stored. The library does not run them. |

**Out of scope** (they do not parse, and they are not a backlog):

- Diagram types: Gantt, pie, journey, git graph. They do not document a library.
- State `entry` / `exit` actions. A colon line is the description.
- `%%{init}%%` keys other than the theme name.

## Layout and responsive

```scala
import mermoid.*

val config = RenderConfig(
  layout = LayoutConfig(),                 // spacing, shape geometry, crossing sweeps
                                            // edgeLabelFontSize (default 12) is the painted edge-label size
                                            // maxLabelWidth wraps a node label instead of growing the node
  theme = css.ThemeName.Default,
  customStylesheet = None,
  resolveVariables = true,                 // false keeps var(--mermoid-*) for page cascade
  responsive = ResponsiveConfig(
    compressSpacing = true,                // shrink/expand spacing toward the viewport
    flipDirectionBelow = None,             // Some(px) opts into a direction flip
    fit = ContainerFit.ToWidth(0.5),       // scale down to the container, never below 0.5
    minSpacingScale = 0.45,
    maxSpacingScale = 1.75,
  ),
)

val scene = DiagramLayout.scene(diagram, config, Some(Viewport(720)))
val svg   = SvgRenderer.paint(scene)       // or SvgRenderer.render(diagram, config, Some(Viewport(720)))
```

`Scene` is the integration point for custom painters. `Scene.Ranked` carries a `DiagramScene`: nodes, edges, routes,
notes, interactions, and the effective direction after a responsive flip. `Scene.Sequence` carries columns, lifelines,
messages, and fragments. `Scene.fitScale` is the container-fit policy for both.

## CSS theming

```scala
import mermoid.*
import mermoid.css.ThemeName

SvgRenderer.render(diagram, RenderConfig(theme = ThemeName.Dark))
```

Themes: `Default`, `Dark`, `Forest`, `Neutral` (twenty `--mermoid-*` custom properties each).

```css
.node-Start .node-shape { fill: #ffd; }        /* one node, by id */
.node-circle .node-shape { stroke-width: 3; }  /* every circle */
.edge-dotted .edge-line { stroke: crimson; }   /* every dotted edge */
#edge-myEdge .edge-line { stroke-width: 4; }   /* one aliased edge */
```

```scala
import mermoid.css.CssParser

val custom = CssParser.parse(".node-shape { fill: papayawhip; }")
val config = RenderConfig(customStylesheet = custom.toOption)
```

## The SVG tree

```scala
enum SvgNode:
  case Element(tag: String, attrs: List[(String, String)], children: List[SvgNode])
  case Text(value: String)
  case Raw(content: String)
```

`SvgRenderer.render` is `SvgSerializer.render` over that tree. Stable ids: `node-{id}`, `edge-{alias|from-to-index}`,
`note-{alias|state-index}`, `subgraph-{id}`. Edges also carry `data-from` / `data-to`. Sequence diagrams use
`actor-{id}`, `lifeline-{id}`, `message-{index}`, `note-{index}`, `fragment-{index}`, and `activation-{index}`.

## CLI

```
sbt cli/assembly
java -jar cli/target/*/mermoid-cli.jar diagram.mmd [more.mmd ...]
java -jar cli/target/*/mermoid-cli.jar examples/*.mmd --gallery   # target/layout-gallery/index.html
```

Writes sibling `.svg` files with the default `RenderConfig`. `--gallery [out-dir]` builds an HTML review page of the
SVGs beside the first input (default out dir: `target/layout-gallery`). JVM-only, not published. For themes or custom
output paths, call the library.

## Gallery

Checked by the test suite (committed SVG must match the renderer):

| Diagram | Source | Output |
|---|---|---|
| Order lifecycle (state + notes) | [order-fsm-state.mmd](examples/order-fsm-state.mmd) | [order-fsm-state.svg](examples/order-fsm-state.svg) |
| Same lifecycle as a flowchart | [order-fsm-flowchart.mmd](examples/order-fsm-flowchart.mmd) | [order-fsm-flowchart.svg](examples/order-fsm-flowchart.svg) |
| Traced path via `style` | [order-1-trace.mmd](examples/order-1-trace.mmd) | [order-1-trace.svg](examples/order-1-trace.svg) |
| Another traced path | [order-3-trace.mmd](examples/order-3-trace.mmd) | [order-3-trace.svg](examples/order-3-trace.svg) |
| Cycle / back-edge | [layout-cycle.mmd](examples/layout-cycle.mmd) | [layout-cycle.svg](examples/layout-cycle.svg) |
| Diamond decision | [layout-diamond.mmd](examples/layout-diamond.mmd) | [layout-diamond.svg](examples/layout-diamond.svg) |
| Fan-out | [layout-fan.mmd](examples/layout-fan.mmd) | [layout-fan.svg](examples/layout-fan.svg) |
| Fan-in hub | [layout-hub.mmd](examples/layout-hub.mmd) | [layout-hub.svg](examples/layout-hub.svg) |
| Crossing stress | [layout-crossed.mmd](examples/layout-crossed.mmd) | [layout-crossed.svg](examples/layout-crossed.svg) |
| Long-span skip edge | [layout-long-span.mmd](examples/layout-long-span.mmd) | [layout-long-span.svg](examples/layout-long-span.svg) |
| Skip edges | [layout-skips.mmd](examples/layout-skips.mmd) | [layout-skips.svg](examples/layout-skips.svg) |
| Parallel edges | [layout-parallels.mmd](examples/layout-parallels.mmd) | [layout-parallels.svg](examples/layout-parallels.svg) |
| Spline routes | [layout-splines.mmd](examples/layout-splines.mmd) | [layout-splines.svg](examples/layout-splines.svg) |
| Click tooltips / href | [interactive-tooltips.mmd](examples/interactive-tooltips.mmd) | [interactive-tooltips.svg](examples/interactive-tooltips.svg) |
| State + note (interactive demo source) | [interactive-state.mmd](examples/interactive-state.mmd) | [interactive-state.svg](examples/interactive-state.svg) |
| Reflow demo source | [interactive-reflow.mmd](examples/interactive-reflow.mmd) | [interactive-reflow.svg](examples/interactive-reflow.svg) |
| Dense hub (interactive) | [interactive-hub.mmd](examples/interactive-hub.mmd) | [interactive-hub.svg](examples/interactive-hub.svg) |
| Sequence span | [sequence-span.mmd](examples/sequence-span.mmd) | [sequence-span.svg](examples/sequence-span.svg) |
| Sequence life | [sequence-life.mmd](examples/sequence-life.mmd) | [sequence-life.svg](examples/sequence-life.svg) |

## Documentation

**[earlyeffect.rocks/mermoid](https://www.earlyeffect.rocks/mermoid/):** every diagram on the site is rendered by the
real renderer while the page is built, and asserted by `sbt testFull`.

Guide path: Quick start → Flowcharts → State diagrams → Sequence diagrams → **Interactive** → Responsive layout → Theming → Custom CSS → SVG structure → CLI.

```
sbt docs/specularPreview      # live-reload docs (do not ~). Stop with docs/specularPreviewStop
sbt testFull                   # core JVM+JS, ascent, cli, docs assertions
```

## Contributing

Once per clone, enable the scalafmt pre-commit hook:

```bash
./scripts/install-git-hooks
```


Formatting is enforced (`sbt scalafmtAll`). CI workflows are generated by
[zipx](https://github.com/early-effect/zipx) (`sbt zipxWorkflowGenerate` after module changes).

Security: [SECURITY.md](SECURITY.md).

## License

[Apache-2.0](LICENSE)
