# mermoid

[![CI](https://github.com/early-effect/mermoid/actions/workflows/ci.yml/badge.svg)](https://github.com/early-effect/mermoid/actions/workflows/ci.yml)
[![Docs](https://img.shields.io/badge/docs-earlyeffect.rocks-blue)](https://www.earlyeffect.rocks/mermoid/)
[![Maven Central](https://img.shields.io/maven-central/v/rocks.earlyeffect/mermoid_3?logo=apachemaven)](https://central.sonatype.com/artifact/rocks.earlyeffect/mermoid_3)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

**[Mermaid](https://mermaid.js.org) diagrams → SVG, in Scala 3.** mermoid parses Mermaid syntax and
renders a complete SVG document — on the JVM and in the browser via Scala.js. No headless Chrome, no
Node build step, no JavaScript at page load.

The output is **styled entirely by CSS**. Every element carries a stable class and id, and the
stylesheet ships in a `<style>` block built from CSS custom properties — so you restyle a diagram
with a stylesheet instead of re-rendering it.

fastparse is the only dependency.

> **Status: early / pre-1.0.** Published under [early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme)
> (`versionScheme := "early-semver"`) — the API can change between minor versions until `1.0`.

```scala
import mermoid.*

val svg: Either[String, String] =
  MermaidParser.parse("flowchart TD\n  A[Start] --> B{OK?}\n  B -->|yes| C((Done))")
    .map(SvgRenderer.render(_))
```

That's the whole API surface for the common case: `parse` returns `Either[String, Diagram]`,
`render` returns a self-contained SVG string you can write to a file or drop into a page.

## Install

```scala
libraryDependencies += "rocks.earlyeffect" %% "mermoid" % "<version>"   // JVM
libraryDependencies += "rocks.earlyeffect" %%% "mermoid" % "<version>"  // Scala.js
```

Layout uses a font-metric estimate rather than DOM measurement, so **the JVM and Scala.js builds
produce byte-identical output** for the same input. You can render server-side and hydrate the same
markup in the browser.

## Why not mermaid.js

|  | mermaid.js | mermoid |
|---|---|---|
| Runtime | JavaScript, in the browser or headless Chrome | Scala 3 — JVM or Scala.js |
| When it runs | page load (or a Puppeteer step in your build) | wherever you call it; output is a plain string |
| Styling | inline attributes + a theme object | real CSS: classes, ids, custom properties |
| Restyling | re-render | ship a different stylesheet |
| Output | DOM it manages | `String`, or an `SvgNode` tree you own |
| Diagram coverage | complete | flowcharts and state diagrams (see below) |

The trade is honest: mermaid.js supports every diagram type and mermoid does not. If you need
sequence, class, ER, or Gantt diagrams today, use mermaid.js.

## Supported syntax

**Flowcharts** — `flowchart` / `graph`, directions `TB` `TD` `BT` `LR` `RL`

| Feature | Syntax |
|---|---|
| 13 node shapes | `[rect]` `(round)` `([stadium])` `{rhombus}` `((circle))` `(((double)))` `{{hex}}` `[[subroutine]]` `[(cylinder)]` `[/trapezoid\]` `[\trapezoid-alt/]` `[/parallelogram/]` `[\parallelogram-alt\]` |
| 5 edge styles | `-->` `---` `-.->` `-.-` `==>` |
| Edge labels | `A -->\|label\| B` and `A -- label --> B` |
| Subgraphs | `subgraph id [Label] … end`, nestable, with an inner `direction` |
| Styling | `style`, `classDef`, `class` |
| Stable ids | `A --> B as myEdge` names the edge's SVG id |

**State diagrams** — `stateDiagram-v2`

| Feature | Syntax |
|---|---|
| Transitions | `A --> B: label` |
| Start / end | `[*] --> A`, `A --> [*]` |
| Self-transitions | `A --> A: retry` |
| Notes | `note right of A … end note`, `note left of A … end note` |
| Note alignment | `style A noteAlign:center` |
| Stable ids | `note right of A as myNote` |

**Not yet implemented.** These parse-fail or are ignored, deliberately rather than silently:

- Diagram types: sequence, class, ER, Gantt, pie, journey, git graph
- Directives: `%%{init: …}%%`
- Flowchart: chained edges on one line (`A --> B --> C`) — write them as separate statements
- State diagrams: composite states, concurrency (`--`), an in-diagram `direction`, `state X as "…"`

## CSS theming

Four built-in themes, selected on the config:

```scala
import mermoid.*
import mermoid.css.ThemeName

SvgRenderer.render(diagram, RenderConfig(theme = ThemeName.Dark))
```

`Default`, `Dark`, `Forest`, `Neutral` — each a palette of 20 `--mermoid-*` custom properties.

Every element is addressable, so a stylesheet can reach any part of a diagram:

```css
.node-Start .node-shape { fill: #ffd; }        /* one node, by id     */
.node-circle .node-shape { stroke-width: 3; }  /* every circle        */
.edge-dotted .edge-line { stroke: crimson; }   /* every dotted edge   */
#edge-myEdge .edge-line { stroke-width: 4; }   /* one aliased edge    */
```

Bring your own CSS as a parsed `Stylesheet`, merged over the theme:

```scala
import mermoid.css.CssParser

val custom = CssParser.parse(".node-shape { fill: papayawhip; }")
val config = RenderConfig(customStylesheet = custom.toOption)
```

`resolveVariables = false` keeps the `var(--mermoid-*)` references in the output, so a page-level
stylesheet can override the palette after the fact. The default (`true`) inlines them, which makes
the SVG portable as a standalone file.

## The SVG tree

`SvgRenderer.renderTree(diagram, config): SvgNode` returns the document as data instead of a string:

```scala
enum SvgNode:
  case Element(tag: String, attrs: List[(String, String)], children: List[SvgNode])
  case Text(value: String)
  case Raw(content: String)
```

This is the integration contract. A UI framework maps `SvgNode` to its own element type; a
post-processor rewrites attributes; a different serializer emits something other than XML — all
without re-parsing mermoid's output. `SvgRenderer.render` is `SvgSerializer.render` over that tree.

## CLI

```
sbt cli/assembly
java -jar cli/target/*/mermoid-cli.jar diagram.mmd [more.mmd ...]
```

Each input writes a sibling `.svg`. The CLI is JVM-only and is not published — it's a convenience for
batch-rendering a directory of diagrams; in an application, call the library.

## Gallery

Checked by this repo's own test suite, which fails if a committed SVG drifts from the renderer:

| Diagram | Source | Output |
|---|---|---|
| Order lifecycle, as a state diagram with notes | [order-fsm-state.mmd](examples/order-fsm-state.mmd) | [order-fsm-state.svg](examples/order-fsm-state.svg) |
| The same lifecycle, as a flowchart | [order-fsm-flowchart.mmd](examples/order-fsm-flowchart.mmd) | [order-fsm-flowchart.svg](examples/order-fsm-flowchart.svg) |
| A traced execution path, highlighted with `style` | [order-1-trace.mmd](examples/order-1-trace.mmd) | [order-1-trace.svg](examples/order-1-trace.svg) |
| A different path through the same machine | [order-3-trace.mmd](examples/order-3-trace.mmd) | [order-3-trace.svg](examples/order-3-trace.svg) |

## Documentation

**[earlyeffect.rocks/mermoid](https://www.earlyeffect.rocks/mermoid/)** — every diagram on the docs
site is rendered by the real renderer while the page is built, and asserted by `sbt test`. A diagram
that stops parsing is a red CI check, not a broken picture.

## Contributing

```
sbt test         # core (JVM + JS), cli, and the docs site's assertions
sbt docsPreview  # docs site with live reload
```

Formatting is enforced (`sbt scalafmtAll`), and the committed CI workflow is generated from the build
graph by [zipx](https://github.com/early-effect/zipx) — run `sbt zipxWorkflowGenerate` after changing
modules, or CI will tell you it drifted.

Planned work is in [FUTURE.md](FUTURE.md). Security reports: [SECURITY.md](SECURITY.md).

## License

[Apache-2.0](LICENSE)
