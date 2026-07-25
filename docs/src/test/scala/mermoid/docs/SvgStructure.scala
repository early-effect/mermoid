package mermoid.docs

import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** The class/id contract, and the `SvgNode` tree behind it. */
object SvgStructure extends DocSpecSuite:

  private val sample =
    """flowchart LR
      |    A((Start)) -->|go| B{Check}
      |    B ==> C[[Work]] as main
      |""".stripMargin

  def doc = page("SVG structure")(
    md"""
The shape of the output is public API. Class names and ids are what make a rendered diagram styleable and testable, so
they are stable and change only with a version bump.
""",
    section("The document")(
      md"""
```
<svg xmlns="…" width="…" height="…" viewBox="0 0 width height">
  <defs><marker id="arrowhead">…<polygon class="arrowhead"/></marker></defs>
  <style>  … the resolved stylesheet …  </style>
  <g class="subgraph"  id="subgraph-{id}">…</g>     <!-- behind everything -->
  <g class="edge …"    id="edge-{…}">…</g>
  <g class="node …"    id="node-{id}">…</g>
  <g class="note"      id="note-{…}">…</g>          <!-- state diagrams -->
</svg>
```

Paint order is subgraph frames, then edges, then nodes, then notes — so a node covers the edges arriving at it, not the
other way round. The `viewBox` always starts at `0 0` and matches `width`/`height`, so the SVG scales cleanly.
""",
      example {
        MermoidUi.diagram(sample)
      },
    ),
    section("Wrapper groups")(
      md"""
| Element | Class | Id |
|---|---|---|
| node | `node node-{shape}` + any `class` names | `node-{nodeId}` |
| edge | `edge edge-{style}` (+ ` self-loop`) | `edge-{alias}` or `edge-{from}-{to}-{index}` |
| note | `note` | `note-{alias}` or `note-{stateId}-{index}` |
| subgraph | `subgraph` | `subgraph-{id}` |

Edges also carry `data-from` and `data-to` with the endpoint node ids, which is how you find every edge touching a node
without parsing its id: `[data-from="A"], [data-to="A"]`.

`{index}` is per group — per `(from, to)` pair for edges, per state for notes — and it is positional, so it moves when you
insert a sibling. Use `as <name>` to pin an id you intend to select. Node and subgraph ids come straight from the diagram
source and never shift.
""",
      exampleValue {
        import mermoid.*
        MermaidParser
          .parse(sample)
          .map(SvgRenderer.render(_))
          .map { svg =>
            List(
              """id="node-A"""",
              """class="node node-circle"""",
              """id="edge-A-B-0"""",
              """class="edge edge-arrow"""",
              """id="edge-main"""",
              """class="edge edge-thick"""",
              """data-from="A"""",
            ).filterNot(svg.contains)
          }
      }.assert(missing => assertTrue(missing == Right(Nil))),
    ),
    section("Inner parts")(
      md"""
Inside a wrapper, the pieces carry their own classes — these are what you actually style, since the wrapper is a `<g>`
with no paint of its own:

| Class | Element | What it is |
|---|---|---|
| `node-shape` | `rect`/`circle`/`polygon`/`path` | the node outline |
| `node-label` | `text` | the node's text |
| `edge-line` | `path` | the edge itself |
| `edge-label` | `text` | the edge's label |
| `edge-label-bg` | `rect` | the plate behind an edge label |
| `note-rect` | `rect` | a note's box |
| `note-text` | `text` | a note's text |
| `note-connector` | `path` | the dashed line to its state |
| `subgraph-rect` | `rect` | a subgraph frame |
| `subgraph-label` | `text` | a subgraph's title |
| `arrowhead` | `polygon` | the shared marker |
| `start-end` | on a node wrapper | `[*]` in a state diagram |

So `.node-circle .node-shape { fill: … }` fills only circles, and `.edge-dotted .edge-line { stroke-dasharray: … }` is
exactly how the built-in themes implement dashing — the dash pattern is CSS, not geometry.
"""
    ),
    section("The SvgNode tree")(
      md"""
`SvgRenderer.renderTree` returns the tree instead of the string. Three cases:

```scala
enum SvgNode:
  case Element(tag: String, attrs: List[(String, String)], children: List[SvgNode])
  case Text(value: String)
  case Raw(content: String)   // the <style> body — CSS, never XML-escaped
```

`attrs` order is preserved. Escaping happens in `SvgSerializer`, not in the tree, so the values you read are the real
ones — no unescaping needed, and no risk of double-escaping when you re-serialize with your own writer.

`Raw` exists because the `<style>` body is CSS: escaping `&`/`<`/`>` there would produce broken CSS, since a browser
does not decode entities inside a raw-text element. If you map `Raw` into a framework that escapes text, check that the
CSS contains none of those characters — mermoid's generated CSS never does, and this site's bridge asserts it.
""",
      exampleValue {
        import mermoid.*
        def summarize(node: SvgNode, depth: Int = 0): List[String] = node match
          case SvgNode.Element(tag, attrs, children) =>
            val cls = attrs.collectFirst { case ("class", v) => s" class=$v" }.getOrElse("")
            s"${"  " * depth}<$tag$cls>" :: children.flatMap(summarize(_, depth + 1))
          case SvgNode.Text(v) => List(s"""${"  " * depth}"$v"""")
          case SvgNode.Raw(_)  => List(s"${"  " * depth}<raw css>")

        MermaidParser
          .parse("flowchart LR\n  A((Go)) --> B[Stop]")
          .map(d => summarize(SvgRenderer.renderTree(d)).mkString("\n"))
          .getOrElse("unparseable")
      }.assert(s =>
        assertTrue(
          s.startsWith("<svg>"),
          s.contains("<g class=node node-circle>"),
          s.contains("<raw css>"),
        )
      ),
      md"""
This is the integration point. A UI framework maps `Element`/`Text` to its own node type and gets a real element tree —
no string parsing, no `innerHTML`. The diagrams on this site are built that way: mermoid's tree is mapped to
[ascent](https://github.com/early-effect/ascent) elements and server-rendered by that library's own engine, and the
result is asserted to carry the same classes and ids `SvgSerializer` produces.
""",
    ),
    section("Guarantees")(
      md"""
- **Deterministic** — same source and config, byte-identical output. No timestamps, no ids from a counter that depends on
  iteration order, no `Math.random`.
- **Platform-independent** — the JVM and Scala.js produce the same bytes. Every number goes through one formatter,
  because `Double.toString` disagrees between the two platforms (`14.0` versus `14`).
- **Well-formed XML** — attribute values and text are escaped; the output parses with a strict XML parser.
- **Whole numbers stay whole** — `width="80"`, not `width="80.0"`.

These are enforced by tests over the real example files, not just asserted here.
"""
    ),
  )
end SvgStructure
