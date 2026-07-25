# Future Enhancements

Items not in current scope but planned for future development. The README's "not yet implemented"
list is the public promise; this file is the plan behind it.

## Downstream Integrations

mermoid publishes exactly one artifact with fastparse as its only dependency. Integrations with other
libraries live in **those** libraries' repos and consume `SvgNode` as the contract, so mermoid stays
dependency-light and nothing here has to track their release cadence.

- **`specular-mermoid`** (in [early-effect/specular](https://github.com/early-effect/specular)) —
  `diagram(mmd): UI[Any]`, mapping `SvgNode` to an ascent UI tree. Specular's markdown renderer drops
  raw HTML, so today a doc page cannot contain a diagram. mermoid's own docs use an unpublished,
  docs-local copy of this (`docs/src/test/scala/mermoid/docs/MermoidUi.scala`) — that file is the
  reference implementation and gets deleted once the published module exists.
- **`marklit-mermoid`** (in [early-effect/marklit](https://github.com/early-effect/marklit)) — blocked
  on a raw/verbatim output modifier: `Passthrough` re-wraps content in a fence, so a block cannot emit
  an image link or inline SVG into rendered markdown. GitHub strips inline SVG from READMEs, so
  write-a-file-and-splice-the-link is the viable shape, and `raw` is what makes it expressible.

## Additional Diagram Types

None of these parse today; the README says so explicitly.

- Sequence diagrams
- Class diagrams
- ER diagrams
- Gantt charts
- Pie charts, user journey, git graph

## Syntax Gaps in Supported Diagram Types

- Flowchart: chained edges on one line (`A --> B --> C`)
- State diagrams: composite states, concurrency (`--`), an in-diagram `direction`, `state X as "…"`

## Mermaid Compatibility

- Mermaid directive parsing (`%%{init: {'theme': 'dark'}}%%`)

## Browser Integration

- Auto-discovery of `<pre class="mermaid">` blocks in browser

## Rendering Improvements

- DOM-based text measurement in Scala.js for accurate layout. Note the trade: this would break the
  byte-identical JVM/Scala.js output the README advertises, so it must be opt-in.
- Animation support via CSS transitions

## Accessibility

- ARIA labels on SVG elements
