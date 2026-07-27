# Future Enhancements

Items not in current scope but planned for future development. The README's "not yet implemented"
list is the public promise; this file is the plan behind it.

## Published artifacts

- **`mermoid`** (core) — parser, `DiagramLayout.scene` / `DiagramScene`, SVG painter. fastparse only.
- **`mermoid-ascent`** — hybrid HTML+SVG ascent painter with Squawk selection, Mermaid `click` tooltips/links, and
  viewport-driven re-layout (routes/splines recomputed). Usable from Specular, Scala.js apps, or any ascent host.

## Downstream Integrations

- **`specular-mermoid`** (in [early-effect/specular](https://github.com/early-effect/specular)) — thin Specular defaults
  (chalkboard, fenced `mermaid`) over `mermoid-ascent` / SVG embed. Prefer the published `mermoid-ascent` painter for
  interactive docs; keep `SvgNode → UI` only where an inert SVG tree is intentional.
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
- Executing Mermaid JS `click` callbacks (`securityLevel`); today callback **names** are stored for the host

## Browser Integration

- Auto-discovery of `<pre class="mermaid">` blocks in browser
- ResizeObserver-driven width source wired by default in `mermoid-ascent` JS builds (docs currently use Narrow/Wide
  controls + `diagramResponsive` for an external `Source[Double]`)

## Rendering Improvements

- DOM-based text measurement in Scala.js for accurate layout. Note the trade: this would break the
  byte-identical JVM/Scala.js output the README advertises, so it must be opt-in.
- Animation support via CSS transitions
- Per-node text wrapping under a viewport

## Accessibility

- ARIA labels on SVG elements (HTML hybrid nodes already use buttons + `aria-label`)
