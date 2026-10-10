# Future Enhancements

Items not in current scope but planned for future development. The README's "not yet implemented"
list is the public promise; this file is the plan behind it.

## Published artifacts

- **`mermoid`** (core) — parser, `DiagramLayout.scene` / `Scene`, SVG painter. fastparse only.
- **`mermoid-ascent`** — hybrid HTML+SVG ascent painter with Squawk selection, Mermaid `click` tooltips/links, and
  viewport-driven re-layout (routes/splines recomputed). Usable from Specular, Scala.js apps, or any ascent host.

## Downstream Integrations

- Specular does not ship a mermoid module, and mermoid ships no specular module. A Specular page passes
  `MermoidAscent.diagram(Mermaid("..."))` to `illustration`; the docs page "Specular illustrations" is the worked
  example. An inert SVG tree (`SvgNode`) is the other option when that is intentional.
- **`marklit-mermoid`** (in [early-effect/marklit](https://github.com/early-effect/marklit)) — blocked
  on a raw/verbatim output modifier: `Passthrough` re-wraps content in a fence, so a block cannot emit
  an image link or inline SVG into rendered markdown. GitHub strips inline SVG from READMEs, so
  write-a-file-and-splice-the-link is the viable shape, and `raw` is what makes it expressible.

## Additional Diagram Types

None of these parse today; the README says so explicitly.

- Gantt charts
- Pie charts, user journey, git graph

## Syntax Gaps in Supported Diagram Types

- State `entry` / `exit` actions. A colon line is the description, not an action.

## Mermaid Compatibility

- `%%{init}%%` keys other than the theme name. `theme` is read when the caller left `RenderConfig.theme` at `Default`.
- Executing Mermaid JS `click` callbacks (`securityLevel`); today callback **names** are stored for the host

## Browser Integration

- Auto-discovery of `<pre class="mermaid">` blocks in browser
- ResizeObserver-driven width source wired by default in `mermoid-ascent` JS builds (docs currently use Narrow/Wide
  controls + `diagramResponsive` for an external `Source[Double]`)

## Rendering Improvements

- DOM text measurement stays opt-in (`DomTextMeasure` in `mermoid-ascent` on Scala.js). The default remains the
  character-width estimate so JVM and Scala.js SVGs stay byte-identical.
- Animation support via CSS transitions
- Per-node text wrapping under a viewport

## Accessibility

- ARIA labels on SVG elements (HTML hybrid nodes already use buttons + `aria-label`)
