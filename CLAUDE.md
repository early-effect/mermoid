# Mermoid

Mermaid-compatible diagram → SVG library in Scala 3, cross-built for the JVM and Scala.js.

## Code Style
- Prefer `@tailrec` recursive inner functions over mutable loops/`var`/`mutable.Map`
- Pure functions — minimize side effects; use immutable data structures
- Pattern matching over if/else chains where the domain is an ADT
- `foldLeft`/`foldRight` over manual accumulators
- No `null` — `Option`, `Either`, or union types
- Small, focused functions — each testable in isolation
- `private[mermoid]` visibility for functions that need test access

## Modules
| Project | Dir | Artifact | Platforms |
|---|---|---|---|
| `root` | `.` | — (aggregate, `publish / skip`) | JVM |
| `core` | `core/` | **`mermoid`** — the only published artifact | JVM + JS |
| `cli` | `cli/` | — (assembly fat jar, `publish / skip`) | JVM |
| `docs` | `docs/` | — (Specular site, `publish / skip`) | JVM |

`core` depends on **fastparse and nothing else** — that constraint is the point, not an accident.
Integrations with other libraries (specular, marklit) live in their own repos and consume `SvgNode`.
ZIO is a `cli`/test-only dependency; ascent/specular are `docs` test-only.

## Project Structure
- Small files — break code into logical modules, one concern per file
- Refactor relentlessly — pre-1.0, no users yet
- Configurable constants, not magic numbers
- Test each function immediately after writing it

## Testing
- ZIO Test for all tests
- Tests alongside each implementation step, smallest chunks possible
- Prefer metals MCP tools over sbt CLI when possible
- **Verification belongs in tests, not in ad-hoc shell commands.** If something needs checking (that
  the committed `examples/*.svg` match the renderer, that every theme's CSS round-trips, that every
  diagram on the docs site renders), write the assertion. `cli`'s `SvgOutputSpec` and `docs`'
  `DocPagesSpec` exist for exactly this; `cli/regenerateExamples` is the paired fix task.
- Specular only turns an `Example` into a zio-test test when it carries `.assert` — `DocPagesSpec`
  covers the rest by walking `BuildSite.pages` directly.

## Build
- sbt 2.0.3, Scala 3.8.4 (bare `build.sbt` settings scope to `ThisBuild`)
- fastparse 3.1.1 (parser), ZIO 2.1.26 (CLI + tests), specular 0.7.3 (docs)
- Use `sbt --client` for faster sbt command execution (reuses running sbt server)
- `test` aliases to `testQuick` in sbt 2.x and skips unchanged suites — use **`testFull`**; a full
  clean is **`cleanFull`**, not `clean`
- Tasks that write files need `Def.uncached` (otherwise sbt asks for `HashWriter` evidence)
- CI is generated from the build graph by zipx — `sbt zipxWorkflowGenerate`, and `zipxWorkflowCheck`
  fails if the committed workflow drifts
- `sbt docsPreview` serves the docs site with live reload
