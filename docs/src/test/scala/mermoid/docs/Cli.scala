package mermoid.docs

import specular.*
import specular.ziotest.DocSpecSuite

/** The fat-jar renderer. */
object Cli extends DocSpecSuite:

  def doc = page("CLI")(
    md"""
`mermoid-cli` renders `.mmd` files to sibling `.svg` files. It is a build tool, not a published artifact; build the jar
from the repo:

```
git clone https://github.com/early-effect/mermoid
cd mermoid
sbt cli/assembly
```

The jar lands at `cli/target/**/mermoid-cli.jar`.
""",
    section("Usage")(
      md"""
```
java -jar mermoid-cli.jar <input.mmd> [input2.mmd ...] [--gallery [out-dir]]
```

Each `.mmd` argument is read, parsed and written next to itself with `.mmd` replaced by `.svg`; the output path is
printed. With no arguments it prints usage.

```
> java -jar mermoid-cli.jar examples/order-fsm-state.mmd
Generated SVG: examples/order-fsm-state.svg
```

A parse error fails the run with the parser's message and a non-zero exit code, so it is safe in a `Makefile` or a CI
step: a malformed diagram stops the build rather than leaving a stale `.svg` in place.
"""
    ),
    section("Layout gallery")(
      md"""
`--gallery [out-dir]` writes an `index.html` that embeds every `.svg` in the directory of the first input file (or
`examples/` if you pass only `--gallery`). Default output directory is `target/layout-gallery`.

```
# render examples, then open a review page
java -jar mermoid-cli.jar examples/*.mmd --gallery
open target/layout-gallery/index.html

# custom output directory
java -jar mermoid-cli.jar examples/layout-hub.mmd --gallery /tmp/mermoid-review
```

Useful for visual review (including Playwright screenshots). The gallery embeds the SVGs that were just written (or
already present) beside the sources; it does not re-theme them.
"""
    ),
    section("Regenerating a directory")(
      md"""
There is no watch mode, theme flag, or output-directory flag for individual files. Compose with the shell instead:

```
# every diagram in the tree
find . -name '*.mmd' -print0 | xargs -0 java -jar mermoid-cli.jar

# check the committed SVGs are current (CI)
find . -name '*.mmd' -print0 | xargs -0 java -jar mermoid-cli.jar && git diff --exit-code
```

That second one works because rendering is deterministic: identical input produces identical bytes, so a clean `git diff`
means the checked-in SVGs match their sources. In this repo, prefer `sbt cli/regenerateExamples` when regenerating the
committed gallery.
"""
    ),
    section("When to use the library instead")(
      md"""
The CLI always renders with the default `RenderConfig`. For a [theme](theming.html), a
[custom stylesheet](custom-css.html), different layout geometry, or output anywhere other than a sibling file, call the
library; it is about ten lines, and the [quick start](quick-start.html) has them.

If you want the CLI's shape with your own config, its whole implementation is one `processFile` function over
`MermaidParser.parse` and `SvgRenderer.render`; copying it is less work than any flag surface would be to learn.
"""
    ),
  )
end Cli
