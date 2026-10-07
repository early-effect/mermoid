package mermoid.docs

import mermoid.Mermaid
import mermoid.ascent.{MermoidAscent, WidthControls}
import specular.*
import zio.test.*

/** How a Specular site illustrates its pages with mermoid, with no glue module on either side.
  *
  * Shared `DocSpec` so docsJS remounts the live illustration. JVM discovery is [[SpecularIllustrationsSuite]].
  */
object SpecularIllustrations extends DocSpec:

  private val pipeline =
    Mermaid("""flowchart LR
      |  spec["DocSpec"] --> tests["sbt test"]
      |  spec --> site["static site"]
      |""".stripMargin)

  def doc = page("Specular illustrations")(
    md"""
[Specular](https://www.earlyeffect.rocks/specular/) pages take an ascent component through
`illustration { ... }`, and `mermoid-ascent` returns one. That is the whole integration: specular
does not depend on mermoid, mermoid does not depend on specular, and neither ships an adapter.

Add mermoid to the **docs** project only. Your published modules never see it:

```scala
// docs project, JVM and Scala.js
libraryDependencies += "rocks.earlyeffect" %%% "mermoid-ascent" % "<version>"
```
""",
    section("A static figure")(
      md"""
`Mermaid("...")` parses the diagram while your docs compile, so a broken figure is a compile
error, not a broken picture. The site SSRs the hybrid HTML and SVG; no JavaScript is needed to see it.
""",
      example {
        MermoidAscent.diagram(
          Mermaid("""flowchart LR
            |  spec["DocSpec"] --> tests["sbt test"]
            |  spec --> site["static site"]
            |""".stripMargin)
        )
      }.assert(ui => assertTrue(ui.toString.contains("mermoid-root"), ui.toString.contains("static site"))),
      md"""
Without the source panel, the same diagram is an `illustration`: the page region *is* the figure.
""",
      illustration(MermoidAscent.diagram(pipeline))
        .assert(ui => assertTrue(ui.toString.contains("sbt test"))),
    ),
    section("A live figure")(
      md"""
`diagramInteractive` allocates its own width source, so it is an effect. Wrap it in
`illustrationIO` and mark it `.live`; `SpecularClient.fromPages` remounts it in the browser with no
mounter of your own:

```scala
illustrationIO(MermoidAscent.diagramInteractive(pipeline, widthControls = WidthControls.Shown)).live
```
""",
      illustrationIO(MermoidAscent.diagramInteractive(pipeline, widthControls = WidthControls.Shown)).live
        // The diagram itself is a reactive child that reflows on width; the shell and its controls are static.
        .assert(ui => assertTrue(ui.toString.contains("mermoid-ascent"), ui.toString.contains("mermoid-controls"))),
    ),
    section("A broken figure does not compile")(
      md"""
The parser runs inside the `Mermaid` literal, so the error points at the diagram source:
""",
      expectFail("""mermoid.Mermaid("flowchart LR\n  a -->")""")
        .assert(errors => assertTrue(errors.exists(_.message.contains("Not a Mermaid diagram")))),
      md"""
Diagram text you read at runtime goes through `Mermaid.from`, which returns a typed `ParseError`.
""",
    ),
  )
end SpecularIllustrations
