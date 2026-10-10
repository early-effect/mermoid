package mermoid.docs

import ascent.ast.UI
import mermoid.ascent.MermoidAscent
import mermoid.{Mermaid, MermaidParser, SvgRenderer}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** Install coordinate. Judge the raw build version. Do not trust a version after `-ci` was stripped. */
object DocsVersion:
  /** Last published tag. */
  val published = "0.2.0"

  /** A dynver distance (`-ci`, `+`, `SNAPSHOT`) is the next line. Anything else is advertised as written. */
  def advertise(raw: String): String =
    val trimmed  = raw.trim
    val distance = trimmed.contains("-ci") || trimmed.contains("+") || trimmed.contains("SNAPSHOT")
    if trimmed.nonEmpty && !distance then trimmed else published

  /** `specular.meta.version` is the raw build version. Absent in unit tests, so those use [[published]]. */
  def version: String =
    sys.props.get("specular.meta.version").map(advertise).getOrElse(published)
end DocsVersion

/** The site front. Specular still emits a summary index; [[BuildSite]] copies this page over it.
  *
  * Not a sidebar item. The diagram pages stay the reference.
  */
object Front extends DocSpecSuite:

  private val coordinate: String =
    s"""libraryDependencies += "rocks.earlyeffect" %% "mermoid" % "${DocsVersion.version}"
       |libraryDependencies += "rocks.earlyeffect" %% "mermoid-ascent" % "${DocsVersion.version}"""".stripMargin

  private val classes =
    Mermaid("""classDiagram
              |    direction TB
              |    class Customer
              |    class Order
              |    class LineItem
              |    Customer "1" --> "*" Order : places
              |    Order "1" *-- "*" LineItem : holds
              |""".stripMargin)

  private val orders =
    Mermaid("""erDiagram
              |    CUSTOMER ||--o{ ORDER : places
              |    CUSTOMER {
              |        string name PK
              |    }
              |    ORDER {
              |        int id PK
              |        string customerId FK
              |    }
              |""".stripMargin)

  private val exchange =
    Mermaid("""sequenceDiagram
              |    participant Alice
              |    participant Bob
              |    Alice->>+Bob: place order
              |    Bob-->>-Alice: accepted
              |""".stripMargin)

  private val pipeline =
    Mermaid("""flowchart TB
              |    Parse --> Layout --> SVG
              |""".stripMargin)

  private def plainText(ui: UI[Any]): String = ui match
    case UI.Text(value)        => value
    case e: UI.Element[Any]    => e.children.map(plainText).mkString
    case UI.Fragment(children) => children.map(plainText).mkString
    case _                     => ""

  private def svgWidths(ui: UI[Any]): List[Double] = ui match
    case e: UI.Element[Any] =>
      val here =
        if e.tag == "svg" then
          e.attrs.collect { case ascent.ast.Attr.StaticAttr("width", ascent.domtypes.AttrValue.Str(raw)) =>
            raw.toDoubleOption
          }.flatten
        else Nil
      here.toList ++ e.children.toList.flatMap(svgWidths)
    case UI.Fragment(children) => children.toList.flatMap(svgWidths)
    case _                     => Nil

  private def shown(ui: UI[Any], needles: String*): TestResult =
    val text   = plainText(ui)
    val widths = svgWidths(ui)
    assertTrue(needles.forall(text.contains), widths.nonEmpty, widths.forall(_ > 0))
      .label(s"widths ${widths.mkString(",")}; missing ${needles.filterNot(text.contains).mkString(",")}")

  def doc = page("Painted in Scala")(
    md"""
mermoid parses Mermaid flowchart, state, class, ER, and sequence diagrams and paints SVG in Scala, on the JVM and in the browser.
""",
    section("Class")(
      md"""
A class diagram is types and the UML relations between them. [Class diagrams](class-diagrams.html) is the reference.
""",
      example {
        MermoidAscent.svgDiagram(classes)
      }.assert { ui => shown(ui, "Customer", "Order", "LineItem", "places") },
    ),
    section("ER")(
      md"""
An ER diagram is entities and cardinality. The mark is a crow's foot. [ER diagrams](er-diagrams.html) is the reference.
""",
      example {
        MermoidAscent.svgDiagram(orders)
      }.assert { ui => shown(ui, "CUSTOMER", "ORDER", "places", "customerId") },
    ),
    section("Sequence")(
      md"""
A sequence diagram is a timeline: participants are columns, messages are rows. [Sequence diagrams](sequence-diagrams.html)
is the reference.
""",
      example {
        MermoidAscent.svgDiagram(exchange)
      }.assert { ui => shown(ui, "Alice", "Bob", "place order", "accepted") },
    ),
    section("Flowchart")(
      md"""
A flowchart ranks nodes and routes the edges. [Flowcharts](flowcharts.html) is the reference. State diagrams live on
[their own page](state-diagrams.html).
""",
      example {
        MermoidAscent.svgDiagram(pipeline)
      }.assert { ui => shown(ui, "Parse", "Layout", "SVG") },
    ),
    section("Install")(
      md"""
The coordinate is the release this site advertises.

```scala
${coordinate}
```
"""
    ),
    section("Parse and paint")(
      md"""
`MermaidParser.parse` reads the source. `SvgRenderer.render` paints the SVG string. Neither method is inline, so the
panels below are those definitions.
""",
      cite[MermaidParser.type](_.parse(_: String)),
      cite(SvgRenderer.render(_, _, _)),
    ),
  )

  override def spec =
    suite("Painted in Scala")(
      super.spec,
      test("install coordinate is the published release") {
        val version = DocsVersion.version
        assertTrue(
          coordinate.contains(version),
          coordinate.contains("rocks.earlyeffect"),
          coordinate.contains("%% \"mermoid\""),
          coordinate.contains("mermoid-ascent"),
          !version.contains("-ci"),
          !version.contains("+"),
          !version.contains("SNAPSHOT"),
          !coordinate.contains("-ci"),
          !coordinate.contains("SNAPSHOT"),
          DocsVersion.advertise("0.2.1-ci") == DocsVersion.published,
          DocsVersion.advertise("0.3.0-ci") == DocsVersion.published,
          DocsVersion.advertise("0.2.0+7-abcdef") == DocsVersion.published,
          DocsVersion.advertise("0.2.1-SNAPSHOT") == DocsVersion.published,
          DocsVersion.advertise("0.2.0") == "0.2.0",
        )
      },
    )
end Front
