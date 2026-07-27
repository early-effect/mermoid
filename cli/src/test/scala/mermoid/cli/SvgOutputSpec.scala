package mermoid.cli

import mermoid.*
import org.w3c.dom.{Element, Node}
import zio.test.*

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Path}
import javax.xml.parsers.DocumentBuilderFactory
import scala.annotation.tailrec

/** Validates rendered SVG against the format's own rules rather than against a recorded snapshot.
  *
  * A golden-file comparison only tells you the output changed; it cannot tell you whether the output is *correct*.
  * These assertions are properties any correct SVG must satisfy — well-formed XML, a viewBox that matches the declared
  * size, finite coordinates, every declared node present, every drawn shape inside the canvas — so they catch real
  * regressions while staying indifferent to cosmetic churn like indentation.
  */
object SvgOutputSpec extends ZIOSpecDefault:

  // ---- an immutable view of a parsed document ---------------------------------

  /** A DOM element snapshotted into immutable data.
    *
    * The JDK's `Document` expands nodes lazily and is not thread-safe, so sharing one across ZIO Test's parallel tests
    * hands back nulls. Reading the whole tree once, on one thread, removes the hazard entirely.
    */
  private case class Elem(
      tag: String,
      namespace: Option[String],
      attrs: List[(String, String)],
      text: String,
      children: List[Elem],
  ):
    def attr(name: String): Option[String] = attrs.collectFirst { case (n, v) if n == name => v }
    def num(name: String): Option[Double]  = attr(name).flatMap(_.toDoubleOption)
    def classes: Set[String]               = attr("class").toList.flatMap(_.split(" ")).toSet
    lazy val descendants: List[Elem]       = this :: children.flatMap(_.descendants)
    def byTag(t: String): List[Elem]       = descendants.filter(_.tag == t)
    def withClass(c: String): List[Elem]   = descendants.filter(_.classes.contains(c))
  end Elem

  private object Elem:
    def from(e: Element): Elem =
      val kids = (0 until e.getChildNodes.getLength).toList
        .map(e.getChildNodes.item)
        .collect { case c: Element => from(c) }
      val attributes = (0 until e.getAttributes.getLength).toList
        .map(e.getAttributes.item)
        .map(a => a.getNodeName -> a.getNodeValue)
      Elem(e.getLocalName, Option(e.getNamespaceURI), attributes, e.getTextContent, kids)

  private def parse(svg: String): Elem =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.setNamespaceAware(true)
    Elem.from(factory.newDocumentBuilder().parse(new ByteArrayInputStream(svg.getBytes("UTF-8"))).getDocumentElement)

  // ---- locating the examples --------------------------------------------------

  /** Tests run with an unspecified working directory, so walk up to the directory holding `examples/`. */
  private def repoRoot: Path =
    @tailrec def up(p: Path): Path =
      if Files.isDirectory(p.resolve("examples")) then p
      else
        Option(p.getParent) match
          case Some(parent) => up(parent)
          case None         => throw new IllegalStateException("could not locate the examples/ directory")
    up(Path.of(sys.props.getOrElse("user.dir", ".")).toAbsolutePath)

  private def examples: List[(String, String)] =
    import scala.jdk.CollectionConverters.*
    val stream = Files.list(repoRoot.resolve("examples"))
    try
      stream.iterator.asScala.toList
        .filter(_.getFileName.toString.endsWith(".mmd"))
        .sortBy(_.getFileName.toString)
        .map(p => p.getFileName.toString -> Files.readString(p))
    finally stream.close()

  // ---- the rendered corpus ----------------------------------------------------

  private case class Rendered(name: String, diagram: Diagram, svg: String, root: Elem):
    /** `viewBox="minX minY width height"`. */
    val viewBox: (Double, Double, Double, Double) =
      root.attr("viewBox").map(_.trim.split("\\s+").flatMap(_.toDoubleOption)) match
        case Some(Array(a, b, c, d)) => (a, b, c, d)
        case other                   => throw new AssertionError(s"$name: malformed viewBox ${other.mkString}")

  private val rendered: List[Rendered] = examples.map { (name, source) =>
    val diagram = MermaidParser.parse(source).fold(err => throw new AssertionError(s"$name: $err"), identity)
    val svg     = SvgRenderer.render(diagram)
    Rendered(name, diagram, svg, parse(svg))
  }

  /** Node ids the diagram declares, as the renderer keys them. */
  private def declaredNodeIds(diagram: Diagram): Set[String] = diagram match
    case Diagram.Flowchart(_, stmts) => StyleResolver.collectNodes(stmts).keySet
    case Diagram.StateDiagram(stmts) =>
      val ends     = stmts.collect { case StateStatement.TransitionSt(t) => List(t.from, t.to) }.flatten
      val hasStart = ends.contains("[*]") && stmts.exists {
        case StateStatement.TransitionSt(t) => t.from == "[*]"
        case _                              => false
      }
      val hasEnd = stmts.exists {
        case StateStatement.TransitionSt(t) => t.to == "[*]"
        case _                              => false
      }
      val base = ends.toSet
      if hasStart && hasEnd then (base - "[*]") + "[*]" + "[*]-end"
      else base

  /** Attribute values that are meant to be numbers — the geometry we can check numerically. */
  private val numericAttrs =
    Set("x", "y", "x1", "y1", "x2", "y2", "cx", "cy", "r", "rx", "ry", "width", "height")

  private def numericValues(r: Rendered): List[(String, String)] =
    for
      e             <- r.root.descendants
      (name, value) <- e.attrs
      if numericAttrs.contains(name)
    yield s"${e.tag}/$name" -> value

  /** One test per example, so a failure names the file that broke. */
  private def forEachExample(label: String)(check: Rendered => TestResult) =
    suite(label)(rendered.map(r => test(r.name)(check(r)))*)

  def spec = suite("rendered SVG")(
    test("the examples directory is non-empty") {
      assertTrue(rendered.nonEmpty)
    },
    suite("the corpus itself")(
      test("no two examples have identical source") {
        // Duplicates make the suite look broader than it is: N files, fewer than N distinct cases.
        val dupes = examples
          .groupBy((_, src) => src.trim)
          .filter((_, fs) => fs.size > 1)
          .map((_, fs) => fs.map((name, _) => name).mkString(" == "))
        assertTrue(dupes.isEmpty)
      },
      test("the committed .svg beside each .mmd is current") {
        // The CLI writes a sibling .svg, and rendering is deterministic — so a stale committed file
        // means someone changed the renderer without regenerating, and the gallery lies.
        val stale = examples.flatMap { (name, source) =>
          val svgPath  = repoRoot.resolve("examples").resolve(name.replaceAll("\\.mmd$", ".svg"))
          val expected = MermaidParser.parse(source).map(SvgRenderer.render(_))
          if !Files.exists(svgPath) then Some(s"$name: no committed .svg")
          else if expected != Right(Files.readString(svgPath)) then Some(s"$name: committed .svg is out of date")
          else None
        }
        assertTrue(stale.isEmpty)
      },
      test("the corpus covers both diagram types") {
        val kinds = rendered
          .map(_.diagram)
          .map {
            case _: Diagram.Flowchart    => "flowchart"
            case _: Diagram.StateDiagram => "stateDiagram-v2"
          }
          .toSet
        assertTrue(kinds == Set("flowchart", "stateDiagram-v2"))
      },
    ),
    forEachExample("is well-formed XML with a conforming root") { r =>
      val (minX, minY, w, h) = r.viewBox
      assertTrue(
        r.root.tag == "svg",
        r.root.namespace.contains("http://www.w3.org/2000/svg"),
        minX == 0.0,
        minY == 0.0,
        r.root.num("width").contains(w),
        r.root.num("height").contains(h),
        w > 0,
        h > 0,
      )
    },
    forEachExample("every geometric attribute is a finite number") { r =>
      val bad = numericValues(r).filter((_, v) => v.toDoubleOption.forall(d => d.isNaN || d.isInfinite))
      assertTrue(bad.isEmpty)
    },
    forEachExample("no NaN or Infinity leaks into the document") { r =>
      assertTrue(!r.svg.contains("NaN"), !r.svg.contains("Infinity"))
    },
    forEachExample("whole-number coordinates render without a trailing .0") { r =>
      // Num.format's contract — and the reason JVM and Scala.js agree byte for byte.
      assertTrue(numericValues(r).filter((_, v) => v.endsWith(".0")).isEmpty)
    },
    forEachExample("every declared node is rendered exactly once") { r =>
      val expected = declaredNodeIds(r.diagram).map(id => s"node-$id")
      val actual   = r.root.withClass("node").flatMap(_.attr("id"))
      assertTrue(actual.toSet == expected, actual.distinct.size == actual.size)
    },
    forEachExample("every rect and circle lies inside the canvas") { r =>
      val (_, _, w, h)                 = r.viewBox
      def inside(x: Double, y: Double) = x >= 0 && y >= 0 && x <= w && y <= h
      val rects                        = r.root.byTag("rect").map { e =>
        (for
          x  <- e.num("x")
          y  <- e.num("y")
          ew <- e.num("width")
          eh <- e.num("height")
        yield inside(x, y) && inside(x + ew, y + eh)).getOrElse(false)
      }
      val circles = r.root.byTag("circle").map { e =>
        (for
          cx <- e.num("cx")
          cy <- e.num("cy")
          rr <- e.num("r")
        yield inside(cx - rr, cy - rr) && inside(cx + rr, cy + rr)).getOrElse(false)
      }
      assertTrue((rects ++ circles).forall(identity))
    },
    forEachExample("carries exactly one stylesheet and one arrowhead marker") { r =>
      val styles  = r.root.byTag("style")
      val markers = r.root.byTag("marker")
      assertTrue(
        styles.size == 1,
        styles.head.text.contains("--mermoid"),
        markers.flatMap(_.attr("id")) == List("arrowhead"),
      )
    },
    forEachExample("every element id is unique") { r =>
      val ids = r.root.descendants.flatMap(_.attr("id"))
      assertTrue(ids.distinct.size == ids.size)
    },
    forEachExample("every edge endpoint refers to a rendered node") { r =>
      val nodeIds   = r.root.withClass("node").flatMap(_.attr("id")).map(_.stripPrefix("node-")).toSet
      val endpoints = r.root.withClass("edge").flatMap(e => e.attr("data-from").toList ++ e.attr("data-to").toList)
      assertTrue(endpoints.forall(nodeIds.contains))
    },
    forEachExample("re-rendering is deterministic") { r =>
      assertTrue(SvgRenderer.render(r.diagram) == r.svg)
    },
  )
end SvgOutputSpec
