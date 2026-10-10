package mermoid.cli

import mermoid.*
import org.w3c.dom.{Element, Node}
import zio.*
import zio.test.*

import java.io.{ByteArrayInputStream, IOException}
import java.nio.file.{Files, Path}
import javax.xml.parsers.DocumentBuilderFactory
import scala.jdk.CollectionConverters.*

/** Validates rendered SVG against the format's own rules rather than against a recorded snapshot.
  *
  * A golden-file comparison only tells you the output changed; it cannot tell you whether the output is *correct*.
  * These assertions are properties any correct SVG must satisfy — well-formed XML, a viewBox that matches the declared
  * size, finite coordinates, every declared node present, every drawn shape inside the canvas — so they catch real
  * regressions while staying indifferent to cosmetic churn like indentation.
  */
object SvgOutputSpec extends ZIOSpec[SvgOutputSpec.Corpus]:

  // ---- an immutable view of a parsed document ---------------------------------

  /** A DOM element snapshotted into immutable data.
    *
    * The JDK's `Document` expands nodes lazily and is not thread-safe, so sharing one across ZIO Test's parallel tests
    * hands back nulls. Reading the whole tree once, on one thread, removes the hazard entirely.
    */
  final case class Elem(
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

  object Elem:
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

  // ---- loading the corpus ----------------------------------------------------

  /** Why the examples could not be loaded and rendered. */
  enum CorpusError:
    case NoExamplesDirectory(start: Path)
    case Unreadable(path: Path, cause: IOException)
    case Unparseable(name: String, error: ParseError)
    case NotXml(name: String, cause: Throwable)
    case MalformedViewBox(name: String, raw: Option[String])

  final case class Example(name: String, source: String)

  final case class Rendered(
      name: String,
      diagram: Diagram,
      svg: String,
      root: Elem,
      /** `viewBox="minX minY width height"`. */
      viewBox: (Double, Double, Double, Double),
  )

  final case class Corpus(root: Path, examples: List[Example], rendered: List[Rendered])

  /** Tests run with an unspecified working directory, so walk up to the directory holding `examples/`. */
  private def repoRoot: IO[CorpusError, Path] =
    def up(p: Path): UIO[Option[Path]] =
      ZIO.succeedBlocking(Files.isDirectory(p.resolve("examples"))).flatMap {
        case true  => ZIO.some(p)
        case false => Option(p.getParent).fold(ZIO.none)(up)
      }
    for
      dir <- System.propertyOrElse("user.dir", ".").orDie
      from = Path.of(dir).toAbsolutePath
      root <- up(from).someOrFail(CorpusError.NoExamplesDirectory(from))
    yield root
  end repoRoot

  private def read(path: Path): IO[CorpusError, String] =
    ZIO.attemptBlockingIO(Files.readString(path)).mapError(CorpusError.Unreadable(path, _))

  private def examplesIn(root: Path): IO[CorpusError, List[Example]] =
    val dir = root.resolve("examples")
    for
      paths <- ZIO
        .scoped(
          ZIO
            .fromAutoCloseable(ZIO.attemptBlockingIO(Files.list(dir)))
            .flatMap(listing => ZIO.attemptBlockingIO(listing.iterator.asScala.toList))
        )
        .mapError(CorpusError.Unreadable(dir, _))
      mmd = paths.filter(_.getFileName.toString.endsWith(".mmd")).sortBy(_.getFileName.toString)
      examples <- ZIO.foreach(mmd)(p => read(p).map(Example(p.getFileName.toString, _)))
    yield examples
    end for
  end examplesIn

  private def render(example: Example): IO[CorpusError, Rendered] =
    for
      mermaid <- ZIO.fromEither(Mermaid.from(example.source)).mapError(CorpusError.Unparseable(example.name, _))
      svg = SvgRenderer.render(mermaid.diagram)
      root <- ZIO.attempt(parse(svg)).mapError(CorpusError.NotXml(example.name, _))
      raw = root.attr("viewBox")
      viewBox <- raw.map(_.trim.split("\\s+").toList.flatMap(_.toDoubleOption)) match
        case Some(List(a, b, c, d)) => ZIO.succeed((a, b, c, d))
        case _                      => ZIO.fail(CorpusError.MalformedViewBox(example.name, raw))
    yield Rendered(example.name, mermaid.diagram, svg, root, viewBox)

  private val corpus: IO[CorpusError, Corpus] =
    for
      root     <- repoRoot
      examples <- examplesIn(root)
      rendered <- ZIO.foreach(examples)(render)
    yield Corpus(root, examples, rendered)

  override val bootstrap: ZLayer[Any, CorpusError, Corpus] = ZLayer(corpus)

  /** Node ids the diagram declares, as the renderer keys them. */
  private def declaredNodeIds(diagram: Diagram): Set[String] = diagram match
    case Diagram.Flowchart(_, stmts) => StyleResolver.collectNodes(stmts).keySet.map(_.value)
    case state: Diagram.StateDiagram => StateModel.resolve(state).fold(_ => Set.empty, atomIds)
    case Diagram.Sequence(_)         => Set.empty

  /** Painted state nodes. A composite is a frame, not a node, so its id is not in this set. */
  private def atomIds(machine: StateMachine): Set[String] =
    machine.regions.flatMap(atomIds).toSet

  private def atomIds(region: StateRegion): Set[String] =
    region.nodes.flatMap {
      case StateNode.Atom(id, _, _, _, _, _)            => Set(id.value)
      case StateNode.Composite(_, _, _, inner, _, _, _) => inner.flatMap(atomIds)
    }.toSet

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
    suite(label)(ZIO.serviceWith[Corpus](_.rendered.map(r => test(r.name)(check(r)))))

  def spec = suite("rendered SVG")(
    test("the examples directory is non-empty") {
      for corpus <- ZIO.service[Corpus]
      yield assertTrue(corpus.rendered.nonEmpty)
    },
    suite("the corpus itself")(
      test("no two examples have identical source") {
        // Duplicates make the suite look broader than it is: N files, fewer than N distinct cases.
        for corpus <- ZIO.service[Corpus]
        yield
          val dupes = corpus.examples
            .groupBy(_.source.trim)
            .filter((_, fs) => fs.size > 1)
            .map((_, fs) => fs.map(_.name).mkString(" == "))
          assertTrue(dupes.isEmpty)
      },
      test("the committed .svg beside each .mmd is current") {
        // The CLI writes a sibling .svg, and rendering is deterministic — so a stale committed file
        // means someone changed the renderer without regenerating, and the gallery lies.
        for
          corpus <- ZIO.service[Corpus]
          stale  <- ZIO.foreach(corpus.rendered) { r =>
            val svgPath = corpus.root.resolve("examples").resolve(r.name.replaceAll("\\.mmd$", ".svg"))
            ZIO.ifZIO(ZIO.succeedBlocking(Files.exists(svgPath)))(
              onTrue = read(svgPath).map(committed =>
                Option.when(committed != r.svg)(s"${r.name}: committed .svg is out of date")
              ),
              onFalse = ZIO.some(s"${r.name}: no committed .svg"),
            )
          }
        yield assertTrue(stale.flatten.isEmpty)
      },
      test("the corpus covers both diagram types") {
        for corpus <- ZIO.service[Corpus]
        yield
          val kinds = corpus.rendered
            .map(_.diagram)
            .map {
              case _: Diagram.Flowchart    => "flowchart"
              case _: Diagram.StateDiagram => "stateDiagram-v2"
              case _: Diagram.Sequence     => "sequence"
            }
            .toSet
          assertTrue(kinds == Set("flowchart", "stateDiagram-v2", "sequence"))
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
      val expected = r.diagram match
        case seq: Diagram.Sequence =>
          SequenceModel.participantOrder(seq.statements).map(id => s"actor-${id.value}").toSet
        case other =>
          declaredNodeIds(other).map(id => s"node-$id")
      val className = r.diagram match
        case _: Diagram.Sequence => "actor"
        case _                   => "node"
      val actual = r.root.withClass(className).flatMap(_.attr("id"))
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
    forEachExample("carries exactly one stylesheet and the markers for its diagram") { r =>
      val styles   = r.root.byTag("style")
      val markers  = r.root.byTag("marker").flatMap(_.attr("id"))
      val css      = styles.flatMap(_.text).mkString
      val expected = r.diagram match
        case _: Diagram.Sequence => Set("seq-head", "seq-open", "seq-cross", "seq-tail")
        case _                   => Set("arrowhead")
      assertTrue(
        styles.size == 1,
        css.contains("--mermoid"),
        markers.toSet == expected,
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
