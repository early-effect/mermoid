package mermoid

import zio.test.*

object SvgSerializerSpec extends ZIOSpecDefault:

  private def compact(node: SvgNode): String = SvgSerializer.renderCompact(node)

  def spec = suite("SvgSerializer")(
    suite("elements")(
      test("an element with no children is self-closing") {
        assertTrue(compact(SvgNode.leaf("rect")("x" -> "1", "y" -> "2")) == """<rect x="1" y="2"/>""")
      },
      test("an element with no attributes omits the attribute space") {
        assertTrue(compact(SvgNode.leaf("defs")()) == "<defs/>")
      },
      test("attribute order is preserved") {
        val node = SvgNode.leaf("line")("z" -> "1", "a" -> "2", "m" -> "3")
        assertTrue(compact(node) == """<line z="1" a="2" m="3"/>""")
      },
      test("nested elements round-trip in order") {
        val node = SvgNode.elem("g")("class" -> "node")(
          SvgNode.leaf("rect")("x"   -> "0"),
          SvgNode.leaf("circle")("r" -> "5"),
        )
        assertTrue(compact(node) == """<g class="node"><rect x="0"/><circle r="5"/></g>""")
      },
    ),
    suite("escaping")(
      test("escapes text content") {
        assertTrue(compact(SvgNode.Text("a < b & c > d")) == "a &lt; b &amp; c &gt; d")
      },
      test("escapes attribute values") {
        val node = SvgNode.leaf("g")("id" -> """a"b<c&d""")
        assertTrue(compact(node) == """<g id="a&quot;b&lt;c&amp;d"/>""")
      },
      test("a quote in a node id cannot break out of the attribute") {
        val node = ShapeRenderer.nodeToSvg(
          LayoutNode("""evil" onload="x""", "L", NodeShape.Rect, Point(0, 0), 10, 10),
          RenderConfig(),
        )
        val out = compact(node)
        assertTrue(
          out.contains("""id="node-evil&quot; onload=&quot;x""""),
          !out.contains("""onload="x""""),
        )
      },
      test("escapes text inside an element") {
        val node = SvgNode.textElem("text")("class" -> "l")("<b>")
        assertTrue(compact(node) == """<text class="l">&lt;b&gt;</text>""")
      },
    ),
    suite("raw")(
      test("Raw content is emitted verbatim") {
        assertTrue(compact(SvgNode.Raw(".a > .b { content: \"&\" }")) == """.a > .b { content: "&" }""")
      },
      test("a style block keeps its CSS unescaped") {
        val node = SvgNode.elem("style")()(SvgNode.Raw(".node-shape > text { fill: #333 }"))
        assertTrue(compact(node) == "<style>.node-shape > text { fill: #333 }</style>")
      },
    ),
    suite("indentation")(
      test("element children are indented one level per depth") {
        val node = SvgNode.elem("svg")()(SvgNode.elem("g")()(SvgNode.leaf("rect")()))
        assertTrue(SvgSerializer.render(node) == "<svg>\n  <g>\n    <rect/>\n  </g>\n</svg>")
      },
      test("character-data-only children stay on one line") {
        val node = SvgNode.elem("svg")()(SvgNode.textElem("text")()("hi"))
        assertTrue(SvgSerializer.render(node) == "<svg>\n  <text>hi</text>\n</svg>")
      },
      test("compact rendering adds no whitespace") {
        val node = SvgNode.elem("svg")()(SvgNode.elem("g")()(SvgNode.leaf("rect")()))
        assertTrue(!SvgSerializer.renderCompact(node).contains("\n"))
      },
    ),
    suite("renderTree")(
      test("the tree serializes to exactly what render produces") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.EdgeSt(
              Edge("A", "B", EdgeStyle.Arrow, Some("go")),
              NodeDef("A", None, NodeShape.Rect),
              NodeDef("B", None, NodeShape.Circle),
            )
          ),
        )
        val tree = SvgRenderer.renderTree(diagram)
        assertTrue(SvgSerializer.render(tree) == SvgRenderer.render(diagram))
      },
      test("the root is an svg element with viewBox") {
        val diagram =
          Diagram.Flowchart(Direction.TD, List(FlowStatement.NodeSt(NodeDef("A", Some("Hi"), NodeShape.Rect))))
        SvgRenderer.renderTree(diagram) match
          case SvgNode.Element(tag, attrs, children) =>
            assertTrue(
              tag == "svg",
              attrs.exists((k, _) => k == "viewBox"),
              attrs.contains("xmlns" -> "http://www.w3.org/2000/svg"),
              children.nonEmpty,
            )
          case _ => assertTrue(false)
      },
      test("state diagrams also produce a tree") {
        val diagram = Diagram.StateDiagram(
          List(
            StateStatement.TransitionSt(StateTransition("[*]", "Idle", None)),
            StateStatement.NoteSt(NotePosition.RightOf, "Idle", "hello"),
          )
        )
        val tree = SvgRenderer.renderTree(diagram)
        assertTrue(SvgSerializer.render(tree) == SvgRenderer.render(diagram))
      },
    ),
    suite("number formatting")(
      // This suite runs on both JVM and Scala.js. Double.toString disagrees between them, so these
      // assertions are what keeps the two platforms emitting byte-identical SVG.
      test("whole numbers lose the decimal point") {
        assertTrue(Num.format(14.0) == "14", Num.format(0.0) == "0", Num.format(-3.0) == "-3")
      },
      test("fractional numbers keep their digits") {
        assertTrue(Num.format(1.5) == "1.5", Num.format(-0.25) == "-0.25")
      },
      test("non-finite values pass through rather than truncating to a bogus integer") {
        assertTrue(Num.format(Double.NaN) == "NaN", Num.format(Double.PositiveInfinity).contains("Infinity"))
      },
      test("a rendered diagram contains no trailing .0 coordinates") {
        val diagram = Diagram.Flowchart(
          Direction.TD,
          List(
            FlowStatement.EdgeSt(
              Edge("A", "B", EdgeStyle.Arrow, Some("go")),
              NodeDef("A", Some("Start"), NodeShape.Circle),
              NodeDef("B", Some("End"), NodeShape.Rect),
            )
          ),
        )
        val svg = SvgRenderer.render(diagram)
        assertTrue(!svg.contains(".0\""), !svg.contains("NaN"))
      },
    ),
    suite("buildLayoutEdges")(
      test("indexes parallel edges between the same pair") {
        val edges = List(
          Edge("A", "B", EdgeStyle.Arrow, None),
          Edge("A", "B", EdgeStyle.Arrow, None),
          Edge("A", "C", EdgeStyle.Arrow, None),
        )
        val built = SvgRenderer.buildLayoutEdges(edges)
        assertTrue(
          built.map(_.edgeIndex) == List(0, 1, 0),
          built.map(_.edgeCount) == List(2, 2, 1),
        )
      },
      test("indexes self-loops per node") {
        val edges = List(
          Edge("A", "A", EdgeStyle.Arrow, Some("one")),
          Edge("A", "A", EdgeStyle.Arrow, Some("two")),
          Edge("B", "B", EdgeStyle.Arrow, Some("three")),
        )
        val built = SvgRenderer.buildLayoutEdges(edges)
        assertTrue(built.map(_.selfLoopIndex) == List(0, 1, 0))
      },
      test("non-self edges have selfLoopIndex 0") {
        val built = SvgRenderer.buildLayoutEdges(List(Edge("A", "B", EdgeStyle.Arrow, None)))
        assertTrue(built.head.selfLoopIndex == 0)
      },
    ),
    suite("longestPathLayers")(
      test("a chain layers sequentially") {
        val layers = Layout.longestPathLayers(List("A", "B", "C"), Map("B" -> List("A"), "C" -> List("B")))
        assertTrue(layers == Map("A" -> 0, "B" -> 1, "C" -> 2))
      },
      test("a node takes its deepest predecessor's layer plus one") {
        // A -> B -> C and A -> C: C must land below B, not beside it
        val layers = Layout.longestPathLayers(List("A", "B", "C"), Map("B" -> List("A"), "C" -> List("A", "B")))
        assertTrue(layers("C") == 2)
      },
      test("a cycle terminates rather than recursing forever") {
        val layers = Layout.longestPathLayers(List("A", "B"), Map("A" -> List("B"), "B" -> List("A")))
        assertTrue(layers.keySet == Set("A", "B"))
      },
    ),
  )
end SvgSerializerSpec
