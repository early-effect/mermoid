package mermoid

// -- Diagram wrapper ----------------------------------------------------------

enum Diagram:
  case Flowchart(direction: Direction, statements: List[FlowStatement])
  case StateDiagram(statements: List[StateStatement])

// -- Flowchart ----------------------------------------------------------------

enum Direction:
  case TB, TD, BT, LR, RL

enum NodeShape(val mermaid: String, val cssClass: String):
  case Rect             extends NodeShape("[text]", "rect")
  case Round            extends NodeShape("(text)", "round")
  case Stadium          extends NodeShape("([text])", "stadium")
  case Subroutine       extends NodeShape("[[text]]", "subroutine")
  case Cylinder         extends NodeShape("[(text)]", "cylinder")
  case Circle           extends NodeShape("((text))", "circle")
  case Rhombus          extends NodeShape("{text}", "rhombus")
  case Hexagon          extends NodeShape("{{text}}", "hexagon")
  case Parallelogram    extends NodeShape("[/text/]", "parallelogram")
  case ParallelogramAlt extends NodeShape("[\\text\\]", "parallelogram-alt")
  case Trapezoid        extends NodeShape("[/text\\]", "trapezoid")
  case TrapezoidAlt     extends NodeShape("[\\text/]", "trapezoid-alt")
  case DoubleCircle     extends NodeShape("(((text)))", "double-circle")

  def wrapperClass: String = s"node-$cssClass"
end NodeShape

object NodeShape:
  def markdownTable: String =
    val rows = values
      .map(s => s"| `A${s.mermaid.replace("\\", "\\\\")}` | `${s.productPrefix}` | `${s.wrapperClass}` |")
      .mkString("\n")
    s"| Syntax | `NodeShape` | CSS class |\n|---|---|---|\n$rows"
end NodeShape

case class NodeDef(
    id: String,
    label: Option[String],
    shape: NodeShape,
    /** Classes from the `:::` suffix (`A:::hot` / `A[Label]:::hot,cold`). */
    cssClasses: List[String] = Nil,
)

enum EdgeStyle(val mermaid: String, val cssClass: String, val arrowhead: Boolean):
  case Arrow      extends EdgeStyle("-->", "arrow", true)
  case Open       extends EdgeStyle("---", "open", false)
  case Dotted     extends EdgeStyle("-.->", "dotted", true)
  case Thick      extends EdgeStyle("==>", "thick", true)
  case DottedOpen extends EdgeStyle("-.-", "dotted-open", false)

  def wrapperClass: String = s"edge-$cssClass"
end EdgeStyle

object EdgeStyle:
  def markdownTable: String =
    val rows = values
      .map { s =>
        val head = if s.arrowhead then "yes" else "no"
        s"| `A ${s.mermaid} B` | `${s.productPrefix}` | `${s.wrapperClass}` | $head |"
      }
      .mkString("\n")
    s"| Syntax | `EdgeStyle` | CSS class | Arrowhead |\n|---|---|---|\n$rows"
end EdgeStyle

case class Edge(from: String, to: String, style: EdgeStyle, label: Option[String], alias: Option[String] = None)

/** A Mermaid `click` binding: tooltip and/or link and/or opaque callback name. */
case class ClickBinding(
    nodeId: String,
    tooltip: Option[String] = None,
    href: Option[String] = None,
    linkTarget: Option[String] = None,
    callbackName: Option[String] = None,
)

enum FlowStatement:
  case NodeSt(node: NodeDef)
  case EdgeSt(edge: Edge, fromNode: NodeDef, toNode: NodeDef)
  case SubgraphSt(id: String, label: Option[String], direction: Option[Direction], statements: List[FlowStatement])
  case StyleSt(nodeId: String, styles: Map[css.CssProperty, String])
  case ClassDefSt(className: String, styles: Map[css.CssProperty, String])
  case ClassSt(nodeIds: List[String], className: String)
  case ClickSt(binding: ClickBinding)

// -- State Diagram ------------------------------------------------------------

enum NotePosition:
  case RightOf, LeftOf

enum NoteTextAlign:
  case Left, Center, Right

case class StateTransition(
    from: String,
    to: String,
    label: Option[String],
    fromClasses: List[String] = Nil,
    toClasses: List[String] = Nil,
)

case class StateStyle(
    noteAlign: Option[NoteTextAlign] = None,
    paint: Map[css.CssProperty, String] = Map.empty,
)

object StateStyle:
  private val noteAlignProp = css.CssProperty.parse("noteAlign")

  /** Split `style A fill:#f00,noteAlign:center` into note alignment plus SVG/HTML paint. */
  def fromProperties(props: Map[css.CssProperty, String]): StateStyle =
    val align = props.get(noteAlignProp).flatMap {
      case "center" => Some(NoteTextAlign.Center)
      case "right"  => Some(NoteTextAlign.Right)
      case "left"   => Some(NoteTextAlign.Left)
      case _        => None
    }
    StateStyle(noteAlign = align, paint = props - noteAlignProp)
end StateStyle

enum StateStatement:
  case TransitionSt(transition: StateTransition)
  case NoteSt(position: NotePosition, stateId: String, text: String, alias: Option[String] = None)
  case StyleSt(stateId: String, style: StateStyle)
  case ClassDefSt(className: String, styles: Map[css.CssProperty, String])
  case ClassSt(stateIds: List[String], className: String)
