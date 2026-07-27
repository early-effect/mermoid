package mermoid

// -- Diagram wrapper ----------------------------------------------------------

enum Diagram:
  case Flowchart(direction: Direction, statements: List[FlowStatement])
  case StateDiagram(statements: List[StateStatement])

// -- Flowchart ----------------------------------------------------------------

enum Direction:
  case TB, TD, BT, LR, RL

enum NodeShape:
  case Rect             // [text]
  case Round            // (text)
  case Stadium          // ([text])
  case Subroutine       // [[text]]
  case Cylinder         // [(text)]
  case Circle           // ((text))
  case Rhombus          // {text}
  case Hexagon          // {{text}}
  case Parallelogram    // [/text/]
  case ParallelogramAlt // [\text\]
  case Trapezoid        // [/text\]
  case TrapezoidAlt     // [\text/]
  case DoubleCircle     // (((text)))
end NodeShape

case class NodeDef(id: String, label: Option[String], shape: NodeShape)

enum EdgeStyle:
  case Arrow      // -->
  case Open       // ---
  case Dotted     // -.->
  case Thick      // ==>
  case DottedOpen // -.-

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
  case StyleSt(nodeId: String, styles: Map[String, String])
  case ClassDefSt(className: String, styles: Map[String, String])
  case ClassSt(nodeIds: List[String], className: String)
  case ClickSt(binding: ClickBinding)

// -- State Diagram ------------------------------------------------------------

enum NotePosition:
  case RightOf, LeftOf

enum NoteTextAlign:
  case Left, Center, Right

case class StateTransition(from: String, to: String, label: Option[String])

case class StateStyle(noteAlign: Option[NoteTextAlign] = None)

enum StateStatement:
  case TransitionSt(transition: StateTransition)
  case NoteSt(position: NotePosition, stateId: String, text: String, alias: Option[String] = None)
  case StyleSt(stateId: String, style: StateStyle)
