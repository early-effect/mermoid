package mermoid

enum SelfLoopSide:
  case Top, Right, Bottom

case class Point(x: Double, y: Double)

case class LayoutNode(
    id: String,
    label: String,
    shape: NodeShape,
    center: Point,
    width: Double,
    height: Double,
    styles: Map[css.CssProperty, String] = Map.empty,
    cssClasses: List[String] = Nil,
    /** Invisible routing waypoint; never painted as a node. */
    dummy: Boolean = false,
)

case class LayoutEdge(
    from: String,
    to: String,
    style: EdgeStyle,
    label: Option[String],
    selfLoopIndex: Int = 0,
    alias: Option[String] = None,
    edgeIndex: Int = 0,
    edgeCount: Int = 1,
)

case class StateNote(
    position: NotePosition,
    stateId: String,
    text: String,
    textAlign: NoteTextAlign = NoteTextAlign.Left,
    alias: Option[String] = None,
    noteIndex: Int = 0,
)
