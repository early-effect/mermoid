package mermoid

enum SelfLoopSide:
  case Top, Right, Bottom

case class Point(x: Double, y: Double)

case class LayoutNode(
    id: NodeId,
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
    from: NodeId,
    to: NodeId,
    style: EdgeStyle,
    label: Option[String],
    selfLoopIndex: Int = 0,
    alias: Option[String] = None,
    edgeIndex: Int = 0,
    edgeCount: Int = 1,
)

/** A frame whose geometry was decided by layout, not measured afterwards from its members. */
case class PlacedFrame(
    id: String,
    label: Option[String],
    rect: Rect,
    /** Absolute y of a dashed divider inside the frame. */
    dividers: List[Double] = Nil,
)

/** A note with no state to point at. `rect` is top-left in scene coordinates. */
case class FloatingNoteBox(text: String, alias: NodeId, rect: Rect)

case class StateNote(
    position: NotePosition,
    stateId: NodeId,
    text: String,
    textAlign: NoteTextAlign = NoteTextAlign.Left,
    alias: Option[String] = None,
    noteIndex: Int = 0,
)
