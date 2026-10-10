package mermoid.css

/** Inner class names mermoid paints. User `classDef` names are not members. */
enum PaintClass(val cssName: String, val element: String, val description: String, val inSvg: Boolean = true):
  case DiagramBg       extends PaintClass("diagram-bg", "rect", "full-scene background")
  case NodeShape       extends PaintClass("node-shape", "rect/circle/polygon/path", "the node outline")
  case NodeLabel       extends PaintClass("node-label", "text", "the node's text")
  case EdgeLine        extends PaintClass("edge-line", "path", "the edge itself")
  case EdgeLabel       extends PaintClass("edge-label", "text", "the edge's label")
  case EdgeLabelBg     extends PaintClass("edge-label-bg", "rect", "the plate behind an edge label")
  case NoteRect        extends PaintClass("note-rect", "rect", "a note's box")
  case NoteText        extends PaintClass("note-text", "text", "a note's text")
  case NoteConnector   extends PaintClass("note-connector", "path", "the dashed line to its state")
  case SubgraphRect    extends PaintClass("subgraph-rect", "rect", "a subgraph frame")
  case SubgraphLabel   extends PaintClass("subgraph-label", "text", "a subgraph's title")
  case Arrowhead       extends PaintClass("arrowhead", "polygon", "the shared marker")
  case ArrowOpen       extends PaintClass("arrow-open", "polyline", "an open sequence arrowhead")
  case ArrowCross      extends PaintClass("arrow-cross", "path", "a crossed sequence arrow")
  case ActorBox        extends PaintClass("actor-box", "rect", "a participant header")
  case ActorLabel      extends PaintClass("actor-label", "text", "a participant or actor name")
  case ActorFigure     extends PaintClass("actor-figure", "circle/line", "a stick-figure actor")
  case Lifeline        extends PaintClass("lifeline", "line", "a participant's dashed lifeline")
  case MessageLine     extends PaintClass("message-line", "path", "a sequence message shaft")
  case MessageLabel    extends PaintClass("message-label", "text", "a sequence message label")
  case Activation      extends PaintClass("activation", "rect", "an activation bar on a lifeline")
  case FragmentFrame   extends PaintClass("fragment-frame", "rect", "a loop, alt, par, or highlight frame")
  case FragmentTab     extends PaintClass("fragment-tab", "rect", "the keyword tab on a fragment")
  case FragmentLabel   extends PaintClass("fragment-label", "text", "a fragment tab or divider label")
  case FragmentDivider extends PaintClass("fragment-divider", "line", "an alt or par section divider")
  case StartEnd        extends PaintClass("start-end", "on a node wrapper", "`[*]` in a state diagram")
  case StateEnd        extends PaintClass("state-end", "on a node wrapper", "the final `[*]`, drawn as a bullseye")
  case StateBar        extends PaintClass("state-bar", "on a node wrapper", "a fork or join bar")
  case SelfLoop extends PaintClass("self-loop", "on an edge wrapper", "edge that starts and ends on the same node")
  case HybridNodeLabel extends PaintClass("mermoid-node-label", "span", "HTML twin of node-label", inSvg = false)
  case IsSelected      extends PaintClass("is-selected", "hybrid node/note", "selected outline", inSvg = false)

  def selector: CssSelector.Class = CssSelector.Class(cssName)
end PaintClass

object PaintClass:
  def markdownTable: String =
    val rows = values.filter(_.inSvg).map(c => s"| `${c.cssName}` | `${c.element}` | ${c.description} |").mkString("\n")
    s"| Class | Element | What it is |\n|---|---|---|\n$rows"

/** Wrapper `<g>` class names. Shape and edge style suffixes live on [[mermoid.NodeShape]] / [[mermoid.EdgeStyle]]. */
enum WrapperClass(val cssName: String, val classList: String, val idPattern: String):
  case Node extends WrapperClass("node", "`node node-{shape}` + any `class` names", "`node-{nodeId}`")
  case Edge
      extends WrapperClass(
        "edge",
        "`edge edge-{style}` (+ ` self-loop`)",
        "`edge-{alias}` or `edge-{from}-{to}-{index}`",
      )
  case Note     extends WrapperClass("note", "`note`", "`note-{alias}` or `note-{stateId}-{index}`")
  case Subgraph extends WrapperClass("subgraph", "`subgraph`", "`subgraph-{id}`")
  case Actor    extends WrapperClass("actor", "`actor` or `actor actor-person`", "`actor-{id}`")
  case Message
      extends WrapperClass(
        "message",
        "`message message-solid` or `message message-dashed`",
        "`message-{index}`",
      )
  case Fragment extends WrapperClass("fragment", "`fragment`", "`fragment-{index}`")

  def selector: CssSelector.Class = CssSelector.Class(cssName)
end WrapperClass

object WrapperClass:
  def markdownTable: String =
    val rows = values
      .map(w => s"| ${w.cssName} | ${w.classList} | ${w.idPattern} |")
      .mkString("\n")
    s"| Element | Class | Id |\n|---|---|---|\n$rows"
end WrapperClass
