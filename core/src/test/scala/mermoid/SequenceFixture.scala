package mermoid

/** The span diagram the sequence specs and the committed example share. */
object SequenceFixture:

  val span: String =
    """sequenceDiagram
      |    participant Alice
      |    participant Bob
      |    participant Carol
      |    participant Dave
      |    participant Eve
      |    Alice->>Bob: place order
      |    Bob->>Carol: reserve stock and write the durable record
      |    Bob->>Carol: confirm the row after reserve completes
      |    Bob->>Alice: order accepted
      |    Alice->>Dave: ask status
      |    Dave->>Carol: read hold
      |    Dave-->>Alice: accepted or pending
      |    Eve->>Dave: fetch snapshot
      |    Dave-->>Eve: snapshot plus version
      |""".stripMargin

  def laid(src: String, config: RenderConfig = RenderConfig()): Option[SequenceScene] =
    MermaidParser.parse(src) match
      case Right(diagram) =>
        DiagramLayout.scene(diagram, config) match
          case Scene.Sequence(scene) => Some(scene)
          case Scene.Ranked(_)       => None
      case Left(_) => None
end SequenceFixture
