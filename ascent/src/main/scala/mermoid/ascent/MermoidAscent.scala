package mermoid.ascent

import ascent.ast.{AscentEvent, Attr, UI}
import ascent.domtypes.{AttrValue, Events}
import ascent.dsl.*
import ascent.squawk.{Source, sq}
import mermoid.*
import zio.*

/** Ascent UI painter for mermoid diagrams: hybrid HTML nodes + SVG edges, with optional reactive reflow. */
object MermoidAscent:

  /** Parse and paint a static hybrid diagram (SSR-friendly). Uses unconstrained layout unless `viewport` is set. */
  def diagram(
      mmd: String,
      config: RenderConfig = RenderConfig(),
      viewport: Option[Viewport] = None,
  ): UI[Any] =
    val d = parseOrThrow(mmd)
    fromScene(DiagramLayout.scene(d, config, viewport), selected = None, onSelect = _ => ZIO.unit)

  /** Inert SVG embed mapped into ascent UI (byte-stable structure demos). */
  def svgDiagram(mmd: String, config: RenderConfig = RenderConfig()): UI[Any] =
    val d = parseOrThrow(mmd)
    SvgBridge.toUi(SvgRenderer.renderTree(d, config))

  /** SVG markup string for the same source. */
  def svg(mmd: String, config: RenderConfig = RenderConfig()): String =
    val d = parseOrThrow(mmd)
    SvgRenderer.render(d, config)

  def fromScene(
      scene: DiagramScene,
      selected: Option[String] = None,
      onSelect: String => UIO[Unit] = _ => ZIO.unit,
      containerWidth: Option[Double] = None,
  ): UI[Any] =
    val scale = containerWidth.map(scene.fitScale).getOrElse(1.0)
    HybridPainter.paint(scene, selected, onSelect, scale)

  /** Interactive diagram: selection + viewport-driven re-layout.
    *
    * Width starts at `initialWidth`. Use the built-in Narrow/Wide controls (and optional external [[width]] source) to
    * reflow; edges/splines are recomputed from a fresh [[DiagramScene]] on every width change. Selection id is
    * preserved across reflow.
    */
  def diagramInteractive(
      mmd: String,
      config: RenderConfig = RenderConfig(),
      initialWidth: Double = 720.0,
      showWidthControls: Boolean = true,
  ): UIO[UI[Any]] =
    val d = parseOrThrow(mmd)
    for
      selected <- sq(Option.empty[String])
      width    <- sq(initialWidth)
    yield interactiveRoot(d, config, selected, width, showWidthControls)
  end diagramInteractive

  /** Same as [[diagramInteractive]] but accepts an external width source (e.g. host ResizeObserver). */
  def diagramResponsive(
      mmd: String,
      width: Source[Double],
      config: RenderConfig = RenderConfig(),
      showWidthControls: Boolean = false,
  ): UIO[UI[Any]] =
    val d = parseOrThrow(mmd)
    for selected <- sq(Option.empty[String])
    yield interactiveRoot(d, config, selected, width, showWidthControls)

  private def interactiveRoot(
      diagram: Diagram,
      config: RenderConfig,
      selected: Source[Option[String]],
      width: Source[Double],
      showWidthControls: Boolean,
  ): UI[Any] =
    val onSelect: String => UIO[Unit] = id =>
      selected.get.flatMap {
        case Some(`id`) => selected.set(None)
        case _          => selected.set(Some(id))
      }

    val body = _root_.ascent.squawk.Squawk.zipWith(width, selected) { (w, sel) =>
      val scene = DiagramLayout.scene(diagram, config, Some(Viewport(w)))
      val scale = scene.fitScale(w)
      HybridPainter.paint(scene, sel, onSelect, scale)
    }

    val controls: UI[Any] =
      if !showWidthControls then UI.Empty
      else
        UI.Element(
          "div",
          Vector(Attr.StaticAttr("class", AttrValue.Str("mermoid-controls"))),
          Vector(
            UI.Element(
              "button",
              Vector(
                Attr.StaticAttr("type", AttrValue.Str("button")),
                Events.onClick((_: AscentEvent) => width.set(360.0)),
              ),
              Vector(UI.Text("Narrow")),
            ),
            UI.Element(
              "button",
              Vector(
                Attr.StaticAttr("type", AttrValue.Str("button")),
                Events.onClick((_: AscentEvent) => width.set(640.0)),
              ),
              Vector(UI.Text("Medium")),
            ),
            UI.Element(
              "button",
              Vector(
                Attr.StaticAttr("type", AttrValue.Str("button")),
                Events.onClick((_: AscentEvent) => width.set(900.0)),
              ),
              Vector(UI.Text("Wide")),
            ),
            UI.Element(
              "span",
              Vector(Attr.StaticAttr("class", AttrValue.Str("mermoid-width-label"))),
              Vector(UI.ReactiveText(width.map(w => s"viewport ${w.toInt}px"))),
            ),
          ),
        )

    UI.Element(
      "div",
      Vector(Attr.StaticAttr("class", AttrValue.Str("mermoid-ascent"))),
      Vector(controls, UI.ReactiveChild(body)),
    )
  end interactiveRoot

  private def parseOrThrow(mmd: String): Diagram =
    MermaidParser.parse(mmd) match
      case Right(d)  => d
      case Left(err) => throw new IllegalArgumentException(s"mermoid could not parse this diagram: $err\n$mmd")
end MermoidAscent
