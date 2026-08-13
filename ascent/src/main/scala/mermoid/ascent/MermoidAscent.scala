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
    val scale  = containerWidth.map(scene.fitScale).getOrElse(1.0)
    val cssFit = containerWidth.isEmpty
    HybridPainter.paint(scene, selected, onSelect, scale, cssFit)

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
    yield interactiveRoot(d, config, selected, width, showWidthControls, toggleSelect(selected))
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
    yield interactiveRoot(d, config, selected, width, showWidthControls, toggleSelect(selected))

  /** Host-driven selection and width. Mechanoid live FSMs use this instead of reimplementing chrome.
    *
    * `selected` is the highlighted node id (typically the live state name). `onSelect` fires on click; the host decides
    * whether that click is a transition. Width controls are off unless `showWidthControls` is true.
    */
  def diagramControlled(
      mmd: String,
      selected: Source[Option[String]],
      onSelect: String => UIO[Unit],
      width: Source[Double],
      config: RenderConfig = RenderConfig(),
      showWidthControls: Boolean = false,
  ): UI[Any] =
    interactiveRoot(parseOrThrow(mmd), config, selected, width, showWidthControls, onSelect)

  private def toggleSelect(selected: Source[Option[String]]): String => UIO[Unit] = id =>
    selected.get.flatMap {
      case Some(`id`) => selected.set(None)
      case _          => selected.set(Some(id))
    }

  private def interactiveRoot(
      diagram: Diagram,
      config: RenderConfig,
      selected: Source[Option[String]],
      width: Source[Double],
      showWidthControls: Boolean,
      onSelect: String => UIO[Unit],
  ): UI[Any] =

    val body = _root_.ascent.squawk.Squawk.zipWith(width, selected) { (w, sel) =>
      val scene = DiagramLayout.scene(diagram, config, Some(Viewport(w)))
      val scale = scene.fitScale(w)
      HybridPainter.paint(scene, sel, onSelect, scale, cssFit = false)
    }

    val controls: UI[Any] =
      if !showWidthControls then UI.Empty
      else
        UI.Element(
          "div",
          Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.Controls.cssName))),
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
              Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.WidthLabel.cssName))),
              Vector(UI.ReactiveText(width.map(w => s"viewport ${w.toInt}px"))),
            ),
          ),
        )

    UI.Element(
      "div",
      Vector(Attr.StaticAttr("class", AttrValue.Str(HybridClass.Ascent.cssName))),
      Vector(controls, UI.ReactiveChild(body)),
    )
  end interactiveRoot

  private def parseOrThrow(mmd: String): Diagram =
    MermaidParser.parse(mmd) match
      case Right(d)  => d
      case Left(err) => throw new IllegalArgumentException(s"mermoid could not parse this diagram: $err\n$mmd")
end MermoidAscent
