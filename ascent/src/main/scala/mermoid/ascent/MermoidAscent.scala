package mermoid.ascent

import ascent.ast.{AscentEvent, Attr, UI}
import ascent.domtypes.{AttrValue, Events}
import ascent.dsl.*
import ascent.squawk.{Source, sq}
import mermoid.*
import zio.*

/** Ascent UI painter for mermoid diagrams: hybrid HTML nodes + SVG edges, with optional reactive reflow. */
object MermoidAscent:

  /** Paint a static hybrid diagram (SSR-friendly). Uses unconstrained layout unless `viewport` is set. */
  def diagram(
      mermaid: Mermaid,
      config: RenderConfig = RenderConfig(),
      viewport: Option[Viewport] = None,
  ): UI[Any] =
    fromScene(DiagramLayout.scene(mermaid.diagram, config, viewport), selected = None, onSelect = _ => ZIO.unit)

  /** Inert SVG embed mapped into ascent UI (byte-stable structure demos). */
  def svgDiagram(mermaid: Mermaid, config: RenderConfig = RenderConfig()): UI[Any] =
    SvgBridge.toUi(SvgRenderer.renderTree(mermaid.diagram, config))

  /** SVG markup string for the same source. */
  def svg(mermaid: Mermaid, config: RenderConfig = RenderConfig()): String =
    SvgRenderer.render(mermaid.diagram, config)

  def fromScene(
      scene: Scene,
      selected: Option[NodeId] = None,
      onSelect: NodeId => UIO[Unit] = _ => ZIO.unit,
      containerWidth: Option[Double] = None,
  ): UI[Any] =
    val cssFit = containerWidth.isEmpty && (scene.config.responsive.fit match
      case ContainerFit.ToWidth(_) => true
      case ContainerFit.Off        => false)
    val scale = containerWidth.fold(1.0)(scene.fitScale)
    HybridPainter.paint(scene, selected, onSelect, scale, cssFit)
  end fromScene

  /** Interactive diagram: selection + viewport-driven re-layout.
    *
    * Width starts at `initialWidth`. Use the built-in Narrow/Wide controls (and optional external [[width]] source) to
    * reflow; edges and splines are recomputed from a fresh [[Scene]] on every width change. Selection id is preserved
    * across reflow.
    */
  def diagramInteractive(
      mermaid: Mermaid,
      config: RenderConfig = RenderConfig(),
      initialWidth: Double = 720.0,
      widthControls: WidthControls = WidthControls.Shown,
  ): UIO[UI[Any]] =
    for
      selected <- sq(Option.empty[NodeId])
      width    <- sq(initialWidth)
    yield interactiveRoot(mermaid.diagram, config, selected, width, widthControls, toggleSelect(selected))
  end diagramInteractive

  /** Same as [[diagramInteractive]] but accepts an external width source (e.g. host ResizeObserver). */
  def diagramResponsive(
      mermaid: Mermaid,
      width: Source[Double],
      config: RenderConfig = RenderConfig(),
      widthControls: WidthControls = WidthControls.Hidden,
  ): UIO[UI[Any]] =
    for selected <- sq(Option.empty[NodeId])
    yield interactiveRoot(mermaid.diagram, config, selected, width, widthControls, toggleSelect(selected))

  /** Host-driven selection and width. Mechanoid live FSMs use this instead of reimplementing chrome.
    *
    * `selected` is the highlighted node id (typically the live state name). `onSelect` fires on click; the host decides
    * whether that click is a transition. Width controls are hidden unless `widthControls` is [[WidthControls.Shown]].
    */
  def diagramControlled(
      mermaid: Mermaid,
      selected: Source[Option[NodeId]],
      onSelect: NodeId => UIO[Unit],
      width: Source[Double],
      config: RenderConfig = RenderConfig(),
      widthControls: WidthControls = WidthControls.Hidden,
  ): UI[Any] =
    interactiveRoot(mermaid.diagram, config, selected, width, widthControls, onSelect)

  private def toggleSelect(selected: Source[Option[NodeId]]): NodeId => UIO[Unit] = id =>
    selected.get.flatMap {
      case Some(`id`) => selected.set(None)
      case _          => selected.set(Some(id))
    }

  private def interactiveRoot(
      diagram: Diagram,
      config: RenderConfig,
      selected: Source[Option[NodeId]],
      width: Source[Double],
      widthControls: WidthControls,
      onSelect: NodeId => UIO[Unit],
  ): UI[Any] =

    val body = _root_.ascent.squawk.Squawk.zipWith(width, selected) { (w, sel) =>
      val scene = DiagramLayout.scene(diagram, config, Some(Viewport(w)))
      val scale = scene.fitScale(w)
      HybridPainter.paint(scene, sel, onSelect, scale, cssFit = false)
    }

    val controls: UI[Any] =
      widthControls match
        case WidthControls.Hidden => UI.Empty
        case WidthControls.Shown  =>
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

end MermoidAscent
