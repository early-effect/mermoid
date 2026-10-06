package mermoid.ascent

import _root_.ascent.{
  BasicShape,
  Cls,
  Color,
  CssMember,
  Elem,
  Declaration,
  Filter,
  GlobalRule,
  GlobalStyle,
  Length,
  PseudoClass,
  S,
  Sel,
  Selector,
  Shadow,
}
import mermoid.{ContainerFit, NodeShape, Num}
import mermoid.css.{PaintClass, ThemeVar, WrapperClass}

/** Hybrid chrome class names. SVG paint classes live on [[PaintClass]]; these are the HTML shell. */
enum HybridClass(val cssName: String, val description: String):
  case Root        extends HybridClass("mermoid-root", "outer wrap; may also carry fit")
  case Fit         extends HybridClass("mermoid-fit", "scale the scene to the parent column")
  case Diagram     extends HybridClass("mermoid-diagram", "positioned scene box")
  case Scaler      extends HybridClass("mermoid-diagram-scaler", "transform origin for fit/scale")
  case Edges       extends HybridClass("mermoid-edges", "SVG layer for edges, frames, connectors")
  case Node        extends HybridClass("mermoid-node", "HTML button/link for a node")
  case Actor       extends HybridClass("mermoid-actor", "HTML button for a sequence participant or actor")
  case ActorBox    extends HybridClass("mermoid-actor-box", "participant header box")
  case ActorPerson extends HybridClass("mermoid-actor-person", "stick-figure actor")
  case ActorHead   extends HybridClass("mermoid-actor-head", "actor head")
  case ActorStem   extends HybridClass("mermoid-actor-stem", "actor stem")
  case ActorLabel  extends HybridClass("mermoid-actor-label", "participant or actor name")
  case DiamondFill extends HybridClass("mermoid-node-diamond-fill", "inner fill of a rhombus")
  case Tooltip     extends HybridClass("mermoid-tooltip", "hover label from a click tooltip")
  case Note        extends HybridClass("mermoid-note", "HTML card for a state note")
  case Controls    extends HybridClass("mermoid-controls", "Narrow/Wide buttons")
  case WidthLabel  extends HybridClass("mermoid-width-label", "current viewport width")
  case NodeLink    extends HybridClass("mermoid-node-link", "node painted as <a href>")
  case Ascent      extends HybridClass("mermoid-ascent", "interactive root")
  case IsIncident  extends HybridClass("is-incident", "edge that touches the selected node")

  def sel: Sel = Cls(cssName)
end HybridClass

enum HybridVar(val cssName: String):
  case SceneWidth  extends HybridVar("--mermoid-scene-width")
  case SceneHeight extends HybridVar("--mermoid-scene-height")
  case Fit         extends HybridVar("--mermoid-fit")
  case ScaleFloor  extends HybridVar("--mermoid-scale-floor")

  def cssVar: String = s"var($cssName)"

private object HybridTokens:
  val selectionStroke: String =
    ThemeVar.Selection.cssVar(ThemeVar.Line.cssVar("#333"))

  def themeColor(v: ThemeVar, fallback: String): Color =
    Color.keyword(v.cssVar(fallback))

  val diamond: BasicShape =
    BasicShape.polygon("50% 0%", "100% 50%", "50% 100%", "0% 50%")

  val diamondShadow: Shadow =
    Shadow(Length.px(0), Length.px(0), Length.px(1.5), Color.keyword(selectionStroke))

  val node: Sel  = HybridClass.Node.sel
  val shape: Sel = Cls(PaintClass.NodeShape.cssName)

  def shaped(shape: NodeShape): Sel =
    node.cls(shape.wrapperClass).descendant(HybridTokens.shape)

  def rule(sel: Sel)(members: CssMember*): GlobalRule =
    GlobalRule.selector(sel)(members*)
end HybridTokens

/** Typed hybrid chrome. Public class names stay on [[HybridClass]] / [[PaintClass]] so hosts can still target them. */
object HybridChrome
    extends GlobalStyle(
      // `\3c number\3e ` is the CSS string `<number>`. A literal `<` fails cssIsEntitySafe and the painter
      // drops the whole stylesheet, theme included.
      GlobalRule.raw(
        "mermoid-fit-property",
        s"""@property ${HybridVar.Fit.cssName} { syntax: "\\3c number\\3e "; inherits: true; initial-value: 1; }""",
      ),
      GlobalRule.raw(
        "mermoid-scale-floor-property",
        s"""@property ${HybridVar.ScaleFloor.cssName} { syntax: "\\3c number\\3e "; inherits: true; initial-value: ${Num
            .format(ContainerFit.defaultFloor)}; }""",
      ),
      HybridTokens.rule(HybridClass.Diagram.sel)(
        S.position.relative,
        S.fontFamily(ThemeVar.FontFamily.cssVar("sans-serif")),
        S.fontSize(ThemeVar.FontSize.cssVar("14px")),
        S.color(HybridTokens.themeColor(ThemeVar.Text, "#333")),
        S.background(HybridTokens.themeColor(ThemeVar.Background, "#ffffff")),
        S.overflow.visible,
      ),
      HybridTokens.rule(HybridClass.Scaler.sel)(
        S.position.relative,
        S.transformOrigin("top left"),
      ),
      HybridTokens.rule(HybridClass.Edges.sel)(
        S.position.absolute,
        S.inset.zero,
        S.width.pct(100),
        S.height.pct(100),
        S.overflow.visible,
        S.pointerEvents.none,
      ),
      HybridTokens.rule(
        HybridClass.Edges.sel
          .descendant(Cls(WrapperClass.Edge.cssName).cls(HybridClass.IsIncident.cssName))
          .descendant(Sel.tag("path"))
          .or(
            HybridClass.Edges.sel
              .descendant(Cls(WrapperClass.Edge.cssName).cls(HybridClass.IsIncident.cssName))
              .descendant(Sel.tag("line"))
          )
      )(
        S.strokeWidth(3),
        S.opacity(1),
      ),
      HybridTokens.rule(HybridTokens.node)(
        S.position.absolute,
        S.display.flex,
        S.alignItems.center,
        S.justifyContent.center,
        S.boxSizing.borderBox,
        S.margin.zero,
        S.padding.zero,
        S.cursor.pointer,
        S.border.none,
        S.background(Color.transparent),
        S.fontFamily.inherit,
        S.fontSize.inherit,
        S.lineHeight(1.2),
        S.textAlign.center,
        S.zIndex(2),
        Selector(Sel.descendant(HybridTokens.shape))(
          S.position.absolute,
          S.inset.zero,
          S.boxSizing.borderBox,
          S.pointerEvents.none,
          S.zIndex(0),
        ),
        Selector(PseudoClass.hover)(
          S.filter.brightness(1.05)
        ),
      ),
      // Paint sits in :where so a classDef or theme `.node-shape` wins regardless of emit order.
      // Layout above stays on `.mermoid-node` so a paint rule cannot pull the shape out of the button.
      HybridTokens.rule(PseudoClass.where(HybridTokens.node))(
        S.color(HybridTokens.themeColor(ThemeVar.Text, "#333"))
      ),
      HybridTokens.rule(PseudoClass.where(HybridTokens.node).descendant(HybridTokens.shape))(
        S.background(HybridTokens.themeColor(ThemeVar.MainBkg, "#ececff")),
        S.border.solid(Length.px(2), HybridTokens.themeColor(ThemeVar.NodeBorder, "#9370db")),
      ),
      HybridTokens.rule(
        HybridTokens.node
          .descendant(Cls(PaintClass.HybridNodeLabel.cssName))
          .or(HybridTokens.node.descendant(HybridClass.DiamondFill.sel))
      )(
        S.position.relative,
        S.zIndex(1),
        S.padding(Length.px(4), Length.px(8)),
      ),
      HybridTokens.rule(HybridTokens.node.cls(PaintClass.IsSelected.cssName))(
        S.outline.solid(Length.px(3), Color.keyword(HybridTokens.selectionStroke)),
        S.outlineOffset.px(2),
        S.zIndex(3),
      ),
      HybridTokens.rule(HybridClass.Actor.sel)(
        S.position.absolute,
        S.display.flex,
        S.alignItems.center,
        S.justifyContent.center,
        S.boxSizing.borderBox,
        S.margin.zero,
        S.padding.zero,
        S.cursor.pointer,
        S.border.none,
        S.background(Color.transparent),
        S.fontFamily.inherit,
        S.fontSize.inherit,
        S.lineHeight(1.2),
        S.textAlign.center,
        S.color(HybridTokens.themeColor(ThemeVar.Text, "#333")),
        S.zIndex(2),
      ),
      HybridTokens.rule(HybridClass.Actor.sel.cls(HybridClass.ActorBox.cssName))(
        S.background(HybridTokens.themeColor(ThemeVar.MainBkg, "#ececff")),
        S.border.solid(Length.px(2), HybridTokens.themeColor(ThemeVar.NodeBorder, "#9370db")),
        S.borderRadius.px(8),
      ),
      HybridTokens.rule(HybridClass.Actor.sel.cls(HybridClass.ActorPerson.cssName))(
        S.flexDirection.column,
        S.justifyContent.flexStart,
      ),
      HybridTokens.rule(HybridClass.ActorHead.sel)(
        S.display.block,
        S.width.px(16),
        S.height.px(16),
        S.boxSizing.borderBox,
        S.borderRadius.pct(50),
        S.border.solid(Length.px(2), HybridTokens.themeColor(ThemeVar.NodeBorder, "#9370db")),
        S.flexShrink(0),
      ),
      HybridTokens.rule(HybridClass.ActorStem.sel)(
        S.display.block,
        S.width.px(2),
        S.height.px(18),
        S.marginTop.px(4),
        S.background(HybridTokens.themeColor(ThemeVar.NodeBorder, "#9370db")),
        S.flexShrink(0),
      ),
      HybridTokens.rule(HybridClass.ActorPerson.sel.descendant(HybridClass.ActorLabel.sel))(
        S.display.block,
        S.marginTop.px(4),
        S.lineHeight(Length.px(18)),
      ),
      HybridTokens.rule(HybridClass.Actor.sel.cls(PaintClass.IsSelected.cssName))(
        S.outline.solid(Length.px(3), Color.keyword(HybridTokens.selectionStroke)),
        S.outlineOffset.px(2),
        S.zIndex(3),
      ),
      HybridTokens.rule(HybridTokens.shaped(NodeShape.Round))(
        S.borderRadius.px(15)
      ),
      HybridTokens.rule(HybridTokens.shaped(NodeShape.Stadium))(
        S.borderRadius.px(999)
      ),
      HybridTokens.rule(
        HybridTokens.shaped(NodeShape.Circle).or(HybridTokens.shaped(NodeShape.DoubleCircle))
      )(
        S.borderRadius.pct(50)
      ),
      HybridTokens.rule(HybridTokens.node.cls(NodeShape.Rhombus.wrapperClass))(
        S.padding.zero,
        Selector(Sel.descendant(HybridTokens.shape))(
          S.border.none,
          S.background(HybridTokens.themeColor(ThemeVar.NodeBorder, "#9370db")),
          S.clipPath(HybridTokens.diamond),
        ),
        Selector(Sel.descendant(HybridClass.DiamondFill.sel))(
          S.boxSizing.borderBox,
          S.display.flex,
          S.alignItems.center,
          S.justifyContent.center,
          S.width.pct(86),
          S.height.pct(86),
          S.background(HybridTokens.themeColor(ThemeVar.MainBkg, "#ececff")),
          S.color.inherit,
          S.clipPath(HybridTokens.diamond),
          S.padding(Length.px(2), Length.px(6)),
          S.lineHeight(1.2),
          S.textAlign.center,
        ),
        Selector(Cls(PaintClass.IsSelected.cssName))(
          S.outline.none,
          S.filter(Filter.dropShadow(HybridTokens.diamondShadow), Filter.dropShadow(HybridTokens.diamondShadow)),
          Selector(PseudoClass.hover)(
            S.filter(
              Filter.brightness(1.05),
              Filter.dropShadow(HybridTokens.diamondShadow),
              Filter.dropShadow(HybridTokens.diamondShadow),
            )
          ),
        ),
      ),
      HybridTokens.rule(HybridTokens.node.cls(PaintClass.StartEnd.cssName).descendant(HybridTokens.shape))(
        S.padding.zero,
        S.borderRadius.pct(50),
        S.background(HybridTokens.themeColor(ThemeVar.Line, "#333")),
        S.borderColor(HybridTokens.themeColor(ThemeVar.Line, "#333")),
      ),
      HybridTokens.rule(HybridClass.Note.sel)(
        S.position.absolute,
        S.boxSizing.borderBox,
        S.padding(Length.px(6), Length.px(10)),
        S.border.solid(Length.px(1), HybridTokens.themeColor(ThemeVar.NoteBorder, "#333")),
        S.background(HybridTokens.themeColor(ThemeVar.NoteBg, "#ffc")),
        S.color(HybridTokens.themeColor(ThemeVar.NoteText, "#333")),
        S.fontSize.px(12),
        S.whiteSpace.preWrap,
        S.zIndex(2),
        S.borderRadius.px(3),
      ),
      HybridTokens.rule(HybridClass.Note.sel.cls(PaintClass.IsSelected.cssName))(
        S.outline.solid(Length.px(2), Color.keyword(HybridTokens.selectionStroke))
      ),
      HybridTokens.rule(HybridClass.Tooltip.sel)(
        S.display.none,
        S.position.absolute,
        S.left.pct(50),
        S.bottom("calc(100% + 8px)"),
        S.transform.translateX(Length.pct(-50)),
        S.background(Color.hex("#1c1d1f")),
        S.color(Color.hex("#e8e6dc")),
        S.padding(Length.px(6), Length.px(10)),
        S.borderRadius.px(4),
        S.fontSize.px(12),
        S.whiteSpace.nowrap,
        S.pointerEvents.none,
        S.zIndex(5),
        S.boxShadow(Shadow(Length.px(0), Length.px(2), Length.px(8), Color.rgba(0, 0, 0, 0.25))),
      ),
      HybridTokens.rule(
        HybridTokens.node
          .pseudoClass(PseudoClass.hover)
          .descendant(HybridClass.Tooltip.sel)
          .or(
            HybridTokens.node
              .pseudoClass(PseudoClass.focusVisible)
              .descendant(HybridClass.Tooltip.sel)
          )
      )(
        S.display.block
      ),
      HybridTokens.rule(HybridClass.Controls.sel)(
        S.display.flex,
        S.gap.px(8),
        S.flexWrap.wrap,
        S.marginBottom.px(8),
        S.alignItems.center,
        Selector(Sel.descendant(Elem.button))(
          S.fontFamily.inherit,
          S.fontSize.inherit,
          S.padding(Length.px(4), Length.px(10)),
          S.cursor.pointer,
        ),
      ),
      HybridTokens.rule(HybridClass.WidthLabel.sel)(
        S.fontSize.px(12),
        S.opacity(0.8),
      ),
      // Firefox rejects length/length (`100cqi / 3000px`). tan(atan2(a, b)) is a unitless ratio, and it has
      // to be computed on the scaler: the root is the container, so 100cqi in a property on the root does not
      // resolve to the root's own width. Registered <number> properties keep the ratio typed. transform does not
      // change layout size, so the negative margins are the scaled width and height.
      HybridTokens.rule(HybridClass.Root.sel.cls(HybridClass.Fit.cssName))(
        S.containerType.inlineSize,
        S.width.pct(100),
        S.maxWidth.pct(100),
        S.overflowX.auto,
        Selector(Sel.descendant(HybridClass.Scaler.sel))(
          Declaration(
            HybridVar.Fit.cssName,
            s"max(${HybridVar.ScaleFloor.cssVar}, min(1, tan(atan2(100cqi, ${HybridVar.SceneWidth.cssVar}))))",
          ),
          S.transform(s"scale(${HybridVar.Fit.cssVar})"),
          S.marginRight(s"calc(${HybridVar.SceneWidth.cssVar} * (${HybridVar.Fit.cssVar} - 1))"),
          S.marginBottom(s"calc(${HybridVar.SceneHeight.cssVar} * (${HybridVar.Fit.cssVar} - 1))"),
        ),
      ),
    ):

  def css: String = contributionBlocks.map(_._2).mkString
end HybridChrome
