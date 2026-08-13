package mermoid.css

/** Map SVG paint properties onto HTML box properties so one stylesheet styles both painters.
  *
  * Hosts write `.warn .node-shape { fill; stroke }` for the SVG tree. Hybrid nodes are HTML, so those declarations are
  * duplicated as `background` / `border-color`. A `classDef hot fill:#f00` rule on `.hot` also gets a
  * `.hot .node-shape` twin so the inner shape element picks it up.
  */
object CssHybrid:

  private val Transparent = CssValue.Str("transparent")

  def htmlCompat(sheet: Stylesheet): Stylesheet =
    val expanded = sheet.rules.flatMap { rule =>
      val mapped = withHtmlPaint(rule)
      mapped :: shapeTwin(mapped).toList
    }
    sheet.copy(rules = expanded)

  /** SVG `style` / `classDef` property maps as they land on an HTML shape element. */
  def htmlInline(styles: Map[CssProperty, String]): Map[CssProperty, String] =
    styles.toList.foldLeft(Map.empty[CssProperty, String]) { case (acc, (prop, raw)) =>
      acc ++ ((prop -> raw) :: htmlTwinValues(prop, raw))
    }

  private[css] def withHtmlPaint(rule: CssRule): CssRule =
    val extras = rule.declarations.flatMap(htmlTwinDecl).filterNot { extra =>
      rule.declarations.exists(_.property == extra.property)
    }
    if extras.isEmpty then rule else rule.copy(declarations = rule.declarations ++ extras)

  private def shapeTwin(rule: CssRule): Option[CssRule] =
    rule.selector match
      case CssSelector.Class(name) if name == PaintClass.NodeLabel.cssName =>
        val colorDecls = rule.declarations.collect {
          case CssDeclaration(CssProperty.Fill, v)           => CssDeclaration(CssProperty.Color, v)
          case d @ CssDeclaration(CssProperty.FontFamily, _) => d
          case d @ CssDeclaration(CssProperty.FontSize, _)   => d
          case d @ CssDeclaration(CssProperty.Color, _)      => d
        }
        Option.when(colorDecls.nonEmpty)(
          CssRule(PaintClass.HybridNodeLabel.selector, colorDecls.distinct)
        )
      case CssSelector.Class(name)
          if name != PaintClass.NodeShape.cssName && name != PaintClass.HybridNodeLabel.cssName &&
            rule.declarations.exists(_.property.isHtmlBox) =>
        Some(
          CssRule(
            CssSelector.Descendant(CssSelector.Class(name), PaintClass.NodeShape.selector),
            rule.declarations.filter(_.property.isHtmlBox),
          )
        )
      case _ => None

  private def htmlTwinDecl(d: CssDeclaration): List[CssDeclaration] =
    val value = htmlTwinPaintValue(d.property, d.value)
    d.property.htmlTwins.map(prop => CssDeclaration(prop, value))

  private def htmlTwinValues(prop: CssProperty, raw: String): List[(CssProperty, String)] =
    val value = if isNone(raw) && prop.isSvgPaint then "transparent" else raw
    prop.htmlTwins.map(_ -> value)

  private def htmlTwinPaintValue(prop: CssProperty, value: CssValue): CssValue =
    if prop.isSvgPaint && isNone(value) then Transparent else value

  private def isNone(value: CssValue): Boolean = value match
    case CssValue.Str(v) => isNone(v)
    case _               => false

  private def isNone(raw: String): Boolean =
    raw.trim.equalsIgnoreCase("none")
end CssHybrid
