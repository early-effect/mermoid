package mermoid.css

enum CssValue:
  case Color(value: String)
  case Length(value: Double, unit: String)
  case Number(value: Double)
  case Str(value: String)
  case Var(name: String, fallback: Option[CssValue])

/** CSS properties mermoid emits or maps. Unknown names from parsed CSS stay [[Custom]]. */
enum CssProperty(val cssName: String, val description: String):
  case Fill                 extends CssProperty("fill", "SVG fill; on HTML nodes also background and background-color")
  case Stroke               extends CssProperty("stroke", "SVG stroke; on HTML nodes also border-color")
  case StrokeWidth          extends CssProperty("stroke-width", "SVG stroke width; on HTML nodes also border-width")
  case StrokeDasharray      extends CssProperty("stroke-dasharray", "SVG dashed stroke pattern")
  case FontFamily           extends CssProperty("font-family", "typeface for SVG text and HTML labels")
  case FontSize             extends CssProperty("font-size", "size of SVG text and HTML labels")
  case Color                extends CssProperty("color", "HTML text colour (SVG labels use fill)")
  case Background           extends CssProperty("background", "HTML box fill (SVG uses fill)")
  case BackgroundColor      extends CssProperty("background-color", "HTML box fill (SVG uses fill)")
  case Border               extends CssProperty("border", "HTML shorthand border")
  case BorderColor          extends CssProperty("border-color", "HTML box stroke (SVG uses stroke)")
  case BorderWidth          extends CssProperty("border-width", "HTML box stroke width (SVG uses stroke-width)")
  case Custom(name: String) extends CssProperty(name, "unknown property from parsed CSS")

  /** HTML box properties that carry the same paint as this SVG property. */
  def htmlTwins: List[CssProperty] = this match
    case CssProperty.Fill        => List(CssProperty.Background, CssProperty.BackgroundColor)
    case CssProperty.Stroke      => List(CssProperty.BorderColor)
    case CssProperty.StrokeWidth => List(CssProperty.BorderWidth)
    case _                       => Nil

  def isSvgPaint: Boolean = this match
    case CssProperty.Fill | CssProperty.Stroke => true
    case _                                     => false

  def isHtmlBox: Boolean = this match
    case CssProperty.Background | CssProperty.BackgroundColor | CssProperty.BorderColor | CssProperty.BorderWidth =>
      true
    case _ => false
end CssProperty

object CssProperty:
  private val known: List[CssProperty] = List(
    Fill,
    Stroke,
    StrokeWidth,
    StrokeDasharray,
    FontFamily,
    FontSize,
    Color,
    Background,
    BackgroundColor,
    Border,
    BorderColor,
    BorderWidth,
  )

  def parse(name: String): CssProperty =
    val n = name.trim.toLowerCase
    known.find(_.cssName == n).getOrElse(Custom(n))
end CssProperty

case class CssDeclaration(property: CssProperty, value: CssValue)

object CssDeclaration:
  def apply(name: String, value: CssValue): CssDeclaration =
    new CssDeclaration(CssProperty.parse(name), value)

enum CssSelector:
  case Element(name: String)
  case Class(name: String)
  case Id(name: String)
  case Compound(parts: List[CssSelector])
  case Descendant(parent: CssSelector, child: CssSelector)
  case PseudoClass(base: CssSelector, pseudo: String)

case class CssRule(selector: CssSelector, declarations: List[CssDeclaration])

case class Stylesheet(
    variables: Map[String, CssValue] = Map.empty,
    rules: List[CssRule] = Nil,
):
  def get(v: ThemeVar): Option[CssValue] = variables.get(v.cssName)

object Stylesheet:
  val empty: Stylesheet = Stylesheet()

  def merge(base: Stylesheet, overrides: Stylesheet): Stylesheet =
    Stylesheet(
      variables = base.variables ++ overrides.variables,
      rules = base.rules ++ overrides.rules,
    )
