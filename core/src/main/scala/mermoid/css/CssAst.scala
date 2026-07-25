package mermoid.css

enum CssValue:
  case Color(value: String)
  case Length(value: Double, unit: String)
  case Number(value: Double)
  case Str(value: String)
  case Var(name: String, fallback: Option[CssValue])

case class CssDeclaration(property: String, value: CssValue)

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
)

object Stylesheet:
  val empty: Stylesheet = Stylesheet()

  def merge(base: Stylesheet, overrides: Stylesheet): Stylesheet =
    Stylesheet(
      variables = base.variables ++ overrides.variables,
      rules = base.rules ++ overrides.rules,
    )
