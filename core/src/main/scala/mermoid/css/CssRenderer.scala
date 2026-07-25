package mermoid.css

import mermoid.Num

object CssRenderer:

  private[css] def renderValue(value: CssValue): String = value match
    case CssValue.Color(v)            => v
    case CssValue.Length(v, unit)     => s"${Num.format(v)}$unit"
    case CssValue.Number(v)           => Num.format(v)
    case CssValue.Str(v)              => v
    case CssValue.Var(name, None)     => s"var($name)"
    case CssValue.Var(name, Some(fb)) => s"var($name, ${renderValue(fb)})"

  private[css] def resolveValue(value: CssValue, vars: Map[String, CssValue]): CssValue = value match
    case CssValue.Var(name, fallback) =>
      vars.get(name) match
        case Some(resolved) => resolveValue(resolved, vars)
        case None           => fallback.map(fb => resolveValue(fb, vars)).getOrElse(value)
    case other => other

  private[css] def renderDeclaration(decl: CssDeclaration): String =
    s"  ${decl.property}: ${renderValue(decl.value)};"

  private[css] def renderSelector(selector: CssSelector): String = selector match
    case CssSelector.Element(name)             => name
    case CssSelector.Class(name)               => s".$name"
    case CssSelector.Id(name)                  => s"#$name"
    case CssSelector.Compound(parts)           => parts.map(renderSelector).mkString
    case CssSelector.Descendant(parent, child) =>
      s"${renderSelector(parent)} ${renderSelector(child)}"
    case CssSelector.PseudoClass(base, pseudo) =>
      s"${renderSelector(base)}:$pseudo"

  private[css] def renderRule(rule: CssRule): String =
    val sel   = renderSelector(rule.selector)
    val decls = rule.declarations.map(renderDeclaration).mkString("\n")
    s"$sel {\n$decls\n}"

  private[css] def renderVariables(vars: Map[String, CssValue]): String =
    if vars.isEmpty then ""
    else
      val decls = vars.toList
        .sortBy(_._1)
        .map { case (name, value) => s"  $name: ${renderValue(value)};" }
        .mkString("\n")
      s":root {\n$decls\n}"

  def render(stylesheet: Stylesheet, resolveVariables: Boolean = false): String =
    val resolved =
      if resolveVariables then
        stylesheet.copy(
          rules = stylesheet.rules.map { rule =>
            rule.copy(declarations = rule.declarations.map { decl =>
              decl.copy(value = resolveValue(decl.value, stylesheet.variables))
            })
          }
        )
      else stylesheet

    val parts = List(
      renderVariables(resolved.variables)
    ) ++ resolved.rules.map(renderRule)

    parts.filter(_.nonEmpty).mkString("\n")
  end render
end CssRenderer
