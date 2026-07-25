package mermoid.css

import fastparse.*
import fastparse.NoWhitespace.*

object CssParser:

  // -- Whitespace & comments ---------------------------------------------------

  private[css] def ws(using P[Any]): P[Unit] =
    P(CharsWhileIn(" \t\r\n", 0))

  private[css] def comment(using P[Any]): P[Unit] =
    P("/*" ~ (!"*/" ~ AnyChar).rep ~ "*/")

  private[css] def wsOrComment(using P[Any]): P[Unit] =
    P((CharsWhileIn(" \t\r\n", 1) | comment).rep)

  // -- CSS values --------------------------------------------------------------

  private def isHexChar(c: Char): Boolean =
    c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  private[css] def hexColor(using P[Any]): P[CssValue] =
    P(("#" ~ CharPred(isHexChar).rep(3)).!)
      .map(CssValue.Color(_))

  private[css] def number(using P[Any]): P[Double] =
    P(("-".? ~ CharsWhileIn("0-9", 1) ~ ("." ~ CharsWhileIn("0-9", 1)).?).!)
      .map(_.toDouble)

  private[css] def lengthUnit(using P[Any]): P[String] =
    P(("px" | "em" | "rem" | "%" | "vh" | "vw" | "pt" | "cm" | "mm" | "in" | "ex" | "ch").!)

  private[css] def lengthValue(using P[Any]): P[CssValue] =
    P(number ~ lengthUnit).map { case (n, u) => CssValue.Length(n, u) }

  private[css] def numberValue(using P[Any]): P[CssValue] =
    P(number).map(CssValue.Number(_))

  private[css] def varRef(using P[Any]): P[CssValue] =
    P("var(" ~ wsOrComment ~ cssVarName ~ (wsOrComment ~ "," ~ wsOrComment ~ cssValue).? ~ wsOrComment ~ ")")
      .map { case (name, fb) => CssValue.Var(name, fb) }

  private[css] def cssVarName(using P[Any]): P[String] =
    P(("--" ~ CharPred(c => c.isLetterOrDigit || c == '-' || c == '_').rep(1)).!)

  private[css] def quotedString(using P[Any]): P[String] =
    P("\"" ~ CharsWhile(_ != '"', 0).! ~ "\"") |
      P("'" ~ CharsWhile(_ != '\'', 0).! ~ "'")

  private[css] def quotedValue(using P[Any]): P[CssValue] =
    P(quotedString).map(CssValue.Str(_))

  private[css] def unquotedValue(using P[Any]): P[CssValue] =
    P(
      CharPred(c => c != ';' && c != '}' && c != '(' && c != ')' && c != ',' && !c.isWhitespace)
        .rep(1)
        .!
    ).map { s =>
      if s.startsWith("#") then CssValue.Color(s)
      else CssValue.Str(s)
    }

  private[css] def cssValue(using P[Any]): P[CssValue] =
    P(varRef | hexColor | lengthValue | quotedValue | numberValue | unquotedValue)

  // Space-separated tokens, joined as a single Str: `1px solid black`
  private[css] def spaceSeparatedValue(using P[Any]): P[CssValue] =
    P(cssValue ~ (CharsWhileIn(" \t", 1) ~ cssValue).rep).map { case (first, rest) =>
      if rest.isEmpty then first
      else
        val parts = (first :: rest.toList).map(CssRenderer.renderValue)
        CssValue.Str(parts.mkString(" "))
    }

  /** A whole declaration value, including comma-separated lists: `4,2`, `ui-monospace, monospace`, `1px solid red`.
    *
    * Comma lists matter beyond convenience — `CssRenderer` emits `stroke-dasharray: 4,2` for the built-in themes, so
    * without this a theme's own output would not parse back.
    */
  private[css] def compositeValue(using P[Any]): P[CssValue] =
    P(spaceSeparatedValue ~ (wsOrComment ~ "," ~ wsOrComment ~ spaceSeparatedValue).rep).map { case (first, rest) =>
      if rest.isEmpty then first
      else CssValue.Str((first :: rest.toList).map(CssRenderer.renderValue).mkString(", "))
    }

  // -- Declarations ------------------------------------------------------------

  private[css] def propertyName(using P[Any]): P[String] =
    P(
      CharPred(c => c.isLetter || c == '-' || c == '_') ~
        CharPred(c => c.isLetterOrDigit || c == '-' || c == '_').rep
    ).!

  private[css] def declaration(using P[Any]): P[CssDeclaration] =
    P(propertyName ~ wsOrComment ~ ":" ~ wsOrComment ~ compositeValue ~ wsOrComment ~ ";".?)
      .map { case (prop, value) => CssDeclaration(prop, value) }

  // -- Selectors ---------------------------------------------------------------

  private[css] def elementSelector(using P[Any]): P[CssSelector] =
    P(CharPred(c => c.isLetter) ~ CharPred(c => c.isLetterOrDigit || c == '-' || c == '_').rep).!.map(
      CssSelector.Element(_)
    )

  private[css] def classSelector(using P[Any]): P[CssSelector] =
    P(
      "." ~ CharPred(c => c.isLetter || c == '_' || c == '-') ~
        CharPred(c => c.isLetterOrDigit || c == '-' || c == '_').rep
    ).!.map(name => CssSelector.Class(name.stripPrefix(".")))

  private[css] def idSelector(using P[Any]): P[CssSelector] =
    P(
      "#" ~ CharPred(c => c.isLetter || c == '_' || c == '-') ~
        CharPred(c => c.isLetterOrDigit || c == '-' || c == '_').rep
    ).!.map(name => CssSelector.Id(name.stripPrefix("#")))

  private[css] def pseudoClassSuffix(using P[Any]): P[String] =
    P(":" ~ CharPred(c => c.isLetter || c == '-').rep(1).!)

  private[css] def simpleSelector(using P[Any]): P[CssSelector] =
    P((classSelector | idSelector | elementSelector) ~ pseudoClassSuffix.?).map {
      case (sel, None)         => sel
      case (sel, Some(pseudo)) => CssSelector.PseudoClass(sel, pseudo)
    }

  private[css] def compoundSelector(using P[Any]): P[CssSelector] =
    P(simpleSelector.rep(1)).map { parts =>
      if parts.length == 1 then parts.head
      else CssSelector.Compound(parts.toList)
    }

  private[css] def descendantPart(using P[Any]): P[CssSelector] =
    P(CharsWhileIn(" \t", 1) ~ &(CharPred(c => c == '.' || c == '#' || c.isLetter)) ~ compoundSelector)

  private[css] def selector(using P[Any]): P[CssSelector] =
    P(compoundSelector ~ descendantPart.rep).map { case (first, rest) =>
      if rest.isEmpty then first
      else rest.foldLeft(first)((parent, child) => CssSelector.Descendant(parent, child))
    }

  // -- Rules -------------------------------------------------------------------

  private[css] def rule(using P[Any]): P[CssRule] =
    P(selector ~ wsOrComment ~ "{" ~ (wsOrComment ~ declaration).rep ~ wsOrComment ~ "}")
      .map { case (sel, decls) => CssRule(sel, decls.toList) }

  // -- :root variables ---------------------------------------------------------

  private[css] def rootBlock(using P[Any]): P[Map[String, CssValue]] =
    P(
      ":root" ~ wsOrComment ~ "{" ~ wsOrComment ~
        (cssVarName ~ wsOrComment ~ ":" ~ wsOrComment ~ compositeValue ~ wsOrComment ~ ";".? ~ wsOrComment).rep ~
        "}"
    )
      .map(_.map { case (name, value) => name -> value }.toMap)

  // -- Stylesheet --------------------------------------------------------------

  private def stylesheetItem(using P[Any]): P[Either[Map[String, CssValue], CssRule]] =
    P(wsOrComment ~ (rootBlock.map(Left(_)) | rule.map(Right(_))))

  private[css] def stylesheet(using P[Any]): P[Stylesheet] =
    P(stylesheetItem.rep ~ wsOrComment ~ End)
      .map { items =>
        val vars  = items.collect { case Left(m) => m }.foldLeft(Map.empty[String, CssValue])(_ ++ _)
        val rules = items.collect { case Right(r) => r }.toList
        Stylesheet(vars, rules)
      }

  // -- Public API --------------------------------------------------------------

  def parse(input: String): Either[String, Stylesheet] =
    fastparse.parse(input, stylesheet(using _)) match
      case Parsed.Success(result, _) => Right(result)
      case f: Parsed.Failure         => Left(f.msg)

  def parseValue(input: String): Either[String, CssValue] =
    fastparse.parse(input, cssValue(using _)) match
      case Parsed.Success(result, _) => Right(result)
      case f: Parsed.Failure         => Left(f.msg)

  private def selectorFull(using P[Any]): P[CssSelector] =
    P(selector ~ End)

  def parseSelector(input: String): Either[String, CssSelector] =
    fastparse.parse(input, selectorFull(using _)) match
      case Parsed.Success(result, _) => Right(result)
      case f: Parsed.Failure         => Left(f.msg)
end CssParser
