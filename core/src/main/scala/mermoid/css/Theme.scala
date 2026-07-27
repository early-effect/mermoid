package mermoid.css

case class ThemeColors(
    primaryColor: String,
    primaryBorderColor: String,
    primaryTextColor: String,
    secondaryColor: String,
    secondaryBorderColor: String,
    secondaryTextColor: String,
    tertiaryColor: String,
    tertiaryBorderColor: String,
    tertiaryTextColor: String,
    lineColor: String,
    textColor: String,
    mainBkg: String,
    nodeBorder: String,
    background: String,
    fontFamily: String,
    fontSize: String,
    edgeLabelBackground: String,
    noteBackground: String,
    noteBorderColor: String,
    noteTextColor: String,
)

enum ThemeName:
  case Default, Dark, Forest, Neutral

object Theme:

  def colors(name: ThemeName): ThemeColors = name match
    case ThemeName.Default => defaultColors
    case ThemeName.Dark    => darkColors
    case ThemeName.Forest  => forestColors
    case ThemeName.Neutral => neutralColors

  def toStylesheet(name: ThemeName): Stylesheet =
    toStylesheet(colors(name))

  def toStylesheet(tc: ThemeColors): Stylesheet =
    val vars: Map[String, CssValue] = Map(
      "--mermoid-primary"          -> CssValue.Color(tc.primaryColor),
      "--mermoid-primary-border"   -> CssValue.Color(tc.primaryBorderColor),
      "--mermoid-primary-text"     -> CssValue.Color(tc.primaryTextColor),
      "--mermoid-secondary"        -> CssValue.Color(tc.secondaryColor),
      "--mermoid-secondary-border" -> CssValue.Color(tc.secondaryBorderColor),
      "--mermoid-secondary-text"   -> CssValue.Color(tc.secondaryTextColor),
      "--mermoid-tertiary"         -> CssValue.Color(tc.tertiaryColor),
      "--mermoid-tertiary-border"  -> CssValue.Color(tc.tertiaryBorderColor),
      "--mermoid-tertiary-text"    -> CssValue.Color(tc.tertiaryTextColor),
      "--mermoid-line"             -> CssValue.Color(tc.lineColor),
      "--mermoid-text"             -> CssValue.Color(tc.textColor),
      "--mermoid-main-bkg"         -> CssValue.Color(tc.mainBkg),
      "--mermoid-node-border"      -> CssValue.Color(tc.nodeBorder),
      "--mermoid-background"       -> CssValue.Color(tc.background),
      "--mermoid-font-family"      -> CssValue.Str(tc.fontFamily),
      "--mermoid-font-size"        -> CssValue.Str(tc.fontSize),
      "--mermoid-edge-label-bg"    -> CssValue.Color(tc.edgeLabelBackground),
      "--mermoid-note-bg"          -> CssValue.Color(tc.noteBackground),
      "--mermoid-note-border"      -> CssValue.Color(tc.noteBorderColor),
      "--mermoid-note-text"        -> CssValue.Color(tc.noteTextColor),
    )

    val rules = List(
      CssRule(
        CssSelector.Class("diagram-bg"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-background", None)),
          CssDeclaration("stroke", CssValue.Str("none")),
        ),
      ),
      CssRule(
        CssSelector.Class("node-shape"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-main-bkg", None)),
          CssDeclaration("stroke", CssValue.Var("--mermoid-node-border", None)),
          CssDeclaration("stroke-width", CssValue.Str("2")),
        ),
      ),
      CssRule(
        CssSelector.Class("node-label"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-text", None)),
          CssDeclaration("font-family", CssValue.Var("--mermoid-font-family", None)),
          CssDeclaration("font-size", CssValue.Var("--mermoid-font-size", None)),
        ),
      ),
      CssRule(
        CssSelector.Class("edge-line"),
        List(
          CssDeclaration("stroke", CssValue.Var("--mermoid-line", None)),
          CssDeclaration("stroke-width", CssValue.Str("2")),
        ),
      ),
      CssRule(
        CssSelector.Class("edge-label"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-text", None)),
          CssDeclaration("font-family", CssValue.Var("--mermoid-font-family", None)),
          CssDeclaration("font-size", CssValue.Str("12px")),
        ),
      ),
      CssRule(
        CssSelector.Class("edge-label-bg"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-edge-label-bg", None))
        ),
      ),
      CssRule(
        CssSelector.Class("note-rect"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-note-bg", None)),
          CssDeclaration("stroke", CssValue.Var("--mermoid-note-border", None)),
          CssDeclaration("stroke-width", CssValue.Str("1")),
          CssDeclaration("stroke-dasharray", CssValue.Str("4,2")),
        ),
      ),
      CssRule(
        CssSelector.Class("note-text"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-note-text", None)),
          CssDeclaration("font-family", CssValue.Var("--mermoid-font-family", None)),
          CssDeclaration("font-size", CssValue.Str("12px")),
        ),
      ),
      CssRule(
        CssSelector.Class("note-connector"),
        List(
          CssDeclaration("stroke", CssValue.Var("--mermoid-note-border", None)),
          CssDeclaration("stroke-width", CssValue.Str("1")),
          CssDeclaration("stroke-dasharray", CssValue.Str("4,2")),
        ),
      ),
      CssRule(
        CssSelector.Class("subgraph-rect"),
        List(
          CssDeclaration("fill", CssValue.Str("none")),
          CssDeclaration("stroke", CssValue.Var("--mermoid-node-border", None)),
          CssDeclaration("stroke-width", CssValue.Str("1")),
          CssDeclaration("stroke-dasharray", CssValue.Str("6,3")),
        ),
      ),
      CssRule(
        CssSelector.Class("subgraph-label"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-text", None)),
          CssDeclaration("font-family", CssValue.Var("--mermoid-font-family", None)),
          CssDeclaration("font-size", CssValue.Var("--mermoid-font-size", None)),
        ),
      ),
      CssRule(
        CssSelector.Descendant(CssSelector.Class("edge-thick"), CssSelector.Class("edge-line")),
        List(
          CssDeclaration("stroke-width", CssValue.Str("3"))
        ),
      ),
      CssRule(
        CssSelector.Descendant(CssSelector.Class("edge-dotted"), CssSelector.Class("edge-line")),
        List(
          CssDeclaration("stroke-dasharray", CssValue.Str("5,5"))
        ),
      ),
      CssRule(
        CssSelector.Descendant(CssSelector.Class("edge-dotted-open"), CssSelector.Class("edge-line")),
        List(
          CssDeclaration("stroke-dasharray", CssValue.Str("5,5"))
        ),
      ),
      CssRule(
        CssSelector.Class("arrowhead"),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-line", None))
        ),
      ),
      CssRule(
        CssSelector.Descendant(CssSelector.Class("start-end"), CssSelector.Class("node-shape")),
        List(
          CssDeclaration("fill", CssValue.Var("--mermoid-line", None)),
          CssDeclaration("stroke", CssValue.Var("--mermoid-line", None)),
        ),
      ),
    )

    Stylesheet(variables = vars, rules = rules)
  end toStylesheet

  // -- Built-in theme color palettes ------------------------------------------

  private val defaultColors = ThemeColors(
    primaryColor = "#1f77b4",
    primaryBorderColor = "#1a5a8a",
    primaryTextColor = "#333",
    secondaryColor = "#ffffde",
    secondaryBorderColor = "#aaaa33",
    secondaryTextColor = "#333",
    tertiaryColor = "#f4f4f4",
    tertiaryBorderColor = "#cccccc",
    tertiaryTextColor = "#333",
    lineColor = "#333",
    textColor = "#333",
    mainBkg = "#ECECFF",
    nodeBorder = "#9370DB",
    background = "#ffffff",
    fontFamily = "sans-serif",
    fontSize = "14px",
    edgeLabelBackground = "#ffffff",
    noteBackground = "#ffffcc",
    noteBorderColor = "#333333",
    noteTextColor = "#333333",
  )

  private val darkColors = ThemeColors(
    primaryColor = "#1f2020",
    primaryBorderColor = "#81B1DB",
    primaryTextColor = "#e0e0e0",
    secondaryColor = "#3b3b4f",
    secondaryBorderColor = "#6b6b8d",
    secondaryTextColor = "#e0e0e0",
    tertiaryColor = "#2c2c3e",
    tertiaryBorderColor = "#555577",
    tertiaryTextColor = "#e0e0e0",
    lineColor = "#d0d0d0",
    textColor = "#e0e0e0",
    mainBkg = "#1f2020",
    nodeBorder = "#81B1DB",
    background = "#0d1117",
    fontFamily = "sans-serif",
    fontSize = "14px",
    edgeLabelBackground = "#1f2020",
    noteBackground = "#3b3b4f",
    noteBorderColor = "#81B1DB",
    noteTextColor = "#e0e0e0",
  )

  private val forestColors = ThemeColors(
    primaryColor = "#cde498",
    primaryBorderColor = "#6eaa49",
    primaryTextColor = "#333",
    secondaryColor = "#cdffb2",
    secondaryBorderColor = "#6eaa49",
    secondaryTextColor = "#333",
    tertiaryColor = "#f4f4de",
    tertiaryBorderColor = "#a2a22a",
    tertiaryTextColor = "#333",
    lineColor = "#333",
    textColor = "#333",
    mainBkg = "#cde498",
    nodeBorder = "#6eaa49",
    background = "#ffffff",
    fontFamily = "sans-serif",
    fontSize = "14px",
    edgeLabelBackground = "#ffffff",
    noteBackground = "#ffffcc",
    noteBorderColor = "#6eaa49",
    noteTextColor = "#333333",
  )

  private val neutralColors = ThemeColors(
    primaryColor = "#f0f0f0",
    primaryBorderColor = "#aaaaaa",
    primaryTextColor = "#333",
    secondaryColor = "#e8e8e8",
    secondaryBorderColor = "#999999",
    secondaryTextColor = "#333",
    tertiaryColor = "#f5f5f5",
    tertiaryBorderColor = "#bbbbbb",
    tertiaryTextColor = "#333",
    lineColor = "#666666",
    textColor = "#333333",
    mainBkg = "#f0f0f0",
    nodeBorder = "#aaaaaa",
    background = "#ffffff",
    fontFamily = "sans-serif",
    fontSize = "14px",
    edgeLabelBackground = "#ffffff",
    noteBackground = "#f5f5f5",
    noteBorderColor = "#aaaaaa",
    noteTextColor = "#333333",
  )
end Theme
