package mermoid.css

import mermoid.EdgeStyle

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
    val vars = ThemeVar.values.map(v => v.cssName -> v.value(tc)).toMap

    val rules = List(
      CssRule(
        PaintClass.DiagramBg.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.Background.asVar),
          CssDeclaration(CssProperty.Stroke, CssValue.Str("none")),
        ),
      ),
      CssRule(
        PaintClass.NodeShape.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.MainBkg.asVar),
          CssDeclaration(CssProperty.Stroke, ThemeVar.NodeBorder.asVar),
          CssDeclaration(CssProperty.StrokeWidth, CssValue.Str("2")),
        ),
      ),
      CssRule(
        PaintClass.NodeLabel.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.Text.asVar),
          CssDeclaration(CssProperty.FontFamily, ThemeVar.FontFamily.asVar),
          CssDeclaration(CssProperty.FontSize, ThemeVar.FontSize.asVar),
        ),
      ),
      CssRule(
        PaintClass.EdgeLine.selector,
        List(
          CssDeclaration(CssProperty.Stroke, ThemeVar.Line.asVar),
          CssDeclaration(CssProperty.StrokeWidth, CssValue.Str("2")),
        ),
      ),
      CssRule(
        PaintClass.EdgeLabel.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.Text.asVar),
          CssDeclaration(CssProperty.FontFamily, ThemeVar.FontFamily.asVar),
          CssDeclaration(CssProperty.FontSize, CssValue.Str("12px")),
        ),
      ),
      CssRule(
        PaintClass.EdgeLabelBg.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.EdgeLabelBg.asVar)
        ),
      ),
      CssRule(
        PaintClass.NoteRect.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.NoteBg.asVar),
          CssDeclaration(CssProperty.Stroke, ThemeVar.NoteBorder.asVar),
          CssDeclaration(CssProperty.StrokeWidth, CssValue.Str("1")),
          CssDeclaration(CssProperty.StrokeDasharray, CssValue.Str("4,2")),
        ),
      ),
      CssRule(
        PaintClass.NoteText.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.NoteText.asVar),
          CssDeclaration(CssProperty.FontFamily, ThemeVar.FontFamily.asVar),
          CssDeclaration(CssProperty.FontSize, CssValue.Str("12px")),
        ),
      ),
      CssRule(
        PaintClass.NoteConnector.selector,
        List(
          CssDeclaration(CssProperty.Stroke, ThemeVar.NoteBorder.asVar),
          CssDeclaration(CssProperty.StrokeWidth, CssValue.Str("1")),
          CssDeclaration(CssProperty.StrokeDasharray, CssValue.Str("4,2")),
        ),
      ),
      CssRule(
        PaintClass.SubgraphRect.selector,
        List(
          CssDeclaration(CssProperty.Fill, CssValue.Str("none")),
          CssDeclaration(CssProperty.Stroke, ThemeVar.NodeBorder.asVar),
          CssDeclaration(CssProperty.StrokeWidth, CssValue.Str("1")),
          CssDeclaration(CssProperty.StrokeDasharray, CssValue.Str("6,3")),
        ),
      ),
      CssRule(
        PaintClass.SubgraphLabel.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.Text.asVar),
          CssDeclaration(CssProperty.FontFamily, ThemeVar.FontFamily.asVar),
          CssDeclaration(CssProperty.FontSize, ThemeVar.FontSize.asVar),
        ),
      ),
      CssRule(
        CssSelector.Descendant(CssSelector.Class(EdgeStyle.Thick.wrapperClass), PaintClass.EdgeLine.selector),
        List(
          CssDeclaration(CssProperty.StrokeWidth, CssValue.Str("3"))
        ),
      ),
      CssRule(
        CssSelector.Descendant(CssSelector.Class(EdgeStyle.Dotted.wrapperClass), PaintClass.EdgeLine.selector),
        List(
          CssDeclaration(CssProperty.StrokeDasharray, CssValue.Str("5,5"))
        ),
      ),
      CssRule(
        CssSelector.Descendant(CssSelector.Class(EdgeStyle.DottedOpen.wrapperClass), PaintClass.EdgeLine.selector),
        List(
          CssDeclaration(CssProperty.StrokeDasharray, CssValue.Str("5,5"))
        ),
      ),
      CssRule(
        PaintClass.Arrowhead.selector,
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.Line.asVar)
        ),
      ),
      CssRule(
        CssSelector.Descendant(PaintClass.StartEnd.selector, PaintClass.NodeShape.selector),
        List(
          CssDeclaration(CssProperty.Fill, ThemeVar.Line.asVar),
          CssDeclaration(CssProperty.Stroke, ThemeVar.Line.asVar),
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
