package mermoid.css

/** A `--mermoid-*` custom property emitted on `:root`. */
enum ThemeVar(val cssName: String, val description: String, pick: ThemeColors => CssValue):

  case Primary
      extends ThemeVar(
        "--mermoid-primary",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.primaryColor),
      )
  case PrimaryBorder
      extends ThemeVar(
        "--mermoid-primary-border",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.primaryBorderColor),
      )
  case PrimaryText
      extends ThemeVar(
        "--mermoid-primary-text",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.primaryTextColor),
      )
  case Secondary
      extends ThemeVar(
        "--mermoid-secondary",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.secondaryColor),
      )
  case SecondaryBorder
      extends ThemeVar(
        "--mermoid-secondary-border",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.secondaryBorderColor),
      )
  case SecondaryText
      extends ThemeVar(
        "--mermoid-secondary-text",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.secondaryTextColor),
      )
  case Tertiary
      extends ThemeVar(
        "--mermoid-tertiary",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.tertiaryColor),
      )
  case TertiaryBorder
      extends ThemeVar(
        "--mermoid-tertiary-border",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.tertiaryBorderColor),
      )
  case TertiaryText
      extends ThemeVar(
        "--mermoid-tertiary-text",
        "unused by built-in rules; palette slot for custom stylesheets",
        tc => CssValue.Color(tc.tertiaryTextColor),
      )
  case Line
      extends ThemeVar("--mermoid-line", "edge stroke, arrowheads, `[*]` markers", tc => CssValue.Color(tc.lineColor))
  case Text    extends ThemeVar("--mermoid-text", "node, edge and subgraph labels", tc => CssValue.Color(tc.textColor))
  case MainBkg extends ThemeVar("--mermoid-main-bkg", "node fill", tc => CssValue.Color(tc.mainBkg))
  case NodeBorder
      extends ThemeVar("--mermoid-node-border", "node stroke, subgraph frame", tc => CssValue.Color(tc.nodeBorder))
  case Background
      extends ThemeVar(
        "--mermoid-background",
        "available for a page/container background",
        tc => CssValue.Color(tc.background),
      )
  case FontFamily extends ThemeVar("--mermoid-font-family", "all text", tc => CssValue.Str(tc.fontFamily))
  case FontSize   extends ThemeVar("--mermoid-font-size", "node and subgraph labels", tc => CssValue.Str(tc.fontSize))
  case EdgeLabelBg
      extends ThemeVar(
        "--mermoid-edge-label-bg",
        "the rect behind an edge label",
        tc => CssValue.Color(tc.edgeLabelBackground),
      )
  case NoteBg extends ThemeVar("--mermoid-note-bg", "note fill", tc => CssValue.Color(tc.noteBackground))
  case NoteBorder
      extends ThemeVar("--mermoid-note-border", "note stroke and connector", tc => CssValue.Color(tc.noteBorderColor))
  case NoteText extends ThemeVar("--mermoid-note-text", "note text", tc => CssValue.Color(tc.noteTextColor))
  case Selection
      extends ThemeVar("--mermoid-selection", "hybrid `is-selected` outline", tc => CssValue.Color(tc.lineColor))

  def value(tc: ThemeColors): CssValue = pick(tc)

  def asVar: CssValue.Var = CssValue.Var(cssName, None)

  def cssVar: String = s"var($cssName)"

  def cssVar(fallback: String): String = s"var($cssName, $fallback)"
end ThemeVar

object ThemeVar:
  def parse(name: String): Option[ThemeVar] = values.find(_.cssName == name)

  def markdownTable: String =
    val rows = values.map(v => s"| `${v.cssName}` | ${v.description} |").mkString("\n")
    s"| Variable | Used by |\n|---|---|\n$rows"
