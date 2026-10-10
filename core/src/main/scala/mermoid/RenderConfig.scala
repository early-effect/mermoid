package mermoid

import mermoid.css.*

case class RenderConfig(
    layout: LayoutConfig = LayoutConfig(),
    theme: ThemeName = ThemeName.Default,
    customStylesheet: Option[Stylesheet] = None,
    resolveVariables: Boolean = true,
    responsive: ResponsiveConfig = ResponsiveConfig(),
    sequence: SequenceConfig = SequenceConfig(),
    /** `None` uses the character estimate. A host with a real font passes a measure. */
    textMeasure: Option[TextMeasure] = None,
)

object RenderConfig:
  def themeColors(config: RenderConfig): ThemeColors = Theme.colors(config.theme)

  def resolvedStylesheet(config: RenderConfig): Stylesheet =
    val base = Theme.toStylesheet(config.theme)
    config.customStylesheet match
      case Some(custom) => Stylesheet.merge(base, custom)
      case None         => base
end RenderConfig
