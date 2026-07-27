package mermoid

import mermoid.css.*

case class RenderConfig(
    layout: LayoutConfig = LayoutConfig(),
    theme: ThemeName = ThemeName.Default,
    customStylesheet: Option[Stylesheet] = None,
    resolveVariables: Boolean = true,
    responsive: ResponsiveConfig = ResponsiveConfig(),
)

object RenderConfig:
  /** Binary-compatible with callers compiled against `RenderConfig` before [[ResponsiveConfig]]. */
  def apply(
      layout: LayoutConfig,
      theme: ThemeName,
      customStylesheet: Option[Stylesheet],
      resolveVariables: Boolean,
  ): RenderConfig =
    new RenderConfig(layout, theme, customStylesheet, resolveVariables, ResponsiveConfig())

  def themeColors(config: RenderConfig): ThemeColors = Theme.colors(config.theme)

  def resolvedStylesheet(config: RenderConfig): Stylesheet =
    val base = Theme.toStylesheet(config.theme)
    config.customStylesheet match
      case Some(custom) => Stylesheet.merge(base, custom)
      case None         => base
end RenderConfig
