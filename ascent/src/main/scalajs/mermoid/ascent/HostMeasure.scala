package mermoid.ascent

import mermoid.RenderConfig

/** In a browser, an ascent diagram that was not given a measure lays out from the canvas.
  *
  * A missing `document` (tests, a non-browser JS host) keeps the character estimate, so the bytes match the JVM. `svg`
  * and `svgDiagram` do not call this. A measure the caller passed is kept.
  */
private[ascent] object HostMeasure:
  def refine(config: RenderConfig): RenderConfig =
    config.textMeasure match
      case Some(_) => config
      case None    =>
        if DomTextMeasure.documentPresent then config.copy(textMeasure = Some(DomTextMeasure()))
        else config
