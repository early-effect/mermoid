package mermoid.ascent

import mermoid.RenderConfig

/** The JVM has no canvas. Layout stays on the character estimate unless the caller passed a measure. */
private[ascent] object HostMeasure:
  def refine(config: RenderConfig): RenderConfig = config
