package mermoid

/** Width of a run of text. The default counts characters so the JVM and Scala.js agree.
  *
  * A host that has the real font passes its own measure on [[RenderConfig]]. Core does not measure glyphs itself.
  */
trait TextMeasure:
  def width(text: String, fontSizePx: Double, fontFamily: String): Double

object TextMeasure:

  /** `charWidth` is [[LayoutConfig.charWidthEstimate]]. Font size and family are ignored. */
  def estimate(charWidth: Double): TextMeasure = new:
    def width(text: String, fontSizePx: Double, fontFamily: String): Double =
      text.linesIterator.map(_.length * charWidth).maxOption.getOrElse(0.0)

  def fromConfig(config: LayoutConfig): TextMeasure =
    estimate(config.charWidthEstimate)

  def resolve(config: RenderConfig): TextMeasure =
    config.textMeasure.getOrElse(fromConfig(config.layout))
end TextMeasure
