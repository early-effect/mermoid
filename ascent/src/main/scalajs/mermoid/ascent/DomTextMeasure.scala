package mermoid.ascent

import mermoid.{Num, TextMeasure}
import scala.scalajs.js

/** Text width from the browser canvas.
  *
  * `svg` and `svgDiagram` stay on the character estimate. An ascent diagram running where `document` exists uses this
  * when the caller left `RenderConfig.textMeasure` empty. A host can still pass it explicitly:
  *
  * ```scala
  * RenderConfig(textMeasure = Some(DomTextMeasure()))
  * ```
  *
  * When `document` is missing, the width falls back to half the font size per character. [[HostMeasure]] does not
  * install this fallback: a missing document keeps the character estimate.
  */
object DomTextMeasure:

  def apply(): TextMeasure = new:
    def width(text: String, fontSizePx: Double, fontFamily: String): Double =
      measured(text, fontSizePx, fontFamily).getOrElse(fallback(text, fontSizePx))

  private def fallback(text: String, fontSizePx: Double): Double =
    text.linesIterator.map(_.length * fontSizePx * 0.5).maxOption.getOrElse(0.0)

  private[ascent] def documentPresent: Boolean =
    js.typeOf(js.Dynamic.global.document) != "undefined"

  private def measured(text: String, fontSizePx: Double, fontFamily: String): Option[Double] =
    if !documentPresent then None
    else
      val canvas  = js.Dynamic.global.document.createElement("canvas")
      val ctx     = canvas.getContext("2d")
      val missing = (ctx: Any) == null || js.typeOf(ctx) == "undefined"
      if missing then None
      else
        ctx.font = s"${Num.format(fontSizePx)}px $fontFamily"
        ctx.measureText(text).width.toString.toDoubleOption
  end measured
end DomTextMeasure
