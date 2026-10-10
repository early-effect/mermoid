package mermoid.ascent

import mermoid.{Num, TextMeasure}
import scala.scalajs.js

/** Opt-in text width from the browser canvas. The default measure stays the character estimate, so a JVM SVG and a
  * Scala.js SVG of the same source stay byte-identical until a host asks for this.
  *
  * Pass it on the config and lay the diagram out again after mount:
  *
  * ```scala
  * RenderConfig(textMeasure = Some(DomTextMeasure()))
  * ```
  *
  * When `document` is missing, the width falls back to half the font size per character.
  */
object DomTextMeasure:

  def apply(): TextMeasure = new:
    def width(text: String, fontSizePx: Double, fontFamily: String): Double =
      measured(text, fontSizePx, fontFamily).getOrElse(fallback(text, fontSizePx))

  private def fallback(text: String, fontSizePx: Double): Double =
    text.linesIterator.map(_.length * fontSizePx * 0.5).maxOption.getOrElse(0.0)

  private def measured(text: String, fontSizePx: Double, fontFamily: String): Option[Double] =
    val global = js.Dynamic.global
    if js.typeOf(global.document) == "undefined" then None
    else
      val canvas = global.document.createElement("canvas")
      val ctx    = canvas.getContext("2d")
      val missing = (ctx: Any) == null || js.typeOf(ctx) == "undefined"
      if missing then None
      else
        ctx.font = s"${Num.format(fontSizePx)}px $fontFamily"
        ctx.measureText(text).width.toString.toDoubleOption
end DomTextMeasure
