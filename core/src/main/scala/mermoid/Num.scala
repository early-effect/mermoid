package mermoid

/** Deterministic number formatting for generated output.
  *
  * `Double.toString` diverges across platforms — the JVM renders `14.0` where Scala.js renders `14` — which would make
  * the same diagram serialize differently on JVM and JS. Everything that writes a number into SVG or CSS goes through
  * here, so both platforms agree and whole numbers stay compact.
  */
private[mermoid] object Num:

  def format(d: Double): String =
    if d.isNaN || d.isInfinite then d.toString
    else if d == Math.floor(d) && Math.abs(d) < 1e15 then d.toLong.toString
    else d.toString

/** `x.f` — the formatted form of a coordinate, for attribute values and interpolated path data. */
extension (d: Double) private[mermoid] def f: String = Num.format(d)
