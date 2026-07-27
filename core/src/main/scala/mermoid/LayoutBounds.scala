package mermoid

/** Axis-aligned content that must stay inside the SVG viewBox. */
final case class InkBox(x: Double, y: Double, w: Double, h: Double):
  def left: Double   = x
  def right: Double  = x + w
  def top: Double    = y
  def bottom: Double = y + h

object InkBox:
  def fromCenter(cx: Double, cy: Double, w: Double, h: Double): InkBox =
    InkBox(cx - w / 2, cy - h / 2, w, h)

  def fromPoint(p: Point, pad: Double = 0.0): InkBox =
    InkBox(p.x - pad, p.y - pad, pad * 2, pad * 2)

  def fromNode(n: LayoutNode): InkBox =
    InkBox(n.center.x - n.width / 2, n.center.y - n.height / 2, n.width, n.height)
end InkBox

/** Canvas size + optional translation so all ink sits inside padding. */
final case class FittedCanvas(width: Double, height: Double, shiftX: Double, shiftY: Double)

object LayoutBounds:

  /** Shift content so mins clear `padding`, then size the canvas to max + `padding`. */
  def fit(padding: Double, boxes: Iterable[InkBox]): FittedCanvas =
    val xs = boxes.iterator.flatMap(b => Iterator(b.left, b.right)).toList
    val ys = boxes.iterator.flatMap(b => Iterator(b.top, b.bottom)).toList
    if xs.isEmpty || ys.isEmpty then FittedCanvas(padding * 2, padding * 2, 0.0, 0.0)
    else
      val minX   = xs.min
      val minY   = ys.min
      val maxX   = xs.max
      val maxY   = ys.max
      val shiftX = if minX < padding then padding - minX else 0.0
      val shiftY = if minY < padding then padding - minY else 0.0
      FittedCanvas(
        width = maxX + shiftX + padding,
        height = maxY + shiftY + padding,
        shiftX = shiftX,
        shiftY = shiftY,
      )
    end if
  end fit
end LayoutBounds
