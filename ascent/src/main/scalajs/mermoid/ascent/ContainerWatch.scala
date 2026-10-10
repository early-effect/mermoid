package mermoid.ascent

import ascent.ast.Attr
import ascent.dom
import ascent.squawk.Source
import zio.*

/** After the root is in the document, its content-box width drives layout.
  *
  * The observer callback is a DOM entry, the same boundary `ascent.js.Dom.listen` crosses to run an effect.
  */
private[ascent] object ContainerWatch:
  def watch(width: Source[Option[Double]]): Attr[Any] =
    Attr.OnMountScoped {
      case el: dom.Element => observe(el, width)
      case _               => ZIO.unit
    }

  private def observe(el: dom.Element, width: Source[Option[Double]]): URIO[Scope, Unit] =
    for
      runtime <- ZIO.runtime[Any]
      _       <- ZIO.acquireRelease(
        ZIO.succeed {
          val observer = new dom.ResizeObserver((_, _) =>
            val px = el.getBoundingClientRect().width
            Unsafe.unsafe { implicit unsafe =>
              val _ = runtime.unsafe.runOrFork(publish(width, px))
            }
          )
          observer.observe(el)
          observer
        }
      )(observer => ZIO.succeed(observer.disconnect()))
      _ <- publish(width, el.getBoundingClientRect().width)
    yield ()

  private def publish(width: Source[Option[Double]], px: Double): UIO[Unit] =
    val rounded = Math.rint(px)
    if rounded <= 0 then ZIO.unit
    else
      width.get.flatMap {
        case Some(current) if Math.abs(current - rounded) < 1 => ZIO.unit
        case _                                                => width.set(Some(rounded))
      }
end ContainerWatch
