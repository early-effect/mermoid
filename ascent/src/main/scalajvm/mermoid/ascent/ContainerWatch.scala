package mermoid.ascent

import ascent.ast.Attr
import ascent.squawk.Source
import zio.*

/** The JVM has no container. The width source stays empty and the diagram is painted unconstrained. */
private[ascent] object ContainerWatch:
  def watch(width: Source[Option[Double]]): Attr[Any] =
    Attr.OnMount(_ => width.get.unit)
