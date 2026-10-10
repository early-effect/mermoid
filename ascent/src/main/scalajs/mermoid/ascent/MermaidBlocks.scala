package mermoid.ascent

import ascent.dom
import ascent.js.AscentApp
import mermoid.Mermaid
import zio.*

/** Replace `<pre class="mermaid">` and `<div class="mermaid">` with a fitted diagram.
  *
  * A `code.language-mermaid` inside a `pre` replaces the `pre`. The caller owns the scope: closing it unmounts every
  * diagram this call inserted. Pass `dom.document` to scan the page.
  */
object MermaidBlocks:

  def install(root: dom.Document): URIO[Scope, Unit] =
    mountAll(root.querySelectorAll(selector))

  def install(root: dom.Element): URIO[Scope, Unit] =
    mountAll(root.querySelectorAll(selector))

  private val selector = "pre.mermaid, div.mermaid, pre > code.language-mermaid"

  private def mountAll(list: dom.NodeList): URIO[Scope, Unit] =
    val hosts = elements(list).map(blockHost).distinct
    ZIO.foreachDiscard(hosts)(mountOne)

  private def elements(list: dom.NodeList): List[dom.Element] =
    List.tabulate(list.length)(list.itemOrNull).flatMap {
      case el: dom.Element => Some(el)
      case _               => None
    }

  private def blockHost(node: dom.Element): dom.Element =
    node.parentElement match
      case Some(parent) if node.localName == "code" && parent.localName == "pre" => parent
      case _                                                                     => node

  private def mountOne(block: dom.Element): URIO[Scope, Unit] =
    (block.textContent, block.parentElement) match
      case (Some(text), Some(parent)) =>
        Mermaid.from(text) match
          case Left(_)        => ZIO.unit
          case Right(diagram) =>
            for
              ui   <- MermoidAscent.diagramFitting(diagram)
              host <- ZIO.succeed(dom.document.createElement("div"))
              _    <- ZIO.succeed(parent.replaceChild(host, block))
              subs <- AscentApp.mount(ui, host)
              _    <- ZIO.addFinalizer(subs.cancelAll)
            yield ()
      case _ => ZIO.unit
end MermaidBlocks
