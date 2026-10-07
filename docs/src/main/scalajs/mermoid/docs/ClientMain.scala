package mermoid.docs

import specular.client.SpecularClient
import zio.*

/** Browser entry: remount every `.interactive` example and `.live` illustration on the current page.
  *
  * The one `ZIO.scoped` is the page lifetime the mounters share, so a width source a diagram allocates stays alive.
  * Registry-to-site-map drift is guarded by [[InteractiveContractSpec]] on the JVM.
  */
object ClientMain extends ZIOAppDefault:

  val pages = Vector(Interactive.doc, SpecularIllustrations.doc)

  def run = ZIO.scoped {
    SpecularClient.mountAll(SpecularClient.fromPages(pages*)) *> ZIO.never
  }
end ClientMain
