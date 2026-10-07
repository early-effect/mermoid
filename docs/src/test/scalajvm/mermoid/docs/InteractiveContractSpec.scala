package mermoid.docs

import specular.*
import zio.test.*

/** The JS client remounts every key the site declares, and the site declares no hand-bound DOM keys. */
object InteractiveContractSpec extends ZIOSpecDefault:

  // Mirrors ClientMain.pages, which lives in the JS-only source tree.
  private val clientPages = Vector(Interactive.doc, SpecularIllustrations.doc)

  def spec = suite("Interactive contract")(
    test("the site's mount keys are exactly the client pages' keys") {
      val fromSite   = DocMounts.keys(BuildSite.pages*)
      val fromClient = DocMounts.keys(clientPages*)
      assertTrue(fromSite.nonEmpty, fromSite == fromClient)
    },
    test("no page needs a hand-registered DOM mounter") {
      assertTrue(DocMounts.domKeys(BuildSite.pages*).isEmpty)
    },
  )
end InteractiveContractSpec
