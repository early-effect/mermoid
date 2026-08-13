package mermoid.ascent

import mermoid.NodeShape
import mermoid.css.{PaintClass, ThemeVar, WrapperClass}
import zio.test.*

object HybridCssSpec extends ZIOSpecDefault:

  def spec = suite("HybridCss")(
    test("chrome CSS is typed GlobalStyle keyed by HybridClass / PaintClass") {
      val css = HybridChrome.css
      assertTrue(
        css.contains(s".${HybridClass.Diagram.cssName}"),
        css.contains(s".${HybridClass.Node.cssName}"),
        css.contains(s".${PaintClass.NodeShape.cssName}"),
        css.contains(s".${HybridClass.DiamondFill.cssName}"),
        css.contains(s".${NodeShape.Rhombus.wrapperClass}"),
        css.contains(s".${HybridClass.Fit.cssName}"),
        css.contains(s".${WrapperClass.Edge.cssName}.${HybridClass.IsIncident.cssName}"),
        css.contains(ThemeVar.Selection.cssName),
        css.contains(HybridVar.SceneWidth.cssVar),
        !css.contains("&lt;"),
      )
    },
    test("hybrid class names stay the public contract") {
      assertTrue(
        HybridClass.Node.cssName == "mermoid-node",
        HybridClass.Fit.cssName == "mermoid-fit",
        HybridClass.DiamondFill.cssName == "mermoid-node-diamond-fill",
        HybridVar.SceneWidth.cssName == "--mermoid-scene-width",
      )
    },
  )
end HybridCssSpec
