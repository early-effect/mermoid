package mermoid.docs

import specular.ziotest.DocSpecSuite

/** JVM test discovery for the SpecularIllustrations DocSpec (shared with docsJS ClientMain). */
object SpecularIllustrationsSuite extends DocSpecSuite:
  def doc = SpecularIllustrations.doc
