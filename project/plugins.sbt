// projectMatrix is built into sbt 2.x (sbt-projectmatrix was in-sourced), so no plugin is needed
// for `projectMatrix` / `jvmPlatform` / `jsPlatform`.
addSbtPlugin("org.scala-js"   % "sbt-scalajs"  % "1.22.0")
addSbtPlugin("org.scalameta"  % "sbt-scalafmt" % "2.6.2")
addSbtPlugin("com.eed3si9n"   % "sbt-assembly" % "2.4.1")
addSbtPlugin("com.github.sbt" % "sbt-pgp"      % "2.3.1")
// `runReload` — restarts the forked docs server on rebuild, backing the `docsPreview` alias.
addSbtPlugin("com.jamesward"     % "sbt-reload"    % "0.0.7")
addSbtPlugin("rocks.earlyeffect" % "sbt-dynver-ci" % "0.2.2")
addSbtPlugin("rocks.earlyeffect" % "sbt-specular"  % "0.12.0")
addSbtPlugin("rocks.earlyeffect" % "sbt-zipx"      % "0.3.3")
