package mermoid

/** Paint-ready diagram. Ranked diagrams (flowchart, state, class, ER) and sequence diagrams are different geometry. */
enum Scene:
  case Ranked(scene: DiagramScene)
  case Sequence(scene: SequenceScene)

  def width: Double = this match
    case Ranked(scene)   => scene.width
    case Sequence(scene) => scene.width

  def height: Double = this match
    case Ranked(scene)   => scene.height
    case Sequence(scene) => scene.height

  def config: RenderConfig = this match
    case Ranked(scene)   => scene.config
    case Sequence(scene) => scene.config

  /** Uniform scale so the scene fits `maxWidth`.
    *
    * `ContainerFit.Off` and a scene that is already narrower than `maxWidth` stay at 1. `ToWidth` never goes below its
    * floor, so a very wide scene scrolls instead of shrinking labels without limit.
    */
  def fitScale(maxWidth: Double): Double =
    config.responsive.fit match
      case ContainerFit.Off            => 1.0
      case ContainerFit.ToWidth(floor) =>
        val w = width
        if w <= 0 || w <= maxWidth then 1.0
        else Math.max(floor, Math.min(1.0, maxWidth / w))
end Scene
