package mermoid

/** The theme name inside `%%{init: ...}%%`. Other init keys are ignored.
  *
  * Applied only when [[RenderConfig.theme]] is still [[mermoid.css.ThemeName.Default]]. An explicit theme wins.
  * [[SvgRenderer.render]] stays source-free: callers that have the Mermaid text apply this first.
  */
object InitDirective:

  def theme(source: String): Option[css.ThemeName] =
    bodies(source).flatMap(themeName).headOption

  def apply(source: String, config: RenderConfig): RenderConfig =
    if config.theme != css.ThemeName.Default then config
    else theme(source).fold(config)(name => config.copy(theme = name))

  private def bodies(source: String): List[String] =
    def go(from: Int, acc: List[String]): List[String] =
      val start = source.indexOf("%%{", from)
      if start < 0 then acc.reverse
      else
        val end = source.indexOf("}%%", start + 3)
        if end < 0 then acc.reverse
        else go(end + 3, source.substring(start + 3, end) :: acc)
    go(0, Nil)

  private def themeName(body: String): Option[css.ThemeName] =
    val lower = body.toLowerCase
    val at    = wordAt(lower, "theme")
    if at < 0 then None
    else
      val after = lower.substring(at + "theme".length).dropWhile(c => !c.isLetter)
      val word  = after.takeWhile(_.isLetter)
      word match
        case "default" => Some(css.ThemeName.Default)
        case "dark"    => Some(css.ThemeName.Dark)
        case "forest"  => Some(css.ThemeName.Forest)
        case "neutral" => Some(css.ThemeName.Neutral)
        case _         => None
    end if
  end themeName

  /** Index of `word` as a whole word, or -1. */
  private def wordAt(text: String, word: String): Int =
    def go(from: Int): Int =
      val at = text.indexOf(word, from)
      if at < 0 then -1
      else
        val before = if at == 0 then true else !text.charAt(at - 1).isLetter
        val after  = at + word.length
        val next   = if after >= text.length then true else !text.charAt(after).isLetter
        if before && next then at else go(after)
    go(0)
  end wordAt
end InitDirective
