package mermoid

/** The name a diagram exposes to assistive tech, from `accTitle` and `accDescr`. */
private[mermoid] object AccessibleName:

  def of(title: Option[String], descr: Option[String]): Option[String] =
    val named     = title.map(_.trim).filter(_.nonEmpty)
    val described = descr.map(_.trim).filter(_.nonEmpty)
    (named, described) match
      case (Some(name), Some(description)) => Some(s"$name. $description")
      case (Some(name), None)              => Some(name)
      case (None, Some(description))       => Some(description)
      case (None, None)                    => None
end AccessibleName
