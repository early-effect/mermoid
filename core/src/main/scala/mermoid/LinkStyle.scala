package mermoid

/** `linkStyle` paint, copied onto edges in source order before layout reorders them. */
private[mermoid] object LinkStyle:

  def decorateFlow(stmts: List[FlowStatement]): List[FlowStatement] =
    val paints = paintsFor(countFlow(stmts), directivesFlow(stmts))
    rewriteFlow(stmts, paints.toList)._1

  def decorateState(stmts: List[StateStatement]): List[StateStatement] =
    val paints = paintsFor(countState(stmts), directivesState(stmts))
    rewriteState(stmts, paints.toList)._1

  def flowAccess(stmts: List[FlowStatement]): (Option[String], Option[String]) =
    def walk(
        stmts: List[FlowStatement],
        title: Option[String],
        descr: Option[String],
    ): (Option[String], Option[String]) =
      stmts.foldLeft((title, descr)) { case ((title, descr), stmt) =>
        stmt match
          case FlowStatement.AccTitle(text)             => (Some(text), descr)
          case FlowStatement.AccDescr(text)             => (title, Some(text))
          case FlowStatement.SubgraphSt(_, _, _, inner) => walk(inner, title, descr)
          case _                                        => (title, descr)
      }
    walk(stmts, None, None)
  end flowAccess

  private def paintsFor(
      count: Int,
      directives: List[(Option[Int], Map[css.CssProperty, String])],
  ): Vector[Map[css.CssProperty, String]] =
    val defaults =
      directives.collect { case (None, styles) => styles }.foldLeft(Map.empty[css.CssProperty, String])(_ ++ _)
    val indexed = directives.foldLeft(Map.empty[Int, Map[css.CssProperty, String]]) {
      case (acc, (Some(index), styles)) => acc.updated(index, acc.getOrElse(index, Map.empty) ++ styles)
      case (acc, (None, _))             => acc
    }
    Vector.tabulate(count)(index => defaults ++ indexed.getOrElse(index, Map.empty))
  end paintsFor

  private def directivesFlow(stmts: List[FlowStatement]): List[(Option[Int], Map[css.CssProperty, String])] =
    stmts.flatMap {
      case FlowStatement.LinkStyleSt(index, styles) => List((index, styles))
      case FlowStatement.SubgraphSt(_, _, _, inner) => directivesFlow(inner)
      case _                                        => Nil
    }

  private def countFlow(stmts: List[FlowStatement]): Int =
    stmts.map {
      case _: FlowStatement.EdgeSt                  => 1
      case FlowStatement.SubgraphSt(_, _, _, inner) => countFlow(inner)
      case _                                        => 0
    }.sum

  private def rewriteFlow(
      stmts: List[FlowStatement],
      paints: List[Map[css.CssProperty, String]],
  ): (List[FlowStatement], List[Map[css.CssProperty, String]]) =
    stmts.foldLeft((List.empty[FlowStatement], paints)) { case ((acc, rest), stmt) =>
      stmt match
        case FlowStatement.EdgeSt(edge, from, to) =>
          rest match
            case paint :: tail =>
              (acc :+ FlowStatement.EdgeSt(edge.copy(inline = paint), from, to), tail)
            case Nil => (acc :+ stmt, Nil)
        case FlowStatement.SubgraphSt(id, label, dir, inner) =>
          val (rewritten, left) = rewriteFlow(inner, rest)
          (acc :+ FlowStatement.SubgraphSt(id, label, dir, rewritten), left)
        case other => (acc :+ other, rest)
    }

  private def directivesState(stmts: List[StateStatement]): List[(Option[Int], Map[css.CssProperty, String])] =
    stmts.flatMap {
      case StateStatement.LinkStyleSt(index, styles) => List((index, styles))
      case StateStatement.Composite(_, _, regions)   => regions.flatMap(directivesState)
      case _                                         => Nil
    }

  private def countState(stmts: List[StateStatement]): Int =
    stmts.map {
      case _: StateStatement.TransitionSt          => 1
      case StateStatement.Composite(_, _, regions) => regions.map(countState).sum
      case _                                       => 0
    }.sum

  private def rewriteState(
      stmts: List[StateStatement],
      paints: List[Map[css.CssProperty, String]],
  ): (List[StateStatement], List[Map[css.CssProperty, String]]) =
    stmts.foldLeft((List.empty[StateStatement], paints)) { case ((acc, rest), stmt) =>
      stmt match
        case StateStatement.TransitionSt(transition) =>
          rest match
            case paint :: tail =>
              (acc :+ StateStatement.TransitionSt(transition.copy(inline = paint)), tail)
            case Nil => (acc :+ stmt, Nil)
        case StateStatement.Composite(id, dir, regions) =>
          val (rewritten, left) = regions.foldLeft((List.empty[List[StateStatement]], rest)) {
            case ((regions, paints), region) =>
              val (next, left) = rewriteState(region, paints)
              (regions :+ next, left)
          }
          (acc :+ StateStatement.Composite(id, dir, rewritten), left)
        case other => (acc :+ other, rest)
    }
end LinkStyle
