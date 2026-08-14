package mermoid

import mermoid.css.*

object StyleResolver:

  private[mermoid] def collectNodes(stmts: List[FlowStatement]): Map[String, NodeDef] =
    stmts.foldLeft(Map.empty[String, NodeDef]) { (acc, stmt) =>
      stmt match
        case FlowStatement.NodeSt(n) =>
          if acc.contains(n.id) then acc else acc + (n.id -> n)
        case FlowStatement.EdgeSt(_, fromNode, toNode) =>
          val a1 = if acc.contains(fromNode.id) then acc else acc + (fromNode.id -> fromNode)
          if a1.contains(toNode.id) then a1 else a1 + (toNode.id -> toNode)
        case FlowStatement.SubgraphSt(_, _, _, inner) =>
          collectNodes(inner).foldLeft(acc) { case (a, (id, nd)) =>
            if a.contains(id) then a else a + (id -> nd)
          }
        case _ => acc
    }

  private[mermoid] def collectEdges(stmts: List[FlowStatement]): List[Edge] =
    stmts.flatMap {
      case FlowStatement.EdgeSt(e, _, _)            => List(e)
      case FlowStatement.SubgraphSt(_, _, _, inner) => collectEdges(inner)
      case _                                        => Nil
    }

  case class SubgraphInfo(id: String, label: Option[String], direction: Option[Direction], nodeIds: Set[String])

  private[mermoid] def collectSubgraphs(stmts: List[FlowStatement]): List[SubgraphInfo] =
    stmts.flatMap {
      case FlowStatement.SubgraphSt(id, label, dir, inner) =>
        val innerNodeIds = collectNodes(inner).keySet
        val nested       = collectSubgraphs(inner)
        SubgraphInfo(id, label, dir, innerNodeIds) :: nested
      case _ => Nil
    }

  private def appendClass(acc: Map[String, List[String]], id: String, className: String): Map[String, List[String]] =
    val cur = acc.getOrElse(id, Nil)
    if cur.contains(className) then acc else acc + (id -> (cur :+ className))

  private def appendClasses(
      acc: Map[String, List[String]],
      id: String,
      classes: List[String],
  ): Map[String, List[String]] =
    classes.foldLeft(acc)(appendClass(_, id, _))

  private def classDefRules(name: String, styles: Map[CssProperty, String]): List[CssRule] =
    val decls = styles.toList.map { case (prop, value) => CssDeclaration(prop, CssValue.Str(value)) }
    List(
      CssRule(CssSelector.Class(name), decls),
      CssRule(
        CssSelector.Descendant(CssSelector.Class(name), PaintClass.NodeShape.selector),
        decls,
      ),
    )

  /** Convert `classDef` statements to CSS rules */
  private[mermoid] def classDefsToRules(stmts: List[FlowStatement]): List[CssRule] =
    stmts.flatMap {
      case FlowStatement.ClassDefSt(name, styles)   => classDefRules(name, styles)
      case FlowStatement.SubgraphSt(_, _, _, inner) => classDefsToRules(inner)
      case _                                        => Nil
    }

  /** Collect CSS class names assigned to nodes via `class A,B foo` and `A:::foo`. */
  private[mermoid] def collectNodeClasses(stmts: List[FlowStatement]): Map[String, List[String]] =
    stmts.foldLeft(Map.empty[String, List[String]]) { (acc, stmt) =>
      stmt match
        case FlowStatement.ClassSt(ids, className) =>
          ids.foldLeft(acc)(appendClass(_, _, className))
        case FlowStatement.NodeSt(n) =>
          appendClasses(acc, n.id, n.cssClasses)
        case FlowStatement.EdgeSt(_, fromNode, toNode) =>
          appendClasses(appendClasses(acc, fromNode.id, fromNode.cssClasses), toNode.id, toNode.cssClasses)
        case FlowStatement.SubgraphSt(_, _, _, inner) =>
          collectNodeClasses(inner).foldLeft(acc) { case (a, (id, cls)) =>
            cls.foldLeft(a)(appendClass(_, id, _))
          }
        case _ => acc
    }

  /** `class` / `:::` on a state diagram. `class end` and `A --> [*]:::x` target the end marker when start/end split. */
  private[mermoid] def collectStateClasses(
      stmts: List[StateStatement],
      splitStartEnd: Boolean,
      endId: String,
  ): Map[String, List[String]] =
    def rewriteClassId(id: String): String =
      if id == "end" && splitStartEnd then endId else id

    stmts.foldLeft(Map.empty[String, List[String]]) { (acc, stmt) =>
      stmt match
        case StateStatement.ClassSt(ids, className) =>
          ids.foldLeft(acc)((m, id) => appendClass(m, rewriteClassId(id), className))
        case StateStatement.TransitionSt(t) =>
          val withFrom = appendClasses(acc, t.from, t.fromClasses)
          val to       = if splitStartEnd && t.to == "[*]" then endId else t.to
          appendClasses(withFrom, to, t.toClasses)
        case _ => acc
    }
  end collectStateClasses

  private[mermoid] def collectStateInlineStyles(stmts: List[StateStatement]): Map[String, Map[CssProperty, String]] =
    stmts.foldLeft(Map.empty[String, Map[CssProperty, String]]) { (acc, stmt) =>
      stmt match
        case StateStatement.StyleSt(id, style) if style.paint.nonEmpty =>
          acc + (id -> (acc.getOrElse(id, Map.empty) ++ style.paint))
        case _ => acc
    }

  private[mermoid] def stateClassDefsToRules(stmts: List[StateStatement]): List[CssRule] =
    stmts.collect { case StateStatement.ClassDefSt(name, styles) => classDefRules(name, styles) }.flatten

  /** Collect inline style overrides from `style A fill:#f00` */
  private[mermoid] def collectInlineStyles(stmts: List[FlowStatement]): Map[String, Map[css.CssProperty, String]] =
    stmts.foldLeft(Map.empty[String, Map[css.CssProperty, String]]) { (acc, stmt) =>
      stmt match
        case FlowStatement.StyleSt(id, styles) =>
          acc + (id -> (acc.getOrElse(id, Map.empty) ++ styles))
        case FlowStatement.SubgraphSt(_, _, _, inner) =>
          collectInlineStyles(inner).foldLeft(acc) { case (a, (id, s)) =>
            a + (id -> (a.getOrElse(id, Map.empty) ++ s))
          }
        case _ => acc
    }

  /** Merge `click` bindings (later statements win per field when re-specified). */
  private[mermoid] def collectInteractions(stmts: List[FlowStatement]): Map[String, NodeInteraction] =
    stmts
      .flatMap {
        case FlowStatement.ClickSt(b)                 => List(b)
        case FlowStatement.SubgraphSt(_, _, _, inner) =>
          collectInteractions(inner).toList.map { case (id, i) =>
            ClickBinding(id, i.tooltip, i.href, i.linkTarget, i.callbackName)
          }
        case _ => Nil
      }
      .foldLeft(Map.empty[String, NodeInteraction]) { (acc, b) =>
        val prev = acc.getOrElse(b.nodeId, NodeInteraction())
        acc.updated(
          b.nodeId,
          NodeInteraction(
            tooltip = b.tooltip.orElse(prev.tooltip),
            href = b.href.orElse(prev.href),
            linkTarget = b.linkTarget.orElse(prev.linkTarget),
            callbackName = b.callbackName.orElse(prev.callbackName),
          ),
        )
      }

  // Keep backward compatibility for now
  private[mermoid] def collectStyleDefs(stmts: List[FlowStatement]): Map[String, Map[css.CssProperty, String]] =
    val (classDefs, nodeStyles, nodeClasses) = stmts.foldLeft(
      (
        Map.empty[String, Map[css.CssProperty, String]],
        Map.empty[String, Map[css.CssProperty, String]],
        Map.empty[String, String],
      )
    ) { case ((cds, ns, ncs), stmt) =>
      stmt match
        case FlowStatement.ClassDefSt(name, styles) => (cds + (name -> styles), ns, ncs)
        case FlowStatement.StyleSt(id, styles)      => (cds, ns + (id -> styles), ncs)
        case FlowStatement.ClassSt(ids, className)  => (cds, ns, ids.foldLeft(ncs)((m, id) => m + (id -> className)))
        case _                                      => (cds, ns, ncs)
    }
    val withClasses = nodeClasses.foldLeft(Map.empty[String, Map[css.CssProperty, String]]) { case (acc, (id, cls)) =>
      classDefs.get(cls) match
        case Some(s) => acc + (id -> (acc.getOrElse(id, Map.empty) ++ s))
        case None    => acc
    }
    nodeStyles.foldLeft(withClasses) { case (acc, (id, s)) =>
      acc + (id -> (acc.getOrElse(id, Map.empty) ++ s))
    }
  end collectStyleDefs
end StyleResolver
