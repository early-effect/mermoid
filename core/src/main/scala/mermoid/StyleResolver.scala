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

  /** Collect CSS class names assigned to nodes via `class A,B foo` */
  private[mermoid] def collectNodeClasses(stmts: List[FlowStatement]): Map[String, List[String]] =
    stmts.foldLeft(Map.empty[String, List[String]]) { (acc, stmt) =>
      stmt match
        case FlowStatement.ClassSt(ids, className) =>
          ids.foldLeft(acc) { (m, id) =>
            m + (id -> (m.getOrElse(id, Nil) :+ className))
          }
        case FlowStatement.SubgraphSt(_, _, _, inner) =>
          collectNodeClasses(inner).foldLeft(acc) { case (a, (id, cls)) =>
            a + (id -> (a.getOrElse(id, Nil) ++ cls))
          }
        case _ => acc
    }

  /** Collect inline style overrides from `style A fill:#f00` */
  private[mermoid] def collectInlineStyles(stmts: List[FlowStatement]): Map[String, Map[String, String]] =
    stmts.foldLeft(Map.empty[String, Map[String, String]]) { (acc, stmt) =>
      stmt match
        case FlowStatement.StyleSt(id, styles) =>
          acc + (id -> (acc.getOrElse(id, Map.empty) ++ styles))
        case FlowStatement.SubgraphSt(_, _, _, inner) =>
          collectInlineStyles(inner).foldLeft(acc) { case (a, (id, s)) =>
            a + (id -> (a.getOrElse(id, Map.empty) ++ s))
          }
        case _ => acc
    }

  /** Convert `classDef` statements to CSS rules */
  private[mermoid] def classDefsToRules(stmts: List[FlowStatement]): List[CssRule] =
    stmts.flatMap {
      case FlowStatement.ClassDefSt(name, styles) =>
        val decls = styles.toList.map { case (prop, value) =>
          CssDeclaration(prop, CssValue.Str(value))
        }
        List(CssRule(CssSelector.Class(name), decls))
      case FlowStatement.SubgraphSt(_, _, _, inner) =>
        classDefsToRules(inner)
      case _ => Nil
    }

  // Keep backward compatibility for now
  private[mermoid] def collectStyleDefs(stmts: List[FlowStatement]): Map[String, Map[String, String]] =
    val (classDefs, nodeStyles, nodeClasses) = stmts.foldLeft(
      (Map.empty[String, Map[String, String]], Map.empty[String, Map[String, String]], Map.empty[String, String])
    ) { case ((cds, ns, ncs), stmt) =>
      stmt match
        case FlowStatement.ClassDefSt(name, styles) => (cds + (name -> styles), ns, ncs)
        case FlowStatement.StyleSt(id, styles)      => (cds, ns + (id -> styles), ncs)
        case FlowStatement.ClassSt(ids, className)  => (cds, ns, ids.foldLeft(ncs)((m, id) => m + (id -> className)))
        case _                                      => (cds, ns, ncs)
    }
    val withClasses = nodeClasses.foldLeft(Map.empty[String, Map[String, String]]) { case (acc, (id, cls)) =>
      classDefs.get(cls) match
        case Some(s) => acc + (id -> (acc.getOrElse(id, Map.empty) ++ s))
        case None    => acc
    }
    nodeStyles.foldLeft(withClasses) { case (acc, (id, s)) =>
      acc + (id -> (acc.getOrElse(id, Map.empty) ++ s))
    }
  end collectStyleDefs
end StyleResolver
