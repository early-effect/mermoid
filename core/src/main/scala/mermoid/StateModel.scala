package mermoid

import mermoid.css.CssProperty

/** A state diagram after the source has been checked. Layout reads this, not the statement list. */
private[mermoid] case class StateMachine(
    direction: Direction,
    regions: List[StateRegion],
    hideEmptyDescription: Boolean,
    scaleWidth: Option[Int],
    accTitle: Option[String],
    accDescr: Option[String],
    classDefRules: List[mermoid.css.CssRule],
    /** Nodes with no other class receive this paint. */
    defaultClass: Option[String],
)

private[mermoid] case class StateRegion(
    nodes: List[StateNode],
    edges: List[Edge],
    notes: List[StateStatement.NoteSt],
    floating: List[StateStatement.FloatingNote],
)

private[mermoid] enum StateNode:
  case Atom(
      id: NodeId,
      label: String,
      form: StateForm,
      classes: List[String],
      styles: Map[CssProperty, String],
      interaction: Option[NodeInteraction],
  )
  case Composite(
      id: NodeId,
      /** `None` when `hide empty description` left a composite with no description. */
      title: Option[String],
      direction: Direction,
      regions: List[StateRegion],
      classes: List[String],
      styles: Map[CssProperty, String],
      interaction: Option[NodeInteraction],
  )
end StateNode

/** Claims each state once, then builds the region tree. A flat machine keeps today's ids for `[*]`. */
private[mermoid] object StateModel:

  def resolve(diagram: Diagram.StateDiagram): Either[ParseError, StateMachine] =
    val acc = walk(diagram.statements, Scope.Root, Acc.empty)
    acc.error match
      case Some(error) => Left(error)
      case None        =>
        val hide    = acc.hideEmpty
        val scale   = parseScale(acc.scaleRaw)
        val regions = buildRegions(diagram.statements, Scope.Root, diagram.direction, acc, hide)
        scale.map { width =>
          StateMachine(
            direction = diagram.direction,
            regions = regions,
            hideEmptyDescription = hide,
            scaleWidth = width,
            accTitle = acc.accTitle,
            accDescr = acc.accDescr,
            classDefRules = acc.classDefs.toList.flatMap { case (name, styles) =>
              StyleResolver.classDefRulesFor(name, styles)
            },
            defaultClass = acc.classDefs.keys.find(_ == "default"),
          )
        }
    end match
  end resolve

  /** No composites, no pseudostate other than start and end, no description that replaces an id. */
  def isLegacy(machine: StateMachine): Boolean =
    machine.scaleWidth.isEmpty && !machine.hideEmptyDescription && machine.regions.size == 1 &&
      machine.regions.forall(regionIsLegacy)

  private def regionIsLegacy(region: StateRegion): Boolean =
    region.floating.isEmpty && region.nodes.forall {
      case StateNode.Atom(id, label, StateForm.Simple, _, _, _) =>
        val marker = id == NodeId.stateMarker || id == NodeId.stateEnd
        marker || label == id.value
      case _ => false
    }

  private enum Scope:
    case Root
    case In(parent: NodeId, region: Int)

  private case class Acc(
      owners: Map[NodeId, Scope],
      descriptions: Map[NodeId, String],
      forms: Map[NodeId, StateForm],
      bodies: Map[NodeId, (Option[Direction], List[List[StateStatement]])],
      classDefs: Map[String, Map[CssProperty, String]],
      hideEmpty: Boolean,
      scaleRaw: Option[String],
      accTitle: Option[String],
      accDescr: Option[String],
      error: Option[ParseError],
  )

  private object Acc:
    val empty: Acc = Acc(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, false, None, None, None, None)

  private def walk(stmts: List[StateStatement], scope: Scope, acc: Acc): Acc =
    stmts.foldLeft(acc) { (acc, stmt) =>
      if acc.error.isDefined then acc
      else
        stmt match
          case StateStatement.Divider =>
            scope match
              case Scope.Root     => fail(acc, ParseError.DividerOutsideComposite)
              case Scope.In(_, _) => acc
          case StateStatement.HideEmptyDescription  => acc.copy(hideEmpty = true)
          case StateStatement.Scale(raw)            => acc.copy(scaleRaw = Some(raw))
          case StateStatement.AccTitle(text)        => acc.copy(accTitle = Some(text))
          case StateStatement.AccDescr(text)        => acc.copy(accDescr = Some(text))
          case StateStatement.UnknownStereo(_, raw) =>
            fail(acc, ParseError.UnknownStereotype(raw))
          case StateStatement.ClassDefSt(name, styles) =>
            acc.copy(classDefs = acc.classDefs + (name -> styles))
          case StateStatement.Description(id, text) =>
            val claimed = claim(acc, id, scope)
            claimed.descriptions.get(id) match
              case Some(prev) if prev != text => fail(claimed, ParseError.ConflictingDescription(id, prev, text))
              case _                          => claimed.copy(descriptions = claimed.descriptions + (id -> text))
          case StateStatement.Form(id, form) =>
            val claimed = claim(acc, id, scope)
            if claimed.bodies.contains(id) && form != StateForm.Simple then
              fail(claimed, ParseError.ConflictingForm(id, "a composite", formName(form)))
            else if claimed.bodies.contains(id) then claimed
            else
              claimed.forms.get(id) match
                case Some(prev) if prev != form && prev != StateForm.Simple && form != StateForm.Simple =>
                  fail(claimed, ParseError.ConflictingForm(id, formName(prev), formName(form)))
                case Some(StateForm.Simple)              => claimed.copy(forms = claimed.forms + (id -> form))
                case Some(_) if form == StateForm.Simple => claimed
                case _                                   => claimed.copy(forms = claimed.forms + (id -> form))
            end if
          case StateStatement.Composite(id, dir, regions) =>
            val claimed = claim(acc, id, scope)
            if claimed.bodies.contains(id) then fail(claimed, ParseError.DuplicateComposite(id))
            else
              claimed.forms.get(id) match
                case Some(form) if form != StateForm.Simple =>
                  fail(claimed, ParseError.ConflictingForm(id, formName(form), "a composite"))
                case _ =>
                  val withBody = claimed.copy(bodies = claimed.bodies + (id -> (dir, regions)))
                  regions.zipWithIndex.foldLeft(withBody) { case (a, (inner, index)) =>
                    walk(inner, Scope.In(id, index), a)
                  }
            end if
          case StateStatement.TransitionSt(t) =>
            claim(claim(acc, t.from, scope), t.to, scope)
          case StateStatement.NoteSt(_, id, _, _) => claim(acc, id, scope)
          case StateStatement.StyleSt(id, _)      => claim(acc, id, scope)
          case StateStatement.ClassSt(ids, _)     => ids.foldLeft(acc)((a, id) => claim(a, id, scope))
          case StateStatement.ClickSt(binding)    => claim(acc, binding.nodeId, scope)
          case StateStatement.FloatingNote(_, _)  => acc
    }

  private def claim(acc: Acc, id: NodeId, scope: Scope): Acc =
    if acc.error.isDefined || isPseudo(id) then acc
    else
      acc.owners.get(id) match
        case None => acc.copy(owners = acc.owners + (id -> scope))
        case Some(prev) if prev == scope || encloses(prev, scope, acc.owners) || encloses(scope, prev, acc.owners) =>
          acc
        case Some(prev) => fail(acc, ParseError.StateInTwoComposites(id, place(prev), place(scope)))

  /** `outer` contains `inner` in the composite tree. Sibling regions do not contain each other. */
  private def encloses(outer: Scope, inner: Scope, owners: Map[NodeId, Scope]): Boolean =
    (outer, inner) match
      case (_, Scope.Root)                            => false
      case (Scope.Root, _)                            => true
      case (Scope.In(a, _), Scope.In(b, _)) if a == b => false
      case (Scope.In(_, _), Scope.In(parent, _))      =>
        owners.get(parent) match
          case Some(parentScope) => parentScope == outer || encloses(outer, parentScope, owners)
          case None              => false

  private def place(scope: Scope): StatePlace = scope match
    case Scope.Root              => StatePlace.Diagram
    case Scope.In(parent, index) => StatePlace.Region(parent, index)

  private def fail(acc: Acc, error: ParseError): Acc =
    if acc.error.isDefined then acc else acc.copy(error = Some(error))

  private def isPseudo(id: NodeId): Boolean =
    id == NodeId.stateMarker || id.value == "[H]" || id.value == "[H*]"

  private def formName(form: StateForm): String = form match
    case StateForm.Simple         => "a state"
    case StateForm.Choice         => "a choice"
    case StateForm.Fork           => "a fork"
    case StateForm.Join           => "a join"
    case StateForm.ShallowHistory => "a history pseudostate"
    case StateForm.DeepHistory    => "a deep history pseudostate"

  private def parseScale(raw: Option[String]): Either[ParseError, Option[Int]] =
    raw match
      case None       => Right(None)
      case Some(text) =>
        text.trim match
          case s"${n} width" =>
            n.trim.toIntOption match
              case Some(px) if px > 0 => Right(Some(px))
              case _                  => Left(ParseError.BadScale(text.trim))
          case _ => Left(ParseError.BadScale(text.trim))

  private def buildRegions(
      stmts: List[StateStatement],
      scope: Scope,
      direction: Direction,
      acc: Acc,
      hide: Boolean,
  ): List[StateRegion] =
    List(buildRegion(stmts, scope, direction, acc, hide))

  private def buildRegion(
      stmts: List[StateStatement],
      scope: Scope,
      direction: Direction,
      acc: Acc,
      hide: Boolean,
  ): StateRegion =
    val transitions = stmts.collect { case StateStatement.TransitionSt(t) => t }
    val startId     = markerId(scope, end = false)
    val endId       = markerId(scope, end = true)
    val hasStart    = transitions.exists(t => t.from == NodeId.stateMarker)
    val hasEnd      = transitions.exists(t => t.to == NodeId.stateMarker)
    val edges       = transitions.map { t =>
      Edge(
        rewrite(t.from, scope, startId, endId, asTarget = false),
        rewrite(t.to, scope, startId, endId, asTarget = true),
        EdgeStyle.Arrow,
        t.label,
      )
    }
    val classes        = classesIn(stmts, scope, endId, hasEnd, acc)
    val styles         = stylesIn(stmts)
    val clicks         = clicksIn(stmts, scope, endId, hasEnd)
    val owned          = acc.owners.collect { case (id, owner) if owner == scope => id }.toList.sortBy(_.value)
    val atomsAndGroups = owned.map { id =>
      nodeFor(id, direction, acc, hide, classes, styles, clicks)
    }
    val markers = markerNodes(startId, endId, hasStart, hasEnd, classes, styles, clicks)
    val history = historyNodes(scope, transitions, classes, styles)
    StateRegion(
      nodes = markers ++ history ++ atomsAndGroups,
      edges = edges,
      notes = stmts.collect { case n: StateStatement.NoteSt => n },
      floating = stmts.collect { case n: StateStatement.FloatingNote => n },
    )
  end buildRegion

  private def nodeFor(
      id: NodeId,
      direction: Direction,
      acc: Acc,
      hide: Boolean,
      classes: Map[NodeId, List[String]],
      styles: Map[NodeId, Map[CssProperty, String]],
      clicks: Map[NodeId, NodeInteraction],
  ): StateNode =
    acc.bodies.get(id) match
      case Some((dir, regions)) =>
        val nestedDir = dir.getOrElse(direction)
        val built     = regions.zipWithIndex.map { case (inner, index) =>
          buildRegion(inner, Scope.In(id, index), nestedDir, acc, hide)
        }
        val title = acc.descriptions.get(id).orElse(Option.unless(hide)(id.value))
        StateNode.Composite(
          id,
          title,
          nestedDir,
          if built.isEmpty then List(StateRegion(Nil, Nil, Nil, Nil)) else built,
          paintClasses(id, classes, acc),
          styles.getOrElse(id, Map.empty),
          clicks.get(id),
        )
      case None =>
        val form  = acc.forms.getOrElse(id, StateForm.Simple)
        val label = acc.descriptions.get(id).getOrElse(if hide then "" else id.value)
        StateNode.Atom(
          id,
          label,
          form,
          paintClasses(id, classes, acc),
          styles.getOrElse(id, Map.empty),
          clicks.get(id),
        )

  private def paintClasses(id: NodeId, classes: Map[NodeId, List[String]], acc: Acc): List[String] =
    val own = classes.getOrElse(id, Nil)
    if own.nonEmpty then own
    else acc.classDefs.get("default").map(_ => List("default")).getOrElse(Nil)

  private def markerNodes(
      startId: NodeId,
      endId: NodeId,
      hasStart: Boolean,
      hasEnd: Boolean,
      classes: Map[NodeId, List[String]],
      styles: Map[NodeId, Map[CssProperty, String]],
      clicks: Map[NodeId, NodeInteraction],
  ): List[StateNode] =
    val start =
      if hasStart then
        List(
          StateNode.Atom(
            startId,
            "",
            StateForm.Simple,
            css.PaintClass.StartEnd.cssName :: classes.getOrElse(startId, Nil),
            styles.getOrElse(NodeId.stateMarker, Map.empty),
            clicks.get(startId),
          )
        )
      else Nil
    val endClasses = classes.getOrElse(endId, Nil)
    val end        =
      if hasEnd then
        List(
          StateNode.Atom(
            endId,
            "",
            StateForm.Simple,
            css.PaintClass.StartEnd.cssName :: css.PaintClass.StateEnd.cssName :: endClasses,
            styles.getOrElse(NodeId.stateMarker, Map.empty) ++ styles.getOrElse(endId, Map.empty),
            clicks.get(endId),
          )
        )
      else Nil
    start ++ end
  end markerNodes

  private def historyNodes(
      scope: Scope,
      transitions: List[StateTransition],
      classes: Map[NodeId, List[String]],
      styles: Map[NodeId, Map[CssProperty, String]],
  ): List[StateNode] =
    val usesShallow = transitions.exists(t => t.from.value == "[H]" || t.to.value == "[H]")
    val usesDeep    = transitions.exists(t => t.from.value == "[H*]" || t.to.value == "[H*]")
    val shallow     =
      if usesShallow then
        val id = historyId(scope, deep = false)
        List(
          StateNode
            .Atom(id, "H", StateForm.ShallowHistory, classes.getOrElse(id, Nil), styles.getOrElse(id, Map.empty), None)
        )
      else Nil
    val deep =
      if usesDeep then
        val id = historyId(scope, deep = true)
        List(
          StateNode
            .Atom(id, "H*", StateForm.DeepHistory, classes.getOrElse(id, Nil), styles.getOrElse(id, Map.empty), None)
        )
      else Nil
    shallow ++ deep
  end historyNodes

  private def classesIn(
      stmts: List[StateStatement],
      scope: Scope,
      endId: NodeId,
      hasEnd: Boolean,
      acc: Acc,
  ): Map[NodeId, List[String]] =
    val startId = markerId(scope, end = false)
    stmts.foldLeft(Map.empty[NodeId, List[String]]) { (accMap, stmt) =>
      stmt match
        case StateStatement.ClassSt(ids, name) =>
          ids.foldLeft(accMap) { (m, id) =>
            val target =
              if id.value == "end" && hasEnd && !acc.owners.contains(NodeId.trusted("end")) then endId
              else id
            append(m, target, name)
          }
        case StateStatement.TransitionSt(t) =>
          val from = rewrite(t.from, scope, startId, endId, asTarget = false)
          val to   = rewrite(t.to, scope, startId, endId, asTarget = true)
          t.toClasses.foldLeft(t.fromClasses.foldLeft(accMap)(append(_, from, _)))(append(_, to, _))
        case _ => accMap
    }
  end classesIn

  private def stylesIn(stmts: List[StateStatement]): Map[NodeId, Map[CssProperty, String]] =
    stmts.foldLeft(Map.empty[NodeId, Map[CssProperty, String]]) { (acc, stmt) =>
      stmt match
        case StateStatement.StyleSt(id, style) if style.paint.nonEmpty =>
          acc + (id -> (acc.getOrElse(id, Map.empty) ++ style.paint))
        case _ => acc
    }

  private def clicksIn(
      stmts: List[StateStatement],
      scope: Scope,
      endId: NodeId,
      hasEnd: Boolean,
  ): Map[NodeId, NodeInteraction] =
    val startId = markerId(scope, end = false)
    stmts.foldLeft(Map.empty[NodeId, NodeInteraction]) { (acc, stmt) =>
      stmt match
        case StateStatement.ClickSt(binding) =>
          val id =
            if binding.nodeId == NodeId.stateMarker then startId
            else if binding.nodeId.value == "end" && hasEnd && !stmts.exists(realEnd) then endId
            else binding.nodeId
          acc + (id -> NodeInteraction(binding.tooltip, binding.href, binding.linkTarget, binding.callbackName))
        case _ => acc
    }
  end clicksIn

  private def realEnd(stmt: StateStatement): Boolean = stmt match
    case StateStatement.TransitionSt(t)     => t.from.value == "end" || t.to.value == "end"
    case StateStatement.Description(id, _)  => id.value == "end"
    case StateStatement.Form(id, _)         => id.value == "end"
    case StateStatement.Composite(id, _, _) => id.value == "end"
    case _                                  => false

  private def append(acc: Map[NodeId, List[String]], id: NodeId, name: String): Map[NodeId, List[String]] =
    val cur = acc.getOrElse(id, Nil)
    if cur.contains(name) then acc else acc + (id -> (cur :+ name))

  private def rewrite(id: NodeId, scope: Scope, startId: NodeId, endId: NodeId, asTarget: Boolean): NodeId =
    if id == NodeId.stateMarker then if asTarget then endId else startId
    else if id.value == "[H]" then historyId(scope, deep = false)
    else if id.value == "[H*]" then historyId(scope, deep = true)
    else id

  /** Root markers keep the ids the flat renderer and the example SVGs already use. */
  private def markerId(scope: Scope, end: Boolean): NodeId = scope match
    case Scope.Root              => if end then NodeId.stateEnd else NodeId.stateMarker
    case Scope.In(parent, index) =>
      val kind = if end then "end" else "start"
      NodeId.trusted(s"[*]-$kind@${parent.value}#$index")

  private def historyId(scope: Scope, deep: Boolean): NodeId =
    val kind = if deep then "H*" else "H"
    scope match
      case Scope.Root              => NodeId.trusted(s"[$kind]")
      case Scope.In(parent, index) => NodeId.trusted(s"[$kind]@${parent.value}#$index")
end StateModel
