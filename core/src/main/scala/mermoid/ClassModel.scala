package mermoid

import mermoid.css.CssProperty

private[mermoid] object ClassModel:

  def resolve(diagram: Diagram.ClassDiagram): Either[ParseError, ResolvedClass] =
    val acc = walk(diagram.statements, Scope.Root, Acc.empty)
    acc.error match
      case Some(err) => Left(err)
      case None      =>
        val built = build(diagram.statements, Scope.Root, acc)
        Right(
          ResolvedClass(
            direction = diagram.direction,
            nodes = built.nodes,
            edges = built.edges,
            namespaces = built.namespaces,
            hideEmpty = acc.hideEmpty,
            classDefRules = acc.classDefs.toList.flatMap { (name, styles) =>
              StyleResolver.classDefRulesFor(name, styles)
            },
            accTitle = acc.accTitle,
            accDescr = acc.accDescr,
          )
        )
    end match
  end resolve

  def nodeIds(resolved: ResolvedClass): Set[String] =
    def fromNs(ns: ClassNamespace): Set[String] =
      ns.nodes.map(_.id.value).toSet ++ ns.children.flatMap(fromNs)
    resolved.nodes.map(_.id.value).toSet ++ resolved.namespaces.flatMap(fromNs)

  private enum Scope:
    case Root
    case In(id: NodeId)

  private case class Data(
      title: Option[String] = None,
      generic: Option[String] = None,
      stereotype: Option[String] = None,
      members: List[ClassMember] = Nil,
      cssClasses: List[String] = Nil,
      styles: Map[CssProperty, String] = Map.empty,
      interaction: Option[NodeInteraction] = None,
  )

  private case class Acc(
      owners: Map[NodeId, Scope],
      parents: Map[NodeId, Scope],
      data: Map[NodeId, Data],
      classDefs: Map[String, Map[CssProperty, String]],
      hideEmpty: Boolean,
      accTitle: Option[String],
      accDescr: Option[String],
      error: Option[ParseError],
  )

  private object Acc:
    val empty: Acc = Acc(Map.empty, Map.empty, Map.empty, Map.empty, false, None, None, None)

  private case class Built(nodes: List[ClassNode], edges: List[Edge], namespaces: List[ClassNamespace])

  private def walk(stmts: List[ClassStatement], scope: Scope, acc: Acc): Acc =
    stmts.foldLeft(acc) { (acc, stmt) =>
      if acc.error.isDefined then acc
      else
        stmt match
          case ClassStatement.HideEmptyMembers         => acc.copy(hideEmpty = true)
          case ClassStatement.AccTitle(text)           => acc.copy(accTitle = Some(text))
          case ClassStatement.AccDescr(text)           => acc.copy(accDescr = Some(text))
          case ClassStatement.ClassDefSt(name, styles) => acc.copy(classDefs = acc.classDefs + (name -> styles))
          case ClassStatement.Note(_, _)               => acc
          case ClassStatement.Declare(id, label, generic, stereo, members, classes) =>
            val claimed  = claim(acc, id, scope)
            val withData = annotate(claimed, id, label, generic, stereo, members, classes, Map.empty, None)
            withData
          case ClassStatement.Member(id, member) =>
            val claimed = claim(acc, id, scope)
            val prev    = claimed.data.getOrElse(id, Data())
            claimed.copy(data = claimed.data + (id -> prev.copy(members = prev.members :+ member)))
          case ClassStatement.Stereotype(id, name) =>
            val claimed = claim(acc, id, scope)
            setStereo(claimed, id, name)
          case ClassStatement.StyleSt(id, styles) =>
            val claimed = claim(acc, id, scope)
            val prev    = claimed.data.getOrElse(id, Data())
            claimed.copy(data = claimed.data + (id -> prev.copy(styles = prev.styles ++ styles)))
          case ClassStatement.ClickSt(binding) =>
            val claimed = claim(acc, binding.nodeId, scope)
            val prev    = claimed.data.getOrElse(binding.nodeId, Data())
            val hit     = NodeInteraction(binding.tooltip, binding.href, binding.linkTarget, binding.callbackName)
            claimed.copy(data = claimed.data + (binding.nodeId -> prev.copy(interaction = Some(hit))))
          case ClassStatement.Relation(from, to, _, _, _, _) =>
            claim(claim(acc, from, scope), to, scope)
          case ClassStatement.Namespace(id, _, inner) =>
            val next = acc.copy(parents = acc.parents + (id -> scope))
            walk(inner, Scope.In(id), next)
    }

  private def annotate(
      acc: Acc,
      id: NodeId,
      label: Option[String],
      generic: Option[String],
      stereo: Option[String],
      members: List[ClassMember],
      classes: List[String],
      styles: Map[CssProperty, String],
      interaction: Option[NodeInteraction],
  ): Acc =
    if acc.error.isDefined then acc
    else
      val prev = acc.data.getOrElse(id, Data())
      stereo match
        case Some(name) if prev.stereotype.exists(_ != name) =>
          fail(acc, ParseError.ConflictingForm(id, prev.stereotype.getOrElse(name), name))
        case _ =>
          val stereod = stereo.orElse(prev.stereotype)
          acc.copy(data =
            acc.data + (id -> prev.copy(
              title = label.orElse(prev.title),
              generic = generic.orElse(prev.generic),
              stereotype = stereod,
              members = prev.members ++ members,
              cssClasses = prev.cssClasses ++ classes,
              styles = prev.styles ++ styles,
              interaction = interaction.orElse(prev.interaction),
            ))
          )
      end match

  private def setStereo(acc: Acc, id: NodeId, name: String): Acc =
    annotate(acc, id, None, None, Some(name), Nil, Nil, Map.empty, None)

  private def claim(acc: Acc, id: NodeId, scope: Scope): Acc =
    if acc.error.isDefined then acc
    else
      acc.owners.get(id) match
        case None => acc.copy(owners = acc.owners + (id -> scope))
        case Some(prev) if prev == scope || encloses(prev, scope, acc) || encloses(scope, prev, acc) => acc
        case Some(prev)                                                                              =>
          fail(acc, ParseError.ClassInTwoNamespaces(id, place(prev), place(scope)))

  private def encloses(outer: Scope, inner: Scope, acc: Acc): Boolean =
    (outer, inner) match
      case (_, Scope.Root)                      => false
      case (Scope.Root, _)                      => true
      case (Scope.In(a), Scope.In(b)) if a == b => false
      case (Scope.In(_), Scope.In(child))       =>
        acc.parents.get(child) match
          case Some(parent) => parent == outer || encloses(outer, parent, acc)
          case None         => false

  private def place(scope: Scope): String = scope match
    case Scope.Root   => "the diagram"
    case Scope.In(id) => id.value

  private def fail(acc: Acc, error: ParseError): Acc =
    if acc.error.isDefined then acc else acc.copy(error = Some(error))

  private def build(stmts: List[ClassStatement], scope: Scope, acc: Acc): Built =
    val edges = stmts.collect { case relation: ClassStatement.Relation =>
      edgeOf(relation)
    }
    val localIds   = acc.owners.collect { case (id, owner) if owner == scope => id }.toList.sortBy(_.value)
    val nodes      = localIds.map(id => nodeOf(id, acc))
    val namespaces = stmts.collect { case ClassStatement.Namespace(id, label, inner) =>
      val built = build(inner, Scope.In(id), acc)
      ClassNamespace(id, label, built.nodes, built.namespaces, built.edges)
    }
    Built(nodes, edges, namespaces)
  end build

  private def edgeOf(relation: ClassStatement.Relation): Edge =
    val text = List(relation.fromCard, relation.label, relation.toCard).flatten.mkString(" ")
    Edge(
      relation.from,
      relation.to,
      EdgeStyle.Open,
      Option.when(text.nonEmpty)(text),
      mark = Some(relation.mark),
    )

  private def nodeOf(id: NodeId, acc: Acc): ClassNode =
    val data         = acc.data.getOrElse(id, Data())
    val (ops, attrs) = data.members.partition(_.operation)
    ClassNode(
      id,
      data.title.getOrElse(id.value),
      data.stereotype,
      data.generic,
      attrs,
      ops,
      data.cssClasses,
      data.styles,
      data.interaction,
    )
  end nodeOf

  def scene(resolved: ResolvedClass, config: RenderConfig, viewport: Option[Viewport]): DiagramScene =
    val dir     = DiagramLayout.effectiveDirection(resolved.direction, config.responsive, viewport)
    val measure = TextMeasure.resolve(config)
    val region = regionOf(resolved.nodes, resolved.edges, resolved.namespaces, dir, resolved.hideEmpty, config, measure)
    val placed = CompoundLayout.place(dir, List(region), config.layout, measure)
    val boxes  = compartments(resolved, config)
    finish(placed, boxes, resolved, config, dir)

  private def regionOf(
      nodes: List[ClassNode],
      edges: List[Edge],
      namespaces: List[ClassNamespace],
      direction: Direction,
      hideEmpty: Boolean,
      config: RenderConfig,
      measure: TextMeasure,
  ): CompoundLayout.Region =
    val groups = namespaces.map { ns =>
      val child = regionOf(ns.nodes, ns.edges, ns.children, direction, hideEmpty, config, measure)
      val title = ns.label.orElse(Some(ns.id.value))
      CompoundLayout.Item.Group(ns.id, title, direction, List(child), Nil, Map.empty)
    }
    val leaves = nodes.map { node =>
      val (w, h) = boxSize(node, hideEmpty, config, measure)
      CompoundLayout.Item.Leaf(
        NodeDef(node.id, Some(node.title), NodeShape.Rect),
        node.cssClasses,
        node.styles,
        Some((w, h)),
      )
    }
    CompoundLayout.Region(leaves ++ groups, edges)
  end regionOf

  private def compartments(resolved: ResolvedClass, config: RenderConfig): List[CompartmentBox] =
    def walk(nodes: List[ClassNode], namespaces: List[ClassNamespace]): List[CompartmentBox] =
      nodes.map(node => boxOf(node, resolved.hideEmpty, config)) ++
        namespaces.flatMap(ns => walk(ns.nodes, ns.children))
    walk(resolved.nodes, resolved.namespaces)

  private def boxOf(node: ClassNode, hideEmpty: Boolean, config: RenderConfig): CompartmentBox =
    val line   = config.layout.lineHeight
    val header = if node.stereotype.isDefined || node.generic.isDefined then line * 2 else line + 8
    val attrH  = band(node.attributes.size, hideEmpty, line)
    val opH    = band(node.operations.size, hideEmpty, line)
    CompartmentBox(
      node.id,
      node.stereotype,
      titleOf(node),
      node.attributes.map(showMember),
      node.operations.map(showMember),
      header,
      attrH,
      opH,
    )
  end boxOf

  private def titleOf(node: ClassNode): String =
    node.generic match
      case Some(arg) => s"${node.title}~$arg~"
      case None      => node.title

  private def showMember(member: ClassMember): String =
    val vis = member.visibility match
      case Some(ClassVisibility.Public)    => "+"
      case Some(ClassVisibility.Private)   => "-"
      case Some(ClassVisibility.Protected) => "#"
      case Some(ClassVisibility.Package)   => "~"
      case None                            => ""
    val body =
      if member.operation then
        val params = member.parameters.getOrElse("")
        val ret    = member.returnType.map(t => s" $t").getOrElse("")
        s"${member.name}($params)$ret"
      else
        member.typeName match
          case Some(tpe) => s"$tpe ${member.name}"
          case None      => member.name
    val mark = member.classifier match
      case ClassClassifier.Abstract => "*"
      case ClassClassifier.Static   => "$"
      case ClassClassifier.None     => ""
    s"$vis$body$mark"
  end showMember

  private def band(count: Int, hideEmpty: Boolean, line: Double): Double =
    if count == 0 && hideEmpty then 0
    else Math.max(line, count * line) + 8

  private def boxSize(
      node: ClassNode,
      hideEmpty: Boolean,
      config: RenderConfig,
      measure: TextMeasure,
  ): (Double, Double) =
    val box    = boxOf(node, hideEmpty, config)
    val lc     = config.layout
    val lines  = box.title :: box.stereotype.toList ++ box.attributes ++ box.operations
    val textW  = lines.map(line => measure.width(line, lc.fontSize.toDouble, lc.fontFamily)).maxOption.getOrElse(0.0)
    val width  = Math.max(lc.minNodeWidth, textW + lc.nodePaddingH * 2)
    val height = box.headerHeight + box.attributeHeight + box.operationHeight + 8
    (width, Math.max(lc.nodeHeight, height))
  end boxSize

  private def finish(
      placed: CompoundLayout.Placed,
      boxes: List[CompartmentBox],
      resolved: ResolvedClass,
      config: RenderConfig,
      direction: Direction,
  ): DiagramScene =
    val loopSide = direction match
      case Direction.TB | Direction.TD | Direction.BT => SelfLoopSide.Right
      case Direction.LR | Direction.RL                => SelfLoopSide.Top
    val interactions = interactionsOf(resolved)
    val nodeMap      = placed.nodes.map(n => n.id -> n).toMap
    val visible      = placed.nodes.filter(!_.dummy)
    val edges        = SvgRenderer.buildLayoutEdges(placed.edges)
    val ink          =
      visible.map(InkBox.fromNode) ++
        placed.routes.values.flatten.map(InkBox.fromPoint(_)) ++
        placed.frames.map(f => InkBox(f.rect.x, f.rect.y, f.rect.w, f.rect.h)) ++
        edges.flatMap(e =>
          EdgeRenderer.edgeInk(config, e, nodeMap, placed.routes.getOrElse((e.from, e.to), Nil), loopSide)
        )
    val fitted                = LayoutBounds.fit(config.layout.padding, ink)
    def move(p: Point): Point = Point(p.x + fitted.shiftX, p.y + fitted.shiftY)
    DiagramScene(
      width = fitted.width,
      height = fitted.height,
      nodes = if fitted.shiftX == 0 && fitted.shiftY == 0 then placed.nodes
      else placed.nodes.map(n => n.copy(center = move(n.center))),
      edges = edges,
      routes = if fitted.shiftX == 0 && fitted.shiftY == 0 then placed.routes
      else placed.routes.map((k, pts) => k -> pts.map(move)),
      subgraphs =
        placed.frames.map(f => f.copy(rect = f.rect.copy(x = f.rect.x + fitted.shiftX, y = f.rect.y + fitted.shiftY))),
      notes = Nil,
      interactions = interactions,
      loopSide = loopSide,
      classDefRules = resolved.classDefRules,
      config = config,
      direction = direction,
      accTitle = resolved.accTitle,
      accDescr = resolved.accDescr,
      compartments = boxes,
    )
  end finish

  private def interactionsOf(resolved: ResolvedClass): Map[NodeId, NodeInteraction] =
    def walk(nodes: List[ClassNode], namespaces: List[ClassNamespace]): Map[NodeId, NodeInteraction] =
      nodes.flatMap(n => n.interaction.map(n.id -> _)).toMap ++
        namespaces.flatMap(ns => walk(ns.nodes, ns.children))
    walk(resolved.nodes, resolved.namespaces)
end ClassModel
