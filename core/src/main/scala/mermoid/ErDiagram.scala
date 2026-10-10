package mermoid

import fastparse.*
import fastparse.NoWhitespace.*
import mermoid.css.CssProperty

case class ErAttribute(
    typeName: String,
    name: String,
    primary: Boolean,
    foreign: Boolean,
    comment: Option[String],
)

enum ErStatement:
  case Entity(id: NodeId, attributes: List[ErAttribute])
  case Relation(from: NodeId, to: NodeId, fromFoot: MarkKind, toFoot: MarkKind, dashed: Boolean, label: String)
  case ClassDefSt(name: String, styles: Map[CssProperty, String])
  case StyleSt(id: NodeId, styles: Map[CssProperty, String])
  case AccTitle(text: String)
  case AccDescr(text: String)

private[mermoid] object ErParser:

  def document(using P[Any]): P[Diagram.ErDiagram] =
    P(MermaidParser.wsnl ~ "erDiagram" ~ MermaidParser.nl ~ body ~ MermaidParser.wsnl ~ End).map { stmts =>
      Diagram.ErDiagram(Direction.TB, stmts)
    }

  private def body(using P[Any]): P[List[ErStatement]] =
    P(MermaidParser.wsnl ~ statement.rep(sep = MermaidParser.sep) ~ MermaidParser.wsnl).map(_.toList)

  private def statement(using P[Any]): P[ErStatement] =
    P(accTitle | accDescr | classDef | style | entity | relation)

  private def accTitle(using P[Any]): P[ErStatement.AccTitle] =
    P("accTitle" ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ rest).map(ErStatement.AccTitle(_))

  private def accDescr(using P[Any]): P[ErStatement.AccDescr] =
    P("accDescr" ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ rest).map(ErStatement.AccDescr(_))

  private def classDef(using P[Any]): P[ErStatement.ClassDefSt] =
    P("classDef" ~ MermaidParser.ws ~ MermaidParser.identifier ~ MermaidParser.ws ~ MermaidParser.styleProperties)
      .map { (name, props) => ErStatement.ClassDefSt(name, props) }

  private def style(using P[Any]): P[ErStatement.StyleSt] =
    P("style" ~ MermaidParser.ws ~ name ~ MermaidParser.ws ~ MermaidParser.styleProperties).map { (id, props) =>
      ErStatement.StyleSt(id, props)
    }

  private def entity(using P[Any]): P[ErStatement.Entity] =
    P(name ~ MermaidParser.ws ~ "{" ~ (!"}" ~ AnyChar).rep.! ~ "}").map { (id, text) =>
      val attrs = text.linesIterator.map(_.trim).filter(_.nonEmpty).map(parseAttribute).toList
      ErStatement.Entity(id, attrs)
    }

  private def relation(using P[Any]): P[ErStatement.Relation] =
    P(
      name ~ MermaidParser.ws ~ foot ~ lineKind ~ foot ~ MermaidParser.ws ~ name ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ rest
    )
      .map { case (from, left, dashed, right, to, label) =>
        ErStatement.Relation(from, to, left, right, dashed, label)
      }

  def parseAttribute(raw: String): ErAttribute =
    val comment  = "\"([^\"]*)\"".r.findFirstMatchIn(raw).map(_.group(1))
    val stripped = comment match
      case Some(_) => "\"[^\"]*\"".r.replaceAllIn(raw, "").trim
      case None    => raw.trim
    val tokens = stripped.split("\\s+").toList.filter(_.nonEmpty)
    val keys   = tokens.filter(token => token == "PK" || token == "FK" || token == "PK," || token == "FK,")
    val body   =
      tokens.filterNot(token => token == "PK" || token == "FK" || token == "PK," || token == "FK," || token == ",")
    val typeName = body.headOption.getOrElse("")
    val name     = body.drop(1).headOption.getOrElse(typeName)
    ErAttribute(
      typeName,
      name,
      primary = keys.exists(_.startsWith("PK")),
      foreign = keys.exists(_.startsWith("FK")),
      comment = comment,
    )
  end parseAttribute

  private def foot(using P[Any]): P[MarkKind] =
    P(
      "|o".!.map(_ => MarkKind.ZeroOrOne) |
        "o|".!.map(_ => MarkKind.ZeroOrOne) |
        "||".!.map(_ => MarkKind.ExactlyOne) |
        "}o".!.map(_ => MarkKind.ZeroOrMore) |
        "o{".!.map(_ => MarkKind.ZeroOrMore) |
        "}|".!.map(_ => MarkKind.OneOrMore) |
        "|{".!.map(_ => MarkKind.OneOrMore)
    )

  private def lineKind(using P[Any]): P[Boolean] =
    P("..".!.map(_ => true) | "--".!.map(_ => false))

  private def name(using P[Any]): P[NodeId] =
    P(
      ("\"" ~ CharsWhile(_ != '"', 1).! ~ "\"") | CharsWhile(c => c.isLetterOrDigit || c == '_' || c == '-' || c == '.')
        .rep(1)
        .!
    )
      .map(NodeId.trusted)

  private def rest(using P[Any]): P[String] =
    P(CharsWhile(c => c != '\n' && c != '\r', 1).!).map(_.trim)
end ErParser

private[mermoid] object ErModel:

  def resolve(diagram: Diagram.ErDiagram): Either[ParseError, ResolvedEr] =
    val acc = diagram.statements.foldLeft(Acc.empty)(add)
    Right(
      ResolvedEr(
        entities = acc.entities.values.toList.sortBy(_.id.value),
        edges = acc.edges,
        styles = acc.styles,
        classes = acc.classes,
        classDefRules = acc.classDefs.toList.flatMap((name, styles) => StyleResolver.classDefRulesFor(name, styles)),
        accTitle = acc.accTitle,
        accDescr = acc.accDescr,
      )
    )
  end resolve

  def nodeIds(resolved: ResolvedEr): Set[String] = resolved.entities.map(_.id.value).toSet

  case class EntityNode(
      id: NodeId,
      attributes: List[ErAttribute],
      cssClasses: List[String],
      styles: Map[CssProperty, String],
  )

  case class ResolvedEr(
      entities: List[EntityNode],
      edges: List[Edge],
      styles: Map[NodeId, Map[CssProperty, String]],
      classes: Map[NodeId, List[String]],
      classDefRules: List[mermoid.css.CssRule],
      accTitle: Option[String],
      accDescr: Option[String],
  )

  private case class Acc(
      entities: Map[NodeId, EntityNode],
      edges: List[Edge],
      styles: Map[NodeId, Map[CssProperty, String]],
      classes: Map[NodeId, List[String]],
      classDefs: Map[String, Map[CssProperty, String]],
      accTitle: Option[String],
      accDescr: Option[String],
  )

  private object Acc:
    val empty: Acc = Acc(Map.empty, Nil, Map.empty, Map.empty, Map.empty, None, None)

  private def add(acc: Acc, stmt: ErStatement): Acc = stmt match
    case ErStatement.Entity(id, attributes) =>
      val prev = acc.entities.getOrElse(id, EntityNode(id, Nil, Nil, Map.empty))
      acc.copy(entities = acc.entities + (id -> prev.copy(attributes = prev.attributes ++ attributes)))
    case ErStatement.Relation(from, to, left, right, dashed, label) =>
      val withEnds = ensure(ensure(acc, from), to)
      val edge     = Edge(
        from,
        to,
        if dashed then EdgeStyle.DottedOpen else EdgeStyle.Open,
        Some(label),
        mark = Some(RelationMark(Some(left), Some(right), dashed)),
      )
      withEnds.copy(edges = withEnds.edges :+ edge)
    case ErStatement.StyleSt(id, styles) =>
      val base = ensure(acc, id)
      val prev = base.entities.getOrElse(id, EntityNode(id, Nil, Nil, Map.empty))
      base.copy(
        entities = base.entities + (id -> prev.copy(styles = prev.styles ++ styles)),
        styles = base.styles + (id     -> (base.styles.getOrElse(id, Map.empty) ++ styles)),
      )
    case ErStatement.ClassDefSt(name, styles) =>
      acc.copy(classDefs = acc.classDefs + (name -> styles))
    case ErStatement.AccTitle(text) => acc.copy(accTitle = Some(text))
    case ErStatement.AccDescr(text) => acc.copy(accDescr = Some(text))

  private def ensure(acc: Acc, id: NodeId): Acc =
    if acc.entities.contains(id) then acc
    else acc.copy(entities = acc.entities + (id -> EntityNode(id, Nil, Nil, Map.empty)))

  def scene(resolved: ResolvedEr, config: RenderConfig, viewport: Option[Viewport]): DiagramScene =
    val measure = TextMeasure.resolve(config)
    val dir     = DiagramLayout.effectiveDirection(Direction.TB, config.responsive, viewport)
    val defs    = resolved.entities.map { entity =>
      entity.id -> NodeDef(entity.id, Some(entity.id.value), NodeShape.Rect)
    }.toMap
    val sizes = resolved.entities.map { entity =>
      entity.id -> entitySize(entity, config, measure)
    }.toMap
    val laid  = Layout.layout(config.layout, dir, defs, resolved.edges, sizes, Some(measure))
    val nodes = laid.nodes.map { node =>
      val entity = resolved.entities.find(_.id == node.id)
      node.copy(
        cssClasses = entity.map(_.cssClasses).getOrElse(Nil),
        styles = entity.map(_.styles).getOrElse(Map.empty),
      )
    }
    val boxes    = resolved.entities.map(entity => entityBox(entity, config))
    val edges    = SvgRenderer.buildLayoutEdges(resolved.edges)
    val nodeMap  = nodes.map(n => n.id -> n).toMap
    val loopSide = SelfLoopSide.Right
    val ink      =
      nodes.filter(!_.dummy).map(InkBox.fromNode) ++
        laid.routes.values.flatten.map(InkBox.fromPoint(_)) ++
        edges.flatMap(e =>
          EdgeRenderer.edgeInk(config, e, nodeMap, laid.routes.getOrElse((e.from, e.to), Nil), loopSide)
        )
    val fitted                = LayoutBounds.fit(config.layout.padding, ink)
    def move(p: Point): Point = Point(p.x + fitted.shiftX, p.y + fitted.shiftY)
    DiagramScene(
      width = fitted.width,
      height = fitted.height,
      nodes =
        if fitted.shiftX == 0 && fitted.shiftY == 0 then nodes else nodes.map(n => n.copy(center = move(n.center))),
      edges = edges,
      routes = if fitted.shiftX == 0 && fitted.shiftY == 0 then laid.routes
      else laid.routes.map((k, pts) => k -> pts.map(move)),
      subgraphs = Nil,
      notes = Nil,
      interactions = Map.empty,
      loopSide = loopSide,
      classDefRules = resolved.classDefRules,
      config = config,
      direction = dir,
      accTitle = resolved.accTitle,
      accDescr = resolved.accDescr,
      compartments = boxes,
    )
  end scene

  private def entitySize(entity: EntityNode, config: RenderConfig, measure: TextMeasure): (Double, Double) =
    val lc     = config.layout
    val lines  = entity.id.value :: entity.attributes.map(showAttribute)
    val textW  = lines.map(line => measure.width(line, lc.fontSize.toDouble, lc.fontFamily)).maxOption.getOrElse(0.0)
    val width  = Math.max(lc.minNodeWidth, textW + lc.nodePaddingH * 2)
    val height = lc.lineHeight + 16 + Math.max(lc.lineHeight, entity.attributes.size * lc.lineHeight) + 8
    (width, height)

  private def entityBox(entity: EntityNode, config: RenderConfig): CompartmentBox =
    val line = config.layout.lineHeight
    CompartmentBox(
      entity.id,
      None,
      entity.id.value,
      entity.attributes.map(showAttribute),
      Nil,
      line + 10,
      Math.max(line, entity.attributes.size * line) + 8,
      0,
      entity = true,
    )
  end entityBox

  def showAttribute(attribute: ErAttribute): String =
    val keys =
      (if attribute.primary then List("PK") else Nil) ++ (if attribute.foreign then List("FK") else Nil)
    val key = if keys.isEmpty then "" else keys.mkString("", ",", " ")
    s"$key${attribute.typeName} ${attribute.name}"
end ErModel
