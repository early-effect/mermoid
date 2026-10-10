package mermoid

import fastparse.*
import fastparse.NoWhitespace.*
import mermoid.css.CssProperty

enum ClassVisibility:
  case Public, Private, Protected, Package

enum ClassClassifier:
  case None, Abstract, Static

/** One attribute or operation. `()` is what makes it an operation. */
case class ClassMember(
    visibility: Option[ClassVisibility],
    operation: Boolean,
    name: String,
    typeName: Option[String],
    parameters: Option[String],
    returnType: Option[String],
    classifier: ClassClassifier,
)

enum ClassStatement:
  case Declare(
      id: NodeId,
      label: Option[String],
      generic: Option[String],
      stereotype: Option[String],
      members: List[ClassMember],
      cssClasses: List[String],
  )
  case Member(id: NodeId, member: ClassMember)
  case Stereotype(id: NodeId, name: String)
  case Relation(
      from: NodeId,
      to: NodeId,
      mark: RelationMark,
      label: Option[String],
      fromCard: Option[String],
      toCard: Option[String],
  )
  case Namespace(id: NodeId, label: Option[String], statements: List[ClassStatement])
  case Note(owner: Option[NodeId], text: String)
  case ClickSt(binding: ClickBinding)
  case ClassDefSt(name: String, styles: Map[CssProperty, String])
  case StyleSt(id: NodeId, styles: Map[CssProperty, String])
  case HideEmptyMembers
  case AccTitle(text: String)
  case AccDescr(text: String)
end ClassStatement

private[mermoid] case class ClassNode(
    id: NodeId,
    title: String,
    stereotype: Option[String],
    generic: Option[String],
    attributes: List[ClassMember],
    operations: List[ClassMember],
    cssClasses: List[String],
    styles: Map[CssProperty, String],
    interaction: Option[NodeInteraction],
)

private[mermoid] case class ClassNamespace(
    id: NodeId,
    label: Option[String],
    nodes: List[ClassNode],
    children: List[ClassNamespace],
    edges: List[Edge],
)

private[mermoid] case class ResolvedClass(
    direction: Direction,
    nodes: List[ClassNode],
    edges: List[Edge],
    namespaces: List[ClassNamespace],
    hideEmpty: Boolean,
    classDefRules: List[mermoid.css.CssRule],
    accTitle: Option[String],
    accDescr: Option[String],
)

private[mermoid] object ClassMembers:

  def parse(raw: String): ClassMember =
    val trimmed                    = raw.trim
    val (visibility, afterVis)     = splitVisibility(trimmed)
    val (classifier, afterClassif) = splitClassifier(afterVis)
    val open                       = afterClassif.indexOf('(')
    val close                      = afterClassif.lastIndexOf(')')
    if open >= 0 && close > open then
      val name   = afterClassif.take(open).trim
      val params = afterClassif.substring(open + 1, close).trim
      val ret    = afterClassif.drop(close + 1).trim
      ClassMember(
        visibility,
        operation = true,
        name = name,
        typeName = None,
        parameters = Option.when(params.nonEmpty)(params),
        returnType = Option.when(ret.nonEmpty)(ret),
        classifier = classifier,
      )
    else
      val (typeName, name) = splitTypeAndName(afterClassif)
      ClassMember(visibility, operation = false, name, typeName, None, None, classifier)
    end if
  end parse

  private def splitVisibility(raw: String): (Option[ClassVisibility], String) =
    raw.headOption match
      case Some('+') => (Some(ClassVisibility.Public), raw.drop(1).trim)
      case Some('-') => (Some(ClassVisibility.Private), raw.drop(1).trim)
      case Some('#') => (Some(ClassVisibility.Protected), raw.drop(1).trim)
      case Some('~') => (Some(ClassVisibility.Package), raw.drop(1).trim)
      case _         => (None, raw)

  private def splitClassifier(raw: String): (ClassClassifier, String) =
    if raw.endsWith("*") then (ClassClassifier.Abstract, raw.dropRight(1).trim)
    else if raw.endsWith("$") then (ClassClassifier.Static, raw.dropRight(1).trim)
    else (ClassClassifier.None, raw)

  /** Last space that is not inside `~ ~` separates a type from a name. */
  private def splitTypeAndName(raw: String): (Option[String], String) =
    val (_, last) = raw.zipWithIndex.foldLeft((0, Option.empty[Int])) { case ((depth, last), (ch, index)) =>
      val next  = if ch == '~' then 1 - depth else depth
      val space = if ch == ' ' && next == 0 then Some(index) else last
      (if ch == '~' then next else depth, space)
    }
    last match
      case Some(index) =>
        val typeName = raw.take(index).trim
        val name     = raw.drop(index + 1).trim
        (Option.when(typeName.nonEmpty)(typeName), if name.nonEmpty then name else raw.trim)
      case None => (None, raw.trim)
  end splitTypeAndName
end ClassMembers

private[mermoid] object ClassParser:

  def document(using P[Any]): P[Diagram.ClassDiagram] =
    P(MermaidParser.wsnl ~ "classDiagram" ~ MermaidParser.nl ~ body ~ MermaidParser.wsnl ~ End).map { stmts =>
      val dir = stmts.collect { case Line.Dir(d) => d }.lastOption.getOrElse(Direction.TB)
      Diagram.ClassDiagram(dir, stmts.collect { case Line.Stmt(s) => s }.toList)
    }

  private enum Line:
    case Dir(direction: Direction)
    case Stmt(statement: ClassStatement)

  private def body(using P[Any]): P[List[Line]] =
    P(MermaidParser.wsnl ~ line.rep(sep = MermaidParser.sep) ~ MermaidParser.wsnl).map(_.toList)

  private def line(using P[Any]): P[Line] =
    P(direction.map(Line.Dir(_)) | statement.map(Line.Stmt(_)))

  private def direction(using P[Any]): P[Direction] =
    P(
      "direction" ~ CharsWhileIn(" \t", 1) ~ MermaidParser.direction ~
        !CharPred(c => c.isLetterOrDigit || c == '_')
    )

  private def statement(using P[Any]): P[ClassStatement] =
    P(
      hideEmpty | accTitle | accDescr | note | click | classDef | style | namespace | classDecl | stereoLine |
        relation | memberOf
    )

  private def hideEmpty(using P[Any]): P[ClassStatement] =
    P("hideEmptyMembersBox").map(_ => ClassStatement.HideEmptyMembers)

  private def accTitle(using P[Any]): P[ClassStatement.AccTitle] =
    P("accTitle" ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ rest).map(ClassStatement.AccTitle(_))

  private def accDescr(using P[Any]): P[ClassStatement.AccDescr] =
    P("accDescr" ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ rest).map(ClassStatement.AccDescr(_))

  private def note(using P[Any]): P[ClassStatement] =
    P(
      ("note" ~ MermaidParser.ws ~ "for" ~ MermaidParser.ws ~ name ~ MermaidParser.ws ~ quoted).map { (id, text) =>
        ClassStatement.Note(Some(id), text)
      } |
        ("note" ~ MermaidParser.ws ~ quoted).map(text => ClassStatement.Note(None, text))
    )

  private def click(using P[Any]): P[ClassStatement.ClickSt] =
    P(("click" | "link" | "callback") ~ MermaidParser.ws ~ name ~ MermaidParser.ws ~ rest).map { (id, tail) =>
      ClassStatement.ClickSt(MermaidParser.parseClickRest(id, tail))
    }

  private def classDef(using P[Any]): P[ClassStatement.ClassDefSt] =
    P("classDef" ~ MermaidParser.ws ~ MermaidParser.identifier ~ MermaidParser.ws ~ MermaidParser.styleProperties)
      .map { (n, props) => ClassStatement.ClassDefSt(n, props) }

  private def style(using P[Any]): P[ClassStatement.StyleSt] =
    P("style" ~ MermaidParser.ws ~ name ~ MermaidParser.ws ~ MermaidParser.styleProperties).map { (id, props) =>
      ClassStatement.StyleSt(id, props)
    }

  private def namespace(using P[Any]): P[ClassStatement.Namespace] =
    P("namespace" ~ MermaidParser.ws ~ name ~ bracketLabel.? ~ MermaidParser.ws ~ block).map { (id, label, stmts) =>
      ClassStatement.Namespace(id, label, stmts)
    }

  private def block(using P[Any]): P[List[ClassStatement]] =
    P("{" ~ body ~ "}").map(_.collect { case Line.Stmt(s) => s })

  private def classDecl(using P[Any]): P[ClassStatement.Declare] =
    P(
      "class" ~ MermaidParser.ws ~ name ~ generic.? ~ bracketLabel.? ~ MermaidParser.ws ~ stereotype.? ~
        MermaidParser.classSuffix.? ~ MermaidParser.ws ~ memberBlock.?
    ).map { case (id, gen, label, stereo, classes, members) =>
      ClassStatement.Declare(id, label, gen, stereo, members.getOrElse(Nil), classes.getOrElse(Nil))
    }

  private def memberBlock(using P[Any]): P[List[ClassMember]] =
    P("{" ~ (!"}" ~ AnyChar).rep.! ~ "}").map { text =>
      text.linesIterator.map(_.trim).filter(_.nonEmpty).map(ClassMembers.parse).toList
    }

  private def stereoLine(using P[Any]): P[ClassStatement.Stereotype] =
    P(stereotype ~ MermaidParser.ws ~ name).map { (stereo, id) => ClassStatement.Stereotype(id, stereo) }

  private def relation(using P[Any]): P[ClassStatement.Relation] =
    P(name ~ card ~ relToken ~ card ~ name ~ (MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ rest).?).map {
      case (from, fromCard, mark, toCard, to, label) =>
        ClassStatement.Relation(from, to, mark, label, fromCard, toCard)
    }

  private def memberOf(using P[Any]): P[ClassStatement.Member] =
    P(name ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ rest).map { (id, text) =>
      ClassStatement.Member(id, ClassMembers.parse(text))
    }

  private def card(using P[Any]): P[Option[String]] =
    P((MermaidParser.ws ~ quoted).? ~ MermaidParser.ws)

  private def relToken(using P[Any]): P[RelationMark] =
    P(
      "<|--|>".!.map(_ => RelationMark(Some(MarkKind.Triangle), Some(MarkKind.Triangle), false)) |
        "<|--".!.map(_ => RelationMark(Some(MarkKind.Triangle), None, false)) |
        "--|>".!.map(_ => RelationMark(None, Some(MarkKind.Triangle), false)) |
        "*--".!.map(_ => RelationMark(Some(MarkKind.Diamond), None, false)) |
        "--*".!.map(_ => RelationMark(None, Some(MarkKind.Diamond), false)) |
        "o--".!.map(_ => RelationMark(Some(MarkKind.OpenDiamond), None, false)) |
        "--o".!.map(_ => RelationMark(None, Some(MarkKind.OpenDiamond), false)) |
        "<|..".!.map(_ => RelationMark(Some(MarkKind.Triangle), None, true)) |
        "..|>".!.map(_ => RelationMark(None, Some(MarkKind.Triangle), true)) |
        "..>".!.map(_ => RelationMark(None, Some(MarkKind.Arrow), true)) |
        "<..".!.map(_ => RelationMark(Some(MarkKind.Arrow), None, true)) |
        "()--".!.map(_ => RelationMark(Some(MarkKind.Lollipop), None, false)) |
        "--()".!.map(_ => RelationMark(None, Some(MarkKind.Lollipop), false)) |
        "-->".!.map(_ => RelationMark(None, Some(MarkKind.Arrow), false)) |
        "<--".!.map(_ => RelationMark(Some(MarkKind.Arrow), None, false)) |
        "..".!.map(_ => RelationMark(None, None, true)) |
        "--".!.map(_ => RelationMark(None, None, false))
    )

  private def stereotype(using P[Any]): P[String] =
    P("<<" ~ MermaidParser.ws ~ CharsWhile(c => c != '>' && c != '\n').! ~ MermaidParser.ws ~ ">>").map(_.trim)

  private def generic(using P[Any]): P[String] =
    P("~" ~ CharsWhile(c => c != '~' && c != '\n' && c != '{').! ~ "~")

  private def bracketLabel(using P[Any]): P[String] =
    P("[" ~ quoted ~ "]")

  private def quoted(using P[Any]): P[String] =
    P("\"" ~ CharsWhile(_ != '"', 0).! ~ "\"")

  private def name(using P[Any]): P[NodeId] =
    P(quoted | rawName).map(NodeId.trusted)

  private def rawName(using P[Any]): P[String] =
    P(CharsWhile(c => c.isLetterOrDigit || c == '_' || c == '-' || c == '.').rep(1).!)

  private def rest(using P[Any]): P[String] =
    P(CharsWhile(c => c != '\n' && c != '\r', 1).!).map { raw =>
      val cut = raw.indexOf("%%")
      (if cut >= 0 then raw.take(cut) else raw).trim
    }
end ClassParser
