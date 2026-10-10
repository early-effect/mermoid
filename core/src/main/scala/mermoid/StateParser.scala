package mermoid

import fastparse.*
import fastparse.NoWhitespace.*

/** `stateDiagram` and `stateDiagram-v2`. The statement list is the source. [[StateModel]] checks it. */
private[mermoid] object StateParser:

  def document(using P[Any]): P[Diagram.StateDiagram] =
    P(MermaidParser.wsnl ~ header ~ scope ~ MermaidParser.wsnl ~ End).map { (dir, stmts) =>
      Diagram.StateDiagram(dir.getOrElse(Direction.TB), stmts)
    }

  def transition(using P[Any]): P[StateStatement.TransitionSt] =
    stateTransition

  def note(using P[Any]): P[StateStatement.NoteSt] =
    noteBlock

  private def header(using P[Any]): P[Unit] =
    P("stateDiagram-v2" | "stateDiagram") ~ MermaidParser.nl

  private enum Line:
    case Dir(direction: Direction)
    case Stmts(statements: List[StateStatement])

  /** Last `direction` in the scope wins. `--` stays a [[StateStatement.Divider]] so the caller can split or reject. */
  private def scope(using P[Any]): P[(Option[Direction], List[StateStatement])] =
    P(MermaidParser.wsnl ~ line.rep(sep = MermaidParser.sep) ~ MermaidParser.wsnl).map { lines =>
      val dir   = lines.collect { case Line.Dir(d) => d }.lastOption
      val stmts = lines.collect { case Line.Stmts(ss) => ss }.toList.flatten
      (dir, stmts)
    }

  private def line(using P[Any]): P[Line] =
    P(directionLine.map(Line.Dir(_)) | statements.map(Line.Stmts(_)))

  private def directionLine(using P[Any]): P[Direction] =
    P(
      "direction" ~ CharsWhileIn(" \t", 1) ~ MermaidParser.direction ~
        !CharPred(c => c.isLetterOrDigit || c == '_')
    )

  private def statements(using P[Any]): P[List[StateStatement]] =
    P(
      noteSt.map(List(_)) | clickSt.map(List(_)) | classDefSt.map(List(_)) | classSt.map(List(_)) |
        styleSt.map(List(_)) | accDescrBlock.map(List(_)) | accDescrLine.map(List(_)) | accTitleLine.map(List(_)) |
        hideEmpty.map(List(_)) | scaleLine.map(List(_)) | divider.map(List(_)) | stateHead | stateTransition.map(
          List(_)
        ) |
        annotatedId
    )

  private def noteSt(using P[Any]): P[StateStatement] =
    P(floatingNote | noteOneLine | noteBlock)

  private def noteBlock(using P[Any]): P[StateStatement.NoteSt] =
    P(
      "note" ~ MermaidParser.ws ~ notePosition ~ MermaidParser.ws ~ MermaidParser.nodeId ~ MermaidParser.asAlias.? ~
        MermaidParser.nl ~ (!("end note") ~ AnyChar).rep.! ~ "end note"
    ).map { case (pos, id, alias, text) =>
      StateStatement.NoteSt(pos, id, text.linesIterator.map(_.trim).filter(_.nonEmpty).mkString("\n"), alias)
    }

  private def noteOneLine(using P[Any]): P[StateStatement.NoteSt] =
    P(
      "note" ~ MermaidParser.ws ~ notePosition ~ MermaidParser.ws ~ MermaidParser.nodeId ~ MermaidParser.ws ~ ":" ~
        MermaidParser.ws ~ lineText
    ).map { case (pos, id, text) =>
      StateStatement.NoteSt(pos, id, text, None)
    }

  private def floatingNote(using P[Any]): P[StateStatement.FloatingNote] =
    P(
      "note" ~ MermaidParser.ws ~ "\"" ~ CharsWhile(_ != '"', 0).! ~ "\"" ~ MermaidParser.ws ~ "as" ~
        MermaidParser.ws ~ MermaidParser.nodeId
    ).map { case (text, id) =>
      StateStatement.FloatingNote(text.trim, id)
    }

  private def notePosition(using P[Any]): P[NotePosition] =
    P("right of".!.map(_ => NotePosition.RightOf) | "left of".!.map(_ => NotePosition.LeftOf))

  private def clickSt(using P[Any]): P[StateStatement.ClickSt] =
    P("click" ~ MermaidParser.ws ~ endpoint ~ MermaidParser.ws ~ CharsWhile(c => c != '\n' && c != '\r', 1).!).map {
      case (id, rest) =>
        StateStatement.ClickSt(MermaidParser.parseClickRest(id, rest.trim))
    }

  private def classDefSt(using P[Any]): P[StateStatement.ClassDefSt] =
    P("classDef" ~ MermaidParser.ws ~ MermaidParser.identifier ~ MermaidParser.ws ~ MermaidParser.styleProperties).map {
      case (name, props) =>
        StateStatement.ClassDefSt(name, props)
    }

  private def classSt(using P[Any]): P[StateStatement.ClassSt] =
    P(
      "class" ~ MermaidParser.ws ~ endpoint.rep(sep = MermaidParser.ws ~ "," ~ MermaidParser.ws, min = 1) ~
        MermaidParser.ws ~ MermaidParser.identifier
    ).map { case (ids, className) =>
      StateStatement.ClassSt(ids.toList, className)
    }

  private def styleSt(using P[Any]): P[StateStatement.StyleSt] =
    P("style" ~ MermaidParser.ws ~ endpoint ~ MermaidParser.ws ~ MermaidParser.styleProperties).map {
      case (id, props) =>
        StateStatement.StyleSt(id, StateStyle.fromProperties(props))
    }

  private def accTitleLine(using P[Any]): P[StateStatement.AccTitle] =
    P("accTitle" ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ lineText).map(StateStatement.AccTitle(_))

  private def accDescrLine(using P[Any]): P[StateStatement.AccDescr] =
    P("accDescr" ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ lineText).map(StateStatement.AccDescr(_))

  private def accDescrBlock(using P[Any]): P[StateStatement.AccDescr] =
    P("accDescr" ~ MermaidParser.ws ~ "{" ~ (!"}" ~ AnyChar).rep.! ~ "}").map { text =>
      StateStatement.AccDescr(text.trim)
    }

  private def hideEmpty(using P[Any]): P[StateStatement] =
    P("hide" ~ MermaidParser.ws ~ "empty" ~ MermaidParser.ws ~ "description").map(_ =>
      StateStatement.HideEmptyDescription
    )

  private def scaleLine(using P[Any]): P[StateStatement.Scale] =
    P("scale" ~ MermaidParser.ws ~ lineText).map(StateStatement.Scale(_))

  /** A concurrency divider, and not the start of `-->`. */
  private def divider(using P[Any]): P[StateStatement] =
    P("--" ~ !CharPred(c => c == '>' || c == '-')).map(_ => StateStatement.Divider)

  private def stateHead(using P[Any]): P[List[StateStatement]] =
    P("state" ~ MermaidParser.ws ~ (quotedHead | idHead))

  private def idHead(using P[Any]): P[List[StateStatement]] =
    P(MermaidParser.nodeId ~ MermaidParser.ws ~ stateTail).map { case (id, tail) =>
      List(tailStatement(id, tail))
    }

  private def quotedHead(using P[Any]): P[List[StateStatement]] =
    P(
      "\"" ~ CharsWhile(_ != '"', 0).! ~ "\"" ~ MermaidParser.ws ~ "as" ~ MermaidParser.ws ~ MermaidParser.nodeId ~
        MermaidParser.ws ~ stateTail
    ).map { case (text, id, tail) =>
      StateStatement.Description(id, text.trim) :: List(tailStatement(id, tail))
    }

  private enum Tail:
    case Stereo(raw: String)
    case Body(direction: Option[Direction], regions: List[List[StateStatement]])
    case Bare

  private def stateTail(using P[Any]): P[Tail] =
    P(
      stereo.map(Tail.Stereo(_)) |
        braced.map { case (dir, regions) => Tail.Body(dir, regions) } |
        Pass.map(_ => Tail.Bare)
    )

  private def tailStatement(id: NodeId, tail: Tail): StateStatement = tail match
    case Tail.Stereo(raw) =>
      knownForm(raw) match
        case Some(form) => StateStatement.Form(id, form)
        case None       => StateStatement.UnknownStereo(id, raw)
    case Tail.Body(dir, regions) => StateStatement.Composite(id, dir, regions)
    case Tail.Bare               => StateStatement.Form(id, StateForm.Simple)

  private def stereo(using P[Any]): P[String] =
    P(("<<" | "[[") ~ MermaidParser.ws ~ MermaidParser.identifier ~ MermaidParser.ws ~ (">>" | "]]"))

  private def knownForm(raw: String): Option[StateForm] = raw match
    case "choice"      => Some(StateForm.Choice)
    case "fork"        => Some(StateForm.Fork)
    case "join"        => Some(StateForm.Join)
    case "history"     => Some(StateForm.ShallowHistory)
    case "deepHistory" => Some(StateForm.DeepHistory)
    case _             => None

  private def braced(using P[Any]): P[(Option[Direction], List[List[StateStatement]])] =
    P("{" ~ scope ~ "}").map { case (dir, stmts) =>
      (dir, splitRegions(stmts))
    }

  private def splitRegions(stmts: List[StateStatement]): List[List[StateStatement]] =
    val (done, current) =
      stmts.foldLeft((List.empty[List[StateStatement]], List.empty[StateStatement])) {
        case ((done, current), StateStatement.Divider) => (current.reverse :: done, Nil)
        case ((done, current), stmt)                   => (done, stmt :: current)
      }
    (current.reverse :: done).reverse

  private def stateTransition(using P[Any]): P[StateStatement.TransitionSt] =
    P(
      endpoint ~ MermaidParser.classSuffix.? ~ MermaidParser.ws ~ "-->" ~ MermaidParser.ws ~ endpoint ~
        MermaidParser.classSuffix.? ~ (MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ lineText).?
    ).map { case (from, fromCls, to, toCls, label) =>
      StateStatement.TransitionSt(
        StateTransition(from, to, label.filter(_.nonEmpty), fromCls.getOrElse(Nil), toCls.getOrElse(Nil))
      )
    }

  /** A bare id, `id : text`, or `id:::class`. A description and classes on the same id both survive. */
  private def annotatedId(using P[Any]): P[List[StateStatement]] =
    P(MermaidParser.nodeId ~ MermaidParser.classSuffix.? ~ (MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ lineText).?)
      .map { case (id, classes, text) =>
        val head = text match
          case Some(body) => StateStatement.Description(id, body)
          case None       => StateStatement.Form(id, StateForm.Simple)
        head :: classes.getOrElse(Nil).map(name => StateStatement.ClassSt(List(id), name))
      }

  private def endpoint(using P[Any]): P[NodeId] =
    P(
      "[H*]".!.map(NodeId.trusted) |
        "[H]".!.map(NodeId.trusted) |
        "[*]".!.map(NodeId.trusted) |
        MermaidParser.nodeId
    )

  /** Rest of the line, with a trailing `%%` comment cut off. */
  private def lineText(using P[Any]): P[String] =
    P(CharsWhile(c => c != '\n' && c != '\r', 0).!).map { raw =>
      val cut  = raw.indexOf("%%")
      val kept = if cut >= 0 then raw.take(cut) else raw
      kept.trim
    }
end StateParser
