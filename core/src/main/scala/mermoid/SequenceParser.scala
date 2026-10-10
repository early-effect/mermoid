package mermoid

import fastparse.*
import fastparse.NoWhitespace.*

/** `sequenceDiagram` syntax. Statement order is time. `else` belongs to `alt`, `and` belongs to `par`. */
private[mermoid] object SequenceParser:

  def diagram(using P[Any]): P[Diagram] =
    P(MermaidParser.wsnl ~ "sequenceDiagram" ~ MermaidParser.nl ~/ body ~ End).map(Diagram.Sequence(_))

  private def body(using P[Any]): P[List[SequenceStatement]] =
    P(sequenceStatement.rep(sep = MermaidParser.sep) ~ MermaidParser.wsnl).map(_.toList)

  private def sequenceStatement(using P[Any]): P[SequenceStatement] =
    P(
      accTitle | accDescr | linkLine | click | classDef | classApply | style | box | created | destroyed | declare |
        numbering | activation | note | group | message
    )

  private def keyword(word: String)(using P[Any]): P[Unit] =
    P(word ~ !CharPred(c => c.isLetterOrDigit || c == '_'))

  private def lineRest(using P[Any]): P[String] =
    P(CharsWhile(c => c != '\n' && c != '\r', 0).!)

  private def intToken(using P[Any]): P[Int] =
    P(("-".? ~ CharsWhileIn("0-9", 1)).!).flatMap { raw =>
      raw.toIntOption match
        case Some(n) => Pass(n)
        case None    => Fail
    }

  private def doubleToken(using P[Any]): P[Double] =
    P(("-".? ~ CharsWhileIn("0-9", 1) ~ ("." ~ CharsWhileIn("0-9", 1)).?).!).flatMap { raw =>
      raw.toDoubleOption match
        case Some(n) => Pass(n)
        case None    => Fail
    }

  private def id(using P[Any]): P[NodeId] =
    P(MermaidParser.identifier).map(NodeId.trusted(_))

  private def declare(using P[Any]): P[SequenceStatement] =
    P(keywordKind ~ MermaidParser.ws ~ id ~ alias.? ~ stereoSuffix).map { case (kind, pid, label, stereo) =>
      named(pid, label, kind, stereo, created = false)
    }

  private def created(using P[Any]): P[SequenceStatement] =
    P(keyword("create") ~ MermaidParser.ws ~ keywordKind ~ MermaidParser.ws ~ id ~ alias.? ~ stereoSuffix).map {
      case (kind, pid, label, stereo) =>
        named(pid, label, kind, stereo, created = true)
    }

  private def destroyed(using P[Any]): P[SequenceStatement.Destroy] =
    P(keyword("destroy") ~ MermaidParser.ws ~ id).map(SequenceStatement.Destroy(_))

  private enum StereoRead:
    case Absent
    case Known(value: SequenceStereotype)
    case Unknown(raw: String)

  private def named(
      pid: NodeId,
      label: Option[String],
      kind: ParticipantKind,
      stereo: StereoRead,
      created: Boolean,
  ): SequenceStatement =
    val text = label.map(_.trim).filter(_.nonEmpty)
    stereo match
      case StereoRead.Unknown(raw) => SequenceStatement.BadStereotype(raw)
      case StereoRead.Absent       =>
        if created then SequenceStatement.Create(pid, text, kind, None)
        else SequenceStatement.Declare(pid, text, kind, None)
      case StereoRead.Known(value) =>
        if created then SequenceStatement.Create(pid, text, kind, Some(value))
        else SequenceStatement.Declare(pid, text, kind, Some(value))
  end named

  private def stereoSuffix(using P[Any]): P[StereoRead] =
    P((MermaidParser.ws ~ "@{" ~ CharsWhile(_ != '}', 0).! ~ "}").?).map {
      case None       => StereoRead.Absent
      case Some(body) => readStereo(body)
    }

  private def readStereo(body: String): StereoRead =
    val lower = body.toLowerCase
    val key   = "\"type\""
    val at    = lower.indexOf(key)
    if at < 0 then StereoRead.Absent
    else
      val after = body.substring(at + key.length).dropWhile(c => c.isWhitespace || c == ':' || c == '"' || c == '\'')
      val raw   = after.takeWhile(c => c != '"' && c != '\'' && c != ',' && c != '}').trim
      if raw.isEmpty then StereoRead.Absent
      else
        SequenceStereotype.parse(raw) match
          case Some(value) => StereoRead.Known(value)
          case None        => StereoRead.Unknown(raw)
  end readStereo

  private def keywordKind(using P[Any]): P[ParticipantKind] =
    P(
      keyword("participant").map(_ => ParticipantKind.Participant) |
        keyword("actor").map(_ => ParticipantKind.Actor)
    )

  private def alias(using P[Any]): P[String] =
    P(
      MermaidParser.ws ~ keyword("as") ~ MermaidParser.ws ~
        (MermaidParser.quotedString | CharsWhile(c => c != '\n' && c != '\r' && c != '@', 1).!)
    )

  private def numbering(using P[Any]): P[SequenceStatement.Autonumber] =
    P("autonumber" ~ MermaidParser.ws ~ numberingMode).map(SequenceStatement.Autonumber(_))

  private def numberingMode(using P[Any]): P[Numbering] =
    P(
      keyword("off").map(_ => Numbering.Off) |
        (intToken ~ MermaidParser.ws ~ intToken).map { (start, step) =>
          Numbering.On(start, if step == 0 then 1 else step)
        } |
        intToken.map(start => Numbering.On(start, 1)) |
        Pass(Numbering.On(1, 1))
    )

  private def activation(using P[Any]): P[SequenceStatement] =
    P((keyword("deactivate").map(_ => false) | keyword("activate").map(_ => true)) ~ MermaidParser.ws ~ id).map {
      case (on, pid) =>
        if on then SequenceStatement.Activate(pid) else SequenceStatement.Deactivate(pid)
    }

  private def note(using P[Any]): P[SequenceStatement.Note] =
    P(IgnoreCase("note") ~ MermaidParser.ws ~ notePlace ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ lineRest).map {
      case (place, text) => SequenceStatement.Note(place, text.trim)
    }

  private def notePlace(using P[Any]): P[NotePlace] =
    P(
      (IgnoreCase("right") ~ MermaidParser.ws ~ IgnoreCase("of") ~ MermaidParser.ws ~ id).map(NotePlace.RightOf(_)) |
        (IgnoreCase("left") ~ MermaidParser.ws ~ IgnoreCase("of") ~ MermaidParser.ws ~ id).map(NotePlace.LeftOf(_)) |
        (IgnoreCase("over") ~ MermaidParser.ws ~ id ~ (MermaidParser.ws ~ "," ~ MermaidParser.ws ~ id).?).map {
          case (a, Some(b)) => NotePlace.Over(a, b)
          case (a, None)    => NotePlace.Over(a, a)
        }
    )

  private def group(using P[Any]): P[SequenceStatement.Group] =
    P(loopGroup | optGroup | criticalGroup | breakGroup | altGroup | parGroup | rectGroup)

  private def block(stop: => P[Unit])(using P[Any]): P[List[SequenceStatement]] =
    P(MermaidParser.wsnl ~ (!stop ~ sequenceStatement).rep(sep = MermaidParser.sep) ~ MermaidParser.wsnl).map(_.toList)

  private def oneSection(label: String, body: List[SequenceStatement]): GroupSection =
    GroupSection(Option(label.trim).filter(_.nonEmpty), body)

  private def loopGroup(using P[Any]): P[SequenceStatement.Group] =
    P(keyword("loop") ~ MermaidParser.ws ~ lineRest ~ block(keyword("end")) ~ keyword("end")).map { (label, stmts) =>
      SequenceStatement.Group(GroupKind.Loop(label.trim), List(oneSection(label, stmts)))
    }

  private def optGroup(using P[Any]): P[SequenceStatement.Group] =
    P(keyword("opt") ~ MermaidParser.ws ~ lineRest ~ block(keyword("end")) ~ keyword("end")).map { (label, stmts) =>
      SequenceStatement.Group(GroupKind.Opt(label.trim), List(oneSection(label, stmts)))
    }

  private def criticalGroup(using P[Any]): P[SequenceStatement.Group] =
    P(keyword("critical") ~ MermaidParser.ws ~ lineRest ~ block(keyword("end")) ~ keyword("end")).map {
      (label, stmts) =>
        SequenceStatement.Group(GroupKind.Critical(label.trim), List(oneSection(label, stmts)))
    }

  private def breakGroup(using P[Any]): P[SequenceStatement.Group] =
    P(keyword("break") ~ MermaidParser.ws ~ lineRest ~ block(keyword("end")) ~ keyword("end")).map { (label, stmts) =>
      SequenceStatement.Group(GroupKind.Break(label.trim), List(oneSection(label, stmts)))
    }

  private def altStop(using P[Any]): P[Unit] =
    P(keyword("else") | keyword("end"))

  private def parStop(using P[Any]): P[Unit] =
    P(keyword("and") | keyword("end"))

  private def altGroup(using P[Any]): P[SequenceStatement.Group] =
    P(
      keyword("alt") ~ MermaidParser.ws ~ lineRest ~ block(altStop) ~
        (keyword("else") ~ MermaidParser.ws ~ lineRest ~ block(altStop)).rep ~
        keyword("end")
    ).map { case (label, first, rest) =>
      val sections = oneSection(label, first) :: rest.map { (l, b) => oneSection(l, b) }.toList
      SequenceStatement.Group(GroupKind.Alt, sections)
    }
  end altGroup

  private def parGroup(using P[Any]): P[SequenceStatement.Group] =
    P(
      keyword("par") ~ MermaidParser.ws ~ lineRest ~ block(parStop) ~
        (keyword("and") ~ MermaidParser.ws ~ lineRest ~ block(parStop)).rep ~
        keyword("end")
    ).map { case (label, first, rest) =>
      val sections = oneSection(label, first) :: rest.map { (l, b) => oneSection(l, b) }.toList
      SequenceStatement.Group(GroupKind.Par, sections)
    }
  end parGroup

  private def box(using P[Any]): P[SequenceStatement.Box] =
    P(keyword("box") ~ MermaidParser.ws ~ boxColor ~ lineRest ~ block(keyword("end")) ~ keyword("end")).map {
      (fill, title, body) =>
        SequenceStatement.Box(Option(title.trim).filter(_.nonEmpty), fill, body)
    }

  private def boxColor(using P[Any]): P[String] =
    P(
      rgbColor.map(_.show) |
        ("#" ~ CharsWhileIn("0-9A-Fa-f", 3)).!.map(hex => s"#$hex") |
        CharsWhile(_.isLetter, 1).!
    )

  private def linkLine(using P[Any]): P[SequenceStatement.Link] =
    P((keyword("links") | keyword("link")) ~ MermaidParser.ws ~ id ~ MermaidParser.ws ~ ":" ~ lineRest).map {
      (pid, rest) =>
        SequenceStatement.Link(pid, splitLinks(rest))
    }

  private def splitLinks(rest: String): List[(String, String)] =
    rest.split(',').toList.map(_.trim).filter(_.nonEmpty).flatMap { part =>
      val at = part.lastIndexOf(" @ ")
      if at < 0 then None
      else
        val label = part.substring(0, at).trim
        val href  = part.substring(at + 3).trim
        if label.isEmpty || href.isEmpty then None else Some((label, href))
    }

  private def click(using P[Any]): P[SequenceStatement.ClickSt] =
    P(keyword("click") ~ MermaidParser.ws ~ id ~ MermaidParser.ws ~ lineRest).map { (pid, rest) =>
      SequenceStatement.ClickSt(MermaidParser.parseClickRest(pid, rest.trim))
    }

  private def classDef(using P[Any]): P[SequenceStatement.ClassDefSt] =
    P(
      keyword(
        "classDef"
      ) ~ MermaidParser.ws ~ MermaidParser.identifier ~ MermaidParser.ws ~ MermaidParser.styleProperties
    )
      .map { (name, props) =>
        SequenceStatement.ClassDefSt(name, props)
      }

  private def classApply(using P[Any]): P[SequenceStatement.ClassSt] =
    P(
      keyword("class") ~ MermaidParser.ws ~ id.rep(sep = MermaidParser.ws ~ "," ~ MermaidParser.ws, min = 1) ~
        MermaidParser.ws ~ MermaidParser.identifier
    ).map { (ids, name) =>
      SequenceStatement.ClassSt(ids.toList, name)
    }

  private def style(using P[Any]): P[SequenceStatement.StyleSt] =
    P(keyword("style") ~ MermaidParser.ws ~ id ~ MermaidParser.ws ~ MermaidParser.styleProperties).map { (pid, props) =>
      SequenceStatement.StyleSt(pid, props)
    }

  private def accTitle(using P[Any]): P[SequenceStatement.AccTitle] =
    P(keyword("accTitle") ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ lineRest).map(text =>
      SequenceStatement.AccTitle(text.trim)
    )

  private def accDescr(using P[Any]): P[SequenceStatement] =
    P(
      (keyword("accDescr") ~ MermaidParser.ws ~ "{" ~ (!"}" ~ AnyChar).rep.! ~ "}").map(text =>
        SequenceStatement.AccDescr(text.trim)
      ) |
        (keyword("accDescr") ~ MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ lineRest).map(text =>
          SequenceStatement.AccDescr(text.trim)
        )
    )

  private def rectGroup(using P[Any]): P[SequenceStatement.Group] =
    P(keyword("rect") ~ MermaidParser.ws ~ rgbColor ~ block(keyword("end")) ~ keyword("end")).map { (color, stmts) =>
      SequenceStatement.Group(GroupKind.Highlight(color), List(GroupSection(None, stmts)))
    }

  private def rgbColor(using P[Any]): P[Rgb] =
    P(
      ("rgba(" ~ MermaidParser.ws ~ intToken ~ MermaidParser.ws ~ "," ~ MermaidParser.ws ~ intToken ~
        MermaidParser.ws ~ "," ~ MermaidParser.ws ~ intToken ~ MermaidParser.ws ~ "," ~ MermaidParser.ws ~
        doubleToken ~ MermaidParser.ws ~ ")").map { (r, g, b, a) => Rgb(r, g, b, Some(a)) } |
        ("rgb(" ~ MermaidParser.ws ~ intToken ~ MermaidParser.ws ~ "," ~ MermaidParser.ws ~ intToken ~
          MermaidParser.ws ~ "," ~ MermaidParser.ws ~ intToken ~ MermaidParser.ws ~ ")").map { (r, g, b) =>
          Rgb(r, g, b, None)
        }
    )

  private def message(using P[Any]): P[SequenceStatement.Message] =
    P(id ~ MermaidParser.ws ~ arrow ~ MermaidParser.ws ~ control ~ MermaidParser.ws ~ id ~ messageText).map {
      case (from, arr, ctrl, to, text) => SequenceStatement.Message(from, to, arr, text, ctrl)
    }

  private def messageText(using P[Any]): P[Option[String]] =
    P((MermaidParser.ws ~ ":" ~ MermaidParser.ws ~ lineRest).?).map(_.map(_.trim).filter(_.nonEmpty))

  private def control(using P[Any]): P[MessageControl] =
    P(
      "+".!.map(_ => MessageControl.Activate) |
        "-".!.map(_ => MessageControl.Deactivate) |
        Pass(MessageControl.None)
    )

  private def arrow(using P[Any]): P[SequenceArrow] =
    P(
      "<<-->>".!.map(_ => SequenceArrow.DashedBoth) |
        "<<->>".!.map(_ => SequenceArrow.SolidBoth) |
        "-->>".!.map(_ => SequenceArrow.DashedHead) |
        "-->".!.map(_ => SequenceArrow.Dashed) |
        "->>".!.map(_ => SequenceArrow.SolidHead) |
        "--x".!.map(_ => SequenceArrow.DashedCross) |
        "--)".!.map(_ => SequenceArrow.DashedOpen) |
        "->".!.map(_ => SequenceArrow.Solid) |
        "-x".!.map(_ => SequenceArrow.SolidCross) |
        "-)".!.map(_ => SequenceArrow.SolidOpen)
    )
end SequenceParser
