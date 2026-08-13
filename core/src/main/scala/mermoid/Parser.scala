package mermoid

import fastparse.*
import fastparse.NoWhitespace.*
import mermoid.css.CssProperty

object MermaidParser:

  // -- Helpers ----------------------------------------------------------------

  private[mermoid] def ws(using P[Any]): P[Unit]             = P(CharsWhileIn(" \t", 0))
  private[mermoid] def nl(using P[Any]): P[Unit]             = P(ws ~ ("\r\n" | "\n") ~ ws)
  private[mermoid] def mermaidComment(using P[Any]): P[Unit] =
    P("%%{" ~ (!"}%%" ~ AnyChar).rep ~ "}%%") |
      P("%%" ~ CharsWhile(c => c != '\n' && c != '\r', 0))
  private[mermoid] def wsnl(using P[Any]): P[Unit] =
    P((CharsWhileIn(" \t\r\n", 1) | mermaidComment).rep)
  private[mermoid] def sep(using P[Any]): P[Unit] = P((ws ~ (";" | nl | mermaidComment) ~ wsnl).rep(1))

  private[mermoid] def identifier(using P[Any]): P[String] =
    P(CharPred(c => c.isLetterOrDigit || c == '_').rep(1).!)

  private[mermoid] def quotedString(using P[Any]): P[String] =
    P("\"" ~ CharsWhile(_ != '"', 0).! ~ "\"")

  /** Unquoted label body up to (but not including) `end`, never crossing a newline. */
  private[mermoid] def labelUntil(end: => P[Unit])(using P[Any]): P[String] =
    P((!(end | "\n") ~ AnyChar).rep(1).!)

  /** Quoted label, or unquoted text stopped at the shape's closer. */
  private[mermoid] def labelOrQuoted(end: => P[Unit])(using P[Any]): P[String] =
    P(quotedString | labelUntil(end))

  private[mermoid] def labelText(using P[Any]): P[String] =
    labelUntil("]")

  // -- Direction --------------------------------------------------------------

  private[mermoid] def direction(using P[Any]): P[Direction] =
    P(
      "TB".!.map(_ => Direction.TB) |
        "TD".!.map(_ => Direction.TD) |
        "BT".!.map(_ => Direction.BT) |
        "LR".!.map(_ => Direction.LR) |
        "RL".!.map(_ => Direction.RL)
    )

  // -- Node shapes ------------------------------------------------------------

  private[mermoid] def nodeShape(using P[Any]): P[(String, NodeShape)] =
    P(
      ("(((" ~ labelOrQuoted(")))") ~ ")))").map(l => (l, NodeShape.DoubleCircle)) |
        ("((" ~ labelOrQuoted("))") ~ "))").map(l => (l, NodeShape.Circle)) |
        ("([" ~ labelOrQuoted("])") ~ "])").map(l => (l, NodeShape.Stadium)) |
        ("[/" ~ labelOrQuoted("\\]") ~ "\\]").map(l => (l, NodeShape.Trapezoid)) |
        ("[\\" ~ labelOrQuoted("/]") ~ "/]").map(l => (l, NodeShape.TrapezoidAlt)) |
        ("[/" ~ labelOrQuoted("/]") ~ "/]").map(l => (l, NodeShape.Parallelogram)) |
        ("[\\" ~ labelOrQuoted("\\]") ~ "\\]").map(l => (l, NodeShape.ParallelogramAlt)) |
        ("[[" ~ labelOrQuoted("]]") ~ "]]").map(l => (l, NodeShape.Subroutine)) |
        ("[(" ~ labelOrQuoted(")]") ~ ")]").map(l => (l, NodeShape.Cylinder)) |
        ("{{" ~ labelOrQuoted("}}") ~ "}}").map(l => (l, NodeShape.Hexagon)) |
        ("{" ~ labelOrQuoted("}") ~ "}").map(l => (l, NodeShape.Rhombus)) |
        ("(" ~ labelOrQuoted(")") ~ ")").map(l => (l, NodeShape.Round)) |
        ("[" ~ labelOrQuoted("]") ~ "]").map(l => (l, NodeShape.Rect))
    )

  // -- Node definition --------------------------------------------------------

  private[mermoid] def nodeDef(using P[Any]): P[NodeDef] =
    P(identifier ~ nodeShape.?).map { case (id, shapeOpt) =>
      shapeOpt match
        case Some((label, shape)) => NodeDef(id, Some(label), shape)
        case None                 => NodeDef(id, None, NodeShape.Rect)
    }

  // -- Edges ------------------------------------------------------------------

  private[mermoid] def edgeStyle(using P[Any]): P[(EdgeStyle, Option[String])] =
    P(
      ("-.->" ~ ws ~ ("|" ~ ws ~ CharsWhile(_ != '|', 1).! ~ ws ~ "|").?).map(lbl => (EdgeStyle.Dotted, lbl)) |
        ("-.-" ~ ws ~ ("|" ~ ws ~ CharsWhile(_ != '|', 1).! ~ ws ~ "|").?).map(lbl => (EdgeStyle.DottedOpen, lbl)) |
        ("==>" ~ ws ~ ("|" ~ ws ~ CharsWhile(_ != '|', 1).! ~ ws ~ "|").?).map(lbl => (EdgeStyle.Thick, lbl)) |
        ("-->" ~ ws ~ ("|" ~ ws ~ CharsWhile(_ != '|', 1).! ~ ws ~ "|").?).map(lbl => (EdgeStyle.Arrow, lbl)) |
        ("---" ~ ws ~ ("|" ~ ws ~ CharsWhile(_ != '|', 1).! ~ ws ~ "|").?).map(lbl => (EdgeStyle.Open, lbl)) |
        ("--" ~ ws ~ CharsWhile(c => c != '-' && c != '\n', 1).! ~ ws ~ "-->").map(lbl =>
          (EdgeStyle.Arrow, Some(lbl.trim))
        ) |
        ("--" ~ ws ~ CharsWhile(c => c != '-' && c != '\n', 1).! ~ ws ~ "---").map(lbl =>
          (EdgeStyle.Open, Some(lbl.trim))
        )
    )

  // -- Statements -------------------------------------------------------------

  private[mermoid] def asAlias(using P[Any]): P[String] =
    P(ws ~ "as" ~ ws ~ identifier)

  private[mermoid] def edgeSt(using P[Any]): P[FlowStatement.EdgeSt] =
    P(nodeDef ~ ws ~ edgeStyle ~ ws ~ nodeDef ~ asAlias.?).map { case (fromDef, (style, label), toDef, alias) =>
      FlowStatement.EdgeSt(Edge(fromDef.id, toDef.id, style, label, alias), fromDef, toDef)
    }

  /** `A --> B --> C` becomes one [[FlowStatement.EdgeSt]] per hop. Alias applies to the last hop. */
  private[mermoid] def edgeChain(using P[Any]): P[List[FlowStatement.EdgeSt]] =
    P(nodeDef ~ (ws ~ edgeStyle ~ ws ~ nodeDef).rep(1) ~ asAlias.?).map { case (first, hops, alias) =>
      val hopList = hops.toList
      hopList.zipWithIndex
        .foldLeft((Vector.empty[FlowStatement.EdgeSt], first)) { case ((acc, from), ((style, label, to), i)) =>
          val hopAlias                 = if i == hopList.length - 1 then alias else None
          val st: FlowStatement.EdgeSt =
            FlowStatement.EdgeSt(Edge(from.id, to.id, style, label, hopAlias), from, to)
          (acc :+ st, to)
        }
        ._1
        .toList
    }

  private[mermoid] def nodeSt(using P[Any]): P[FlowStatement.NodeSt] =
    P(nodeDef).map(FlowStatement.NodeSt(_))

  private[mermoid] def styleProperty(using P[Any]): P[(CssProperty, String)] =
    P(
      CharPred(c => c.isLetter || c == '-' || c == '_').rep(1).! ~ ws ~ ":" ~ ws ~
        CharsWhile(c => c != ',' && c != ';' && c != '\n', 1).!
    ).map { case (name, value) => CssProperty.parse(name) -> value.trim }

  private[mermoid] def styleProperties(using P[Any]): P[Map[CssProperty, String]] =
    P(styleProperty.rep(sep = ws ~ "," ~ ws)).map(_.toMap)

  private[mermoid] def styleSt(using P[Any]): P[FlowStatement.StyleSt] =
    P("style" ~ ws ~ identifier ~ ws ~ styleProperties).map { case (id, props) =>
      FlowStatement.StyleSt(id, props)
    }

  private[mermoid] def classDefSt(using P[Any]): P[FlowStatement.ClassDefSt] =
    P("classDef" ~ ws ~ identifier ~ ws ~ styleProperties).map { case (name, props) =>
      FlowStatement.ClassDefSt(name, props)
    }

  private[mermoid] def classSt(using P[Any]): P[FlowStatement.ClassSt] =
    P("class" ~ ws ~ identifier.rep(sep = ws ~ "," ~ ws, min = 1) ~ ws ~ identifier).map { case (ids, className) =>
      FlowStatement.ClassSt(ids.toList, className)
    }

  /** Mermaid `click` lines: callback and/or href, optional tooltip and link target. */
  private[mermoid] def clickSt(using P[Any]): P[FlowStatement.ClickSt] =
    P("click" ~ ws ~ identifier ~ ws ~ CharsWhile(c => c != '\n' && c != '\r', 1).!).map { case (id, rest) =>
      FlowStatement.ClickSt(parseClickRest(id, rest.trim))
    }

  /** Interpret the remainder of a `click` line after `click <id>`. */
  private[mermoid] def parseClickRest(nodeId: String, rest: String): ClickBinding =
    def unquote(s: String): String =
      if s.length >= 2 && s.head == '"' && s.last == '"' then s.substring(1, s.length - 1)
      else s

    val tokens = tokenizeClickRest(rest)
    tokens match
      case "href" :: url :: tip :: tgt :: Nil if isLinkTarget(tgt) =>
        ClickBinding(nodeId, Some(unquote(tip)), Some(unquote(url)), Some(tgt), None)
      case "href" :: url :: tgt :: Nil if isLinkTarget(tgt) =>
        ClickBinding(nodeId, None, Some(unquote(url)), Some(tgt), None)
      case "href" :: url :: tip :: Nil if tip.startsWith("\"") =>
        ClickBinding(nodeId, Some(unquote(tip)), Some(unquote(url)), None, None)
      case "href" :: url :: Nil =>
        ClickBinding(nodeId, None, Some(unquote(url)), None, None)
      case "call" :: cb :: tip :: Nil if cb.endsWith("()") && tip.startsWith("\"") =>
        ClickBinding(nodeId, Some(unquote(tip)), None, None, Some(cb.dropRight(2)))
      case "call" :: cb :: Nil if cb.endsWith("()") =>
        ClickBinding(nodeId, None, None, None, Some(cb.dropRight(2)))
      case "call" :: cb :: "()" :: tip :: Nil =>
        ClickBinding(nodeId, Some(unquote(tip)), None, None, Some(cb))
      case "call" :: cb :: "()" :: Nil =>
        ClickBinding(nodeId, None, None, None, Some(cb))
      case url :: tip :: tgt :: Nil if url.startsWith("\"") && isLinkTarget(tgt) =>
        ClickBinding(nodeId, Some(unquote(tip)), Some(unquote(url)), Some(tgt), None)
      case url :: tip :: Nil if url.startsWith("\"") =>
        ClickBinding(nodeId, Some(unquote(tip)), Some(unquote(url)), None, None)
      case url :: Nil if url.startsWith("\"") =>
        ClickBinding(nodeId, None, Some(unquote(url)), None, None)
      case cb :: tip :: Nil if tip.startsWith("\"") =>
        ClickBinding(nodeId, Some(unquote(tip)), None, None, Some(cb))
      case cb :: Nil =>
        ClickBinding(nodeId, None, None, None, Some(cb))
      case _ =>
        ClickBinding(nodeId, None, None, None, tokens.headOption)
    end match
  end parseClickRest

  private def isLinkTarget(s: String): Boolean =
    s == "_blank" || s == "_self" || s == "_parent" || s == "_top"

  /** Split click-rest into tokens; quoted strings stay as one token including quotes. */
  private def tokenizeClickRest(rest: String): List[String] =
    @annotation.tailrec
    def loop(i: Int, acc: List[String]): List[String] =
      if i >= rest.length then acc.reverse
      else if rest.charAt(i).isWhitespace then loop(i + 1, acc)
      else if rest.charAt(i) == '"' then
        val end = rest.indexOf('"', i + 1)
        if end < 0 then (rest.substring(i) :: acc).reverse
        else loop(end + 1, rest.substring(i, end + 1) :: acc)
      else if rest.startsWith("()", i) then loop(i + 2, "()" :: acc)
      else
        val end = rest.indexWhere(c => c.isWhitespace || c == '"', i)
        val j   = if end < 0 then rest.length else end
        loop(j, rest.substring(i, j) :: acc)
    loop(0, Nil)
  end tokenizeClickRest

  private[mermoid] def subgraphSt(using P[Any]): P[FlowStatement.SubgraphSt] =
    P(
      "subgraph" ~ ws ~ identifier ~ (ws ~ "[" ~ labelOrQuoted("]") ~ "]").? ~ nl ~
        ("direction" ~ ws ~ direction ~ nl).? ~
        flowStatements ~
        wsnl ~ endKeyword
    ).map { case (id, label, dir, stmts) =>
      FlowStatement.SubgraphSt(id, label, dir, stmts)
    }

  /** `end` closing a subgraph — a bare keyword, not the prefix of an id like `endpoint`. */
  private[mermoid] def endKeyword(using P[Any]): P[Unit] =
    P("end" ~ !CharPred(c => c.isLetterOrDigit || c == '_'))

  /** A statement, except the `end` that closes a subgraph.
    *
    * Without the guard `nodeSt` accepts `end` as a node id — `end` is a perfectly good identifier — so the statement
    * list swallows the closing keyword and the enclosing `subgraphSt` can never match it.
    */
  private[mermoid] def flowStatement(using P[Any]): P[List[FlowStatement]] =
    // `click` must precede `nodeSt`: otherwise `click A …` is swallowed as a bare node id.
    P(
      !endKeyword ~ (
        subgraphSt.map(s => List[FlowStatement](s)) |
          styleSt.map(s => List(s)) |
          classDefSt.map(s => List(s)) |
          classSt.map(s => List(s)) |
          clickSt.map(s => List(s)) |
          edgeChain |
          nodeSt.map(s => List(s))
      )
    )

  private[mermoid] def flowStatements(using P[Any]): P[List[FlowStatement]] =
    P(wsnl ~ flowStatement.rep(sep = sep) ~ wsnl).map(_.toList.flatten)

  // -- State Diagram -----------------------------------------------------------

  private[mermoid] def stateId(using P[Any]): P[String] =
    P("[*]".! | identifier)

  private[mermoid] def stateTransition(using P[Any]): P[StateStatement.TransitionSt] =
    P(stateId ~ ws ~ "-->" ~ ws ~ stateId ~ (":" ~ ws ~ CharsWhile(_ != '\n', 1).!).?).map { case (from, to, label) =>
      StateStatement.TransitionSt(StateTransition(from, to, label.map(_.trim)))
    }

  private[mermoid] def notePosition(using P[Any]): P[NotePosition] =
    P(
      "right of".!.map(_ => NotePosition.RightOf) |
        "left of".!.map(_ => NotePosition.LeftOf)
    )

  private[mermoid] def noteSt(using P[Any]): P[StateStatement.NoteSt] =
    P(
      "note" ~ ws ~ notePosition ~ ws ~ identifier ~ asAlias.? ~ nl ~
        (!("end note") ~ AnyChar).rep.! ~
        "end note"
    ).map { case (pos, id, alias, text) =>
      StateStatement.NoteSt(pos, id, text.linesIterator.map(_.trim).filter(_.nonEmpty).mkString("\n"), alias)
    }

  private[mermoid] def noteTextAlign(using P[Any]): P[NoteTextAlign] =
    P(
      "center".!.map(_ => NoteTextAlign.Center) |
        "right".!.map(_ => NoteTextAlign.Right) |
        "left".!.map(_ => NoteTextAlign.Left)
    )

  private[mermoid] def stateStyleSt(using P[Any]): P[StateStatement.StyleSt] =
    P("style" ~ ws ~ identifier ~ ws ~ "noteAlign" ~ ws ~ ":" ~ ws ~ noteTextAlign).map { case (id, align) =>
      StateStatement.StyleSt(id, StateStyle(noteAlign = Some(align)))
    }

  private[mermoid] def stateStatement(using P[Any]): P[StateStatement] =
    P(noteSt | stateStyleSt | stateTransition)

  private[mermoid] def stateStatements(using P[Any]): P[List[StateStatement]] =
    P(wsnl ~ stateStatement.rep(sep = sep) ~ wsnl).map(_.toList)

  private[mermoid] def stateDiagramHeader(using P[Any]): P[Unit] =
    P("stateDiagram-v2" ~ nl)

  private[mermoid] def stateDiagram(using P[Any]): P[Diagram.StateDiagram] =
    P(wsnl ~ stateDiagramHeader ~ stateStatements ~ wsnl ~ End).map { stmts =>
      Diagram.StateDiagram(stmts)
    }

  // -- Top-level --------------------------------------------------------------

  private[mermoid] def flowchartHeader(using P[Any]): P[Direction] =
    P(("flowchart" | "graph") ~ ws ~ direction.? ~ nl).map(_.getOrElse(Direction.TB))

  private[mermoid] def flowchart(using P[Any]): P[Diagram.Flowchart] =
    P(wsnl ~ flowchartHeader ~/ flowStatements ~ wsnl ~ End).map { case (dir, stmts) =>
      Diagram.Flowchart(dir, stmts)
    }

  def diagram(using P[Any]): P[Diagram] = P(stateDiagram | flowchart)

  // -- Public API -------------------------------------------------------------

  def parse(input: String): Either[String, Diagram] =
    fastparse.parse(input, diagram(using _)) match
      case Parsed.Success(value, _) => Right(value)
      case f: Parsed.Failure        => Left(f.msg)
end MermaidParser
