package mermoid

import fastparse.*
import fastparse.NoWhitespace.*

object MermaidParser:

  // -- Helpers ----------------------------------------------------------------

  private[mermoid] def ws(using P[Any]): P[Unit]   = P(CharsWhileIn(" \t", 0))
  private[mermoid] def nl(using P[Any]): P[Unit]   = P(ws ~ ("\r\n" | "\n") ~ ws)
  private[mermoid] def wsnl(using P[Any]): P[Unit] = P(CharsWhileIn(" \t\r\n", 0))
  private[mermoid] def sep(using P[Any]): P[Unit]  = P((ws ~ (";" | nl) ~ wsnl).rep(1))

  private[mermoid] def identifier(using P[Any]): P[String] =
    P(CharPred(c => c.isLetterOrDigit || c == '_').rep(1).!)

  private[mermoid] def quotedString(using P[Any]): P[String] =
    P("\"" ~/ CharsWhile(_ != '"', 0).! ~ "\"")

  private[mermoid] def labelText(using P[Any]): P[String] =
    P(CharsWhile(c => c != ']' && c != ')' && c != '}' && c != '/' && c != '\\' && c != '|' && c != '\n', 1).!)

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
      ("(((" ~ labelText ~ ")))").map(l => (l, NodeShape.DoubleCircle)) |
        ("((" ~ labelText ~ "))").map(l => (l, NodeShape.Circle)) |
        ("([" ~ labelText ~ "])").map(l => (l, NodeShape.Stadium)) |
        ("[/" ~ labelText ~ "\\]").map(l => (l, NodeShape.Trapezoid)) |
        ("[\\" ~ labelText ~ "/]").map(l => (l, NodeShape.TrapezoidAlt)) |
        ("[/" ~ labelText ~ "/]").map(l => (l, NodeShape.Parallelogram)) |
        ("[\\" ~ labelText ~ "\\]").map(l => (l, NodeShape.ParallelogramAlt)) |
        ("[[" ~ labelText ~ "]]").map(l => (l, NodeShape.Subroutine)) |
        ("[(" ~ labelText ~ ")]").map(l => (l, NodeShape.Cylinder)) |
        ("{{" ~ labelText ~ "}}").map(l => (l, NodeShape.Hexagon)) |
        ("{" ~ labelText ~ "}").map(l => (l, NodeShape.Rhombus)) |
        ("(" ~ labelText ~ ")").map(l => (l, NodeShape.Round)) |
        ("[" ~ labelText ~ "]").map(l => (l, NodeShape.Rect))
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

  private[mermoid] def nodeSt(using P[Any]): P[FlowStatement.NodeSt] =
    P(nodeDef).map(FlowStatement.NodeSt(_))

  private[mermoid] def styleProperty(using P[Any]): P[(String, String)] =
    P(
      CharPred(c => c.isLetter || c == '-' || c == '_').rep(1).! ~ ws ~ ":" ~ ws ~
        CharsWhile(c => c != ',' && c != ';' && c != '\n', 1).!
    )

  private[mermoid] def styleProperties(using P[Any]): P[Map[String, String]] =
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

  private[mermoid] def subgraphSt(using P[Any]): P[FlowStatement.SubgraphSt] =
    P(
      "subgraph" ~ ws ~ identifier ~ (ws ~ "[" ~ labelText ~ "]").? ~ nl ~
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
  private[mermoid] def flowStatement(using P[Any]): P[FlowStatement] =
    P(!endKeyword ~ (subgraphSt | styleSt | classDefSt | classSt | edgeSt | nodeSt))

  private[mermoid] def flowStatements(using P[Any]): P[List[FlowStatement]] =
    P(wsnl ~ flowStatement.rep(sep = sep) ~ wsnl).map(_.toList)

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
    P(wsnl ~ flowchartHeader ~ flowStatements ~ wsnl ~ End).map { case (dir, stmts) =>
      Diagram.Flowchart(dir, stmts)
    }

  def diagram(using P[Any]): P[Diagram] = P(stateDiagram | flowchart)

  // -- Public API -------------------------------------------------------------

  def parse(input: String): Either[String, Diagram] =
    fastparse.parse(input, diagram(using _)) match
      case Parsed.Success(value, _) => Right(value)
      case f: Parsed.Failure        => Left(f.msg)
end MermaidParser
