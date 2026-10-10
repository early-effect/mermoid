package mermoid

import mermoid.css.CssProperty

import scala.deriving.Mirror
import scala.quoted.*

/** Compile-time lifting of a parsed [[Diagram]] into the code that rebuilds it, for the [[Mermaid]] literal. */
private[mermoid] object MermaidLift:

  /** Fields lift through the tuple `ToExpr`, so a field added to a case class cannot be dropped here. */
  private def product[T <: Product: Type](x: T)(using
      m: Mirror.ProductOf[T],
      q: Quotes,
  )(using
      ToExpr[m.MirroredElemTypes],
      Type[m.MirroredElemTypes],
  ): Expr[T] =
    val fields = Expr(Tuple.fromProductTyped(x))
    '{ scala.compiletime.summonInline[Mirror.ProductOf[T]].fromProduct($fields) }

  given ToExpr[NodeId] with
    def apply(x: NodeId)(using Quotes): Expr[NodeId] = '{ NodeId.trusted(${ Expr(x.value) }) }

  given ToExpr[Direction] with
    def apply(x: Direction)(using Quotes): Expr[Direction] = x match
      case Direction.TB => '{ Direction.TB }
      case Direction.TD => '{ Direction.TD }
      case Direction.BT => '{ Direction.BT }
      case Direction.LR => '{ Direction.LR }
      case Direction.RL => '{ Direction.RL }

  given ToExpr[NodeShape] with
    def apply(x: NodeShape)(using Quotes): Expr[NodeShape] = x match
      case NodeShape.Rect             => '{ NodeShape.Rect }
      case NodeShape.Round            => '{ NodeShape.Round }
      case NodeShape.Stadium          => '{ NodeShape.Stadium }
      case NodeShape.Subroutine       => '{ NodeShape.Subroutine }
      case NodeShape.Cylinder         => '{ NodeShape.Cylinder }
      case NodeShape.Circle           => '{ NodeShape.Circle }
      case NodeShape.Rhombus          => '{ NodeShape.Rhombus }
      case NodeShape.Hexagon          => '{ NodeShape.Hexagon }
      case NodeShape.Parallelogram    => '{ NodeShape.Parallelogram }
      case NodeShape.ParallelogramAlt => '{ NodeShape.ParallelogramAlt }
      case NodeShape.Trapezoid        => '{ NodeShape.Trapezoid }
      case NodeShape.TrapezoidAlt     => '{ NodeShape.TrapezoidAlt }
      case NodeShape.DoubleCircle     => '{ NodeShape.DoubleCircle }
  end given

  given ToExpr[EdgeStyle] with
    def apply(x: EdgeStyle)(using Quotes): Expr[EdgeStyle] = x match
      case EdgeStyle.Arrow      => '{ EdgeStyle.Arrow }
      case EdgeStyle.Open       => '{ EdgeStyle.Open }
      case EdgeStyle.Dotted     => '{ EdgeStyle.Dotted }
      case EdgeStyle.Thick      => '{ EdgeStyle.Thick }
      case EdgeStyle.DottedOpen => '{ EdgeStyle.DottedOpen }

  given ToExpr[CssProperty] with
    def apply(x: CssProperty)(using Quotes): Expr[CssProperty] = x match
      case CssProperty.Fill            => '{ CssProperty.Fill }
      case CssProperty.Stroke          => '{ CssProperty.Stroke }
      case CssProperty.StrokeWidth     => '{ CssProperty.StrokeWidth }
      case CssProperty.StrokeDasharray => '{ CssProperty.StrokeDasharray }
      case CssProperty.FontFamily      => '{ CssProperty.FontFamily }
      case CssProperty.FontSize        => '{ CssProperty.FontSize }
      case CssProperty.Color           => '{ CssProperty.Color }
      case CssProperty.Background      => '{ CssProperty.Background }
      case CssProperty.BackgroundColor => '{ CssProperty.BackgroundColor }
      case CssProperty.Border          => '{ CssProperty.Border }
      case CssProperty.BorderColor     => '{ CssProperty.BorderColor }
      case CssProperty.BorderWidth     => '{ CssProperty.BorderWidth }
      case v: CssProperty.Custom       => product(v)
  end given

  given ToExpr[NodeDef] with
    def apply(x: NodeDef)(using Quotes): Expr[NodeDef] = product(x)

  given ToExpr[Edge] with
    def apply(x: Edge)(using Quotes): Expr[Edge] = product(x)

  given ToExpr[ClickBinding] with
    def apply(x: ClickBinding)(using Quotes): Expr[ClickBinding] = product(x)

  given ToExpr[FlowStatement] with
    def apply(x: FlowStatement)(using Quotes): Expr[FlowStatement] = x match
      case v: FlowStatement.NodeSt     => product(v)
      case v: FlowStatement.EdgeSt     => product(v)
      case v: FlowStatement.SubgraphSt => product(v)
      case v: FlowStatement.StyleSt    => product(v)
      case v: FlowStatement.ClassDefSt => product(v)
      case v: FlowStatement.ClassSt    => product(v)
      case v: FlowStatement.ClickSt    => product(v)

  given ToExpr[NotePosition] with
    def apply(x: NotePosition)(using Quotes): Expr[NotePosition] = x match
      case NotePosition.RightOf => '{ NotePosition.RightOf }
      case NotePosition.LeftOf  => '{ NotePosition.LeftOf }

  given ToExpr[NoteTextAlign] with
    def apply(x: NoteTextAlign)(using Quotes): Expr[NoteTextAlign] = x match
      case NoteTextAlign.Left   => '{ NoteTextAlign.Left }
      case NoteTextAlign.Center => '{ NoteTextAlign.Center }
      case NoteTextAlign.Right  => '{ NoteTextAlign.Right }

  given ToExpr[StateTransition] with
    def apply(x: StateTransition)(using Quotes): Expr[StateTransition] = product(x)

  given ToExpr[StateStyle] with
    def apply(x: StateStyle)(using Quotes): Expr[StateStyle] = product(x)

  given ToExpr[StateForm] with
    def apply(x: StateForm)(using Quotes): Expr[StateForm] = x match
      case StateForm.Simple         => '{ StateForm.Simple }
      case StateForm.Choice         => '{ StateForm.Choice }
      case StateForm.Fork           => '{ StateForm.Fork }
      case StateForm.Join           => '{ StateForm.Join }
      case StateForm.ShallowHistory => '{ StateForm.ShallowHistory }
      case StateForm.DeepHistory    => '{ StateForm.DeepHistory }

  given ToExpr[StateStatement] with
    def apply(x: StateStatement)(using Quotes): Expr[StateStatement] = x match
      case v: StateStatement.TransitionSt      => product(v)
      case v: StateStatement.Description       => product(v)
      case v: StateStatement.Form              => product(v)
      case v: StateStatement.Composite         => product(v)
      case v: StateStatement.NoteSt            => product(v)
      case v: StateStatement.FloatingNote      => product(v)
      case v: StateStatement.StyleSt           => product(v)
      case v: StateStatement.ClassDefSt        => product(v)
      case v: StateStatement.ClassSt           => product(v)
      case v: StateStatement.ClickSt           => product(v)
      case StateStatement.HideEmptyDescription => '{ StateStatement.HideEmptyDescription }
      case v: StateStatement.Scale             => product(v)
      case v: StateStatement.AccTitle          => product(v)
      case v: StateStatement.AccDescr          => product(v)
      case StateStatement.Divider              => '{ StateStatement.Divider }
      case v: StateStatement.UnknownStereo     => product(v)
  end given

  given ToExpr[ParticipantKind] with
    def apply(x: ParticipantKind)(using Quotes): Expr[ParticipantKind] = x match
      case ParticipantKind.Participant => '{ ParticipantKind.Participant }
      case ParticipantKind.Actor       => '{ ParticipantKind.Actor }

  given ToExpr[SequenceArrow] with
    def apply(x: SequenceArrow)(using Quotes): Expr[SequenceArrow] = x match
      case SequenceArrow.Solid       => '{ SequenceArrow.Solid }
      case SequenceArrow.Dashed      => '{ SequenceArrow.Dashed }
      case SequenceArrow.SolidHead   => '{ SequenceArrow.SolidHead }
      case SequenceArrow.DashedHead  => '{ SequenceArrow.DashedHead }
      case SequenceArrow.SolidBoth   => '{ SequenceArrow.SolidBoth }
      case SequenceArrow.DashedBoth  => '{ SequenceArrow.DashedBoth }
      case SequenceArrow.SolidCross  => '{ SequenceArrow.SolidCross }
      case SequenceArrow.DashedCross => '{ SequenceArrow.DashedCross }
      case SequenceArrow.SolidOpen   => '{ SequenceArrow.SolidOpen }
      case SequenceArrow.DashedOpen  => '{ SequenceArrow.DashedOpen }
  end given

  given ToExpr[MessageControl] with
    def apply(x: MessageControl)(using Quotes): Expr[MessageControl] = x match
      case MessageControl.None       => '{ MessageControl.None }
      case MessageControl.Activate   => '{ MessageControl.Activate }
      case MessageControl.Deactivate => '{ MessageControl.Deactivate }

  given ToExpr[NotePlace] with
    def apply(x: NotePlace)(using Quotes): Expr[NotePlace] = x match
      case v: NotePlace.LeftOf  => product(v)
      case v: NotePlace.RightOf => product(v)
      case v: NotePlace.Over    => product(v)

  given ToExpr[Numbering] with
    def apply(x: Numbering)(using Quotes): Expr[Numbering] = x match
      case v: Numbering.On => product(v)
      case Numbering.Off   => '{ Numbering.Off }

  given ToExpr[Rgb] with
    def apply(x: Rgb)(using Quotes): Expr[Rgb] = product(x)

  given ToExpr[GroupKind] with
    def apply(x: GroupKind)(using Quotes): Expr[GroupKind] = x match
      case v: GroupKind.Loop      => product(v)
      case v: GroupKind.Opt       => product(v)
      case v: GroupKind.Critical  => product(v)
      case v: GroupKind.Break     => product(v)
      case GroupKind.Alt          => '{ GroupKind.Alt }
      case GroupKind.Par          => '{ GroupKind.Par }
      case v: GroupKind.Highlight => product(v)

  given ToExpr[GroupSection] with
    def apply(x: GroupSection)(using Quotes): Expr[GroupSection] = product(x)

  given ToExpr[SequenceStatement] with
    def apply(x: SequenceStatement)(using Quotes): Expr[SequenceStatement] = x match
      case v: SequenceStatement.Declare    => product(v)
      case v: SequenceStatement.Message    => product(v)
      case v: SequenceStatement.Activate   => product(v)
      case v: SequenceStatement.Deactivate => product(v)
      case v: SequenceStatement.Note       => product(v)
      case v: SequenceStatement.Autonumber => product(v)
      case v: SequenceStatement.Group      => product(v)

  given ToExpr[Diagram] with
    def apply(x: Diagram)(using Quotes): Expr[Diagram] = x match
      case v: Diagram.Flowchart    => product(v)
      case v: Diagram.StateDiagram => product(v)
      case v: Diagram.Sequence     => product(v)
end MermaidLift
