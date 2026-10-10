package mermoid

import mermoid.css.CssProperty
import mermoid.css.PaintClass

/** A region laid out on its own, then stacked. A group is a node in its parent whose size is the diagram inside it. */
private[mermoid] object CompoundLayout:

  enum Item:
    case Leaf(
        defn: NodeDef,
        classes: List[String],
        styles: Map[CssProperty, String],
        /** When set, the ranker uses this box instead of measuring the label. */
        fixed: Option[(Double, Double)],
    )
    case Group(
        id: NodeId,
        title: Option[String],
        direction: Direction,
        regions: List[Region],
        classes: List[String],
        styles: Map[CssProperty, String],
    )
  end Item

  case class Region(items: List[Item], edges: List[Edge])

  case class Placed(
      nodes: List[LayoutNode],
      routes: Map[(NodeId, NodeId), List[Point]],
      frames: List[PlacedFrame],
      edges: List[Edge],
      /** Y of a concurrency divider, in the same coordinates as `nodes`. */
      dividers: List[Double],
  )

  private val framePad  = 14.0
  private val titleBand = 22.0
  private val regionGap = 18.0
  private val minFrameW = 56.0
  private val minFrameH = 36.0
  private val barThick  = 8.0
  private val nestedPad = 16.0

  def place(
      direction: Direction,
      regions: List[Region],
      config: LayoutConfig,
      measure: TextMeasure,
  ): Placed =
    stack(regions.map(region => placeRegion(region, direction, config, measure)))

  private def placeRegion(
      region: Region,
      direction: Direction,
      config: LayoutConfig,
      measure: TextMeasure,
  ): Placed =
    val nested = region.items.collect { case group: Item.Group =>
      val inner = place(group.direction, group.regions, config.copy(padding = nestedPad), measure)
      group.id -> (group, inner, outerSize(inner, group.title))
    }.toMap

    val directIds                           = region.items.collect { case Item.Leaf(defn, _, _, _) => defn.id }.toSet
    def groupOf(id: NodeId): Option[NodeId] =
      if directIds.contains(id) then None
      else nested.collectFirst { case (gid, (_, inner, _)) if containsId(inner, id) => gid }

    val defs = region.items.map {
      case Item.Leaf(defn, _, _, _)          => defn.id -> defn
      case Item.Group(id, title, _, _, _, _) =>
        id -> NodeDef(id, title.orElse(Some(id.value)), NodeShape.Round)
    }.toMap

    val sizes = region.items.flatMap {
      case Item.Leaf(defn, _, _, Some(fixed)) => List(defn.id -> fixed)
      case Item.Leaf(_, _, _, None)           => Nil
      case Item.Group(id, _, _, _, _, _)      => nested.get(id).map { case (_, _, wh) => id -> wh }.toList
    }.toMap

    val rankEdges = region.edges.flatMap { edge =>
      val from     = groupOf(edge.from).getOrElse(edge.from)
      val to       = groupOf(edge.to).getOrElse(edge.to)
      val internal = groupOf(edge.from).isDefined && groupOf(edge.from) == groupOf(edge.to)
      if internal then Nil
      else if defs.contains(from) && defs.contains(to) then List(edge.copy(from = from, to = to))
      else Nil
    }

    val laid = Layout.layout(
      config,
      direction,
      defs,
      rankEdges,
      sizes,
      Some(measure),
    )
    val bars      = barIds(region)
    val stretched = stretchBars(laid.nodes, rankEdges, bars, isVertical(direction))
    val decorated = stretched.map { node =>
      region.items
        .collectFirst {
          case Item.Leaf(defn, classes, styles, _) if defn.id == node.id =>
            node.copy(cssClasses = classes, styles = styles, label = defn.label.getOrElse(node.label))
        }
        .getOrElse(node)
    }

    val inserted = nested.foldLeft(Inserted(decorated, Map.empty, Nil)) { case (acc, (gid, (group, inner, _))) =>
      acc.nodes.find(_.id == gid) match
        case None          => acc
        case Some(standIn) =>
          val left    = standIn.center.x - standIn.width / 2
          val top     = standIn.center.y - standIn.height / 2
          val title   = if group.title.isDefined then titleBand else 0.0
          val box     = bounds(inner)
          val dx      = left + framePad - box.minX
          val dy      = top + framePad + title - box.minY
          val shifted = shift(inner, dx, dy)
          val frame   = PlacedFrame(
            gid.value,
            group.title,
            Rect(left, top, standIn.width, standIn.height),
            shifted.dividers,
          )
          val hidden = acc.nodes.map(n => if n.id == gid then n.copy(dummy = true) else n)
          Inserted(
            hidden ++ shifted.nodes,
            acc.routes ++ shifted.routes,
            acc.frames ++ shifted.frames :+ frame,
          )
    }

    val visible    = inserted.nodes.filter(!_.dummy).map(_.id).toSet
    val ownRoutes  = laid.routes.filter { case ((from, to), _) => visible.contains(from) && visible.contains(to) }
    val drawn      = region.edges.filter(e => visible.contains(e.from) && visible.contains(e.to))
    val childEdges = nested.values.flatMap(_._2.edges).filter(e => visible.contains(e.from) && visible.contains(e.to))
    Placed(inserted.nodes, ownRoutes ++ inserted.routes, inserted.frames, drawn ++ childEdges.toList, Nil)
  end placeRegion

  private case class Inserted(
      nodes: List[LayoutNode],
      routes: Map[(NodeId, NodeId), List[Point]],
      frames: List[PlacedFrame],
  )

  private def containsId(placed: Placed, id: NodeId): Boolean =
    placed.nodes.exists(_.id == id) || placed.frames.exists(_.id == id.value)

  private def barIds(region: Region): Set[NodeId] =
    region.items.collect {
      case Item.Leaf(defn, classes, _, _) if classes.contains(PaintClass.StateBar.cssName) => defn.id
    }.toSet

  private def isVertical(direction: Direction): Boolean = direction match
    case Direction.TB | Direction.TD | Direction.BT => true
    case Direction.LR | Direction.RL                => false

  private def stretchBars(
      nodes: List[LayoutNode],
      edges: List[Edge],
      bars: Set[NodeId],
      vertical: Boolean,
  ): List[LayoutNode] =
    nodes.map { node =>
      if !bars.contains(node.id) then node
      else
        val linked = edges.flatMap { edge =>
          if edge.from == node.id then nodes.find(_.id == edge.to)
          else if edge.to == node.id then nodes.find(_.id == edge.from)
          else None
        }
        val across =
          if vertical then linked.map(_.center.x)
          else linked.map(_.center.y)
        val min  = across.minOption.getOrElse(if vertical then node.center.x else node.center.y)
        val max  = across.maxOption.getOrElse(min)
        val span = Math.max(barThick, max - min + framePad)
        if vertical then node.copy(width = span, height = barThick, center = Point((min + max) / 2, node.center.y))
        else node.copy(width = barThick, height = span, center = Point(node.center.x, (min + max) / 2))
    }

  private def outerSize(inner: Placed, title: Option[String]): (Double, Double) =
    val box    = bounds(inner)
    val titleH = if title.isDefined then titleBand else 0.0
    (Math.max(minFrameW, box.width + framePad * 2), Math.max(minFrameH, box.height + framePad * 2 + titleH))

  private case class Box(minX: Double, minY: Double, maxX: Double, maxY: Double):
    def width: Double  = Math.max(0.0, maxX - minX)
    def height: Double = Math.max(0.0, maxY - minY)

  private def bounds(placed: Placed): Box =
    val nodes = placed.nodes.filter(!_.dummy)
    val xs    =
      nodes.flatMap(n => List(n.center.x - n.width / 2, n.center.x + n.width / 2)) ++
        placed.frames.flatMap(f => List(f.rect.x, f.rect.x + f.rect.w))
    val ys =
      nodes.flatMap(n => List(n.center.y - n.height / 2, n.center.y + n.height / 2)) ++
        placed.frames.flatMap(f => List(f.rect.y, f.rect.y + f.rect.h))
    Box(
      xs.minOption.getOrElse(0.0),
      ys.minOption.getOrElse(0.0),
      xs.maxOption.getOrElse(0.0),
      ys.maxOption.getOrElse(0.0),
    )
  end bounds

  private def stack(parts: List[Placed]): Placed = parts match
    case Nil        => Placed(Nil, Map.empty, Nil, Nil, Nil)
    case one :: Nil => one
    case many       =>
      val (placed, _, dividers) = many.foldLeft((List.empty[Placed], 0.0, List.empty[Double])) {
        case ((acc, y, dividers), part) =>
          val box     = bounds(part)
          val line    = if acc.isEmpty then dividers else (y - regionGap / 2) :: dividers
          val shifted = shift(part, -box.minX, y - box.minY)
          val nextY   = y + box.height + regionGap
          (shifted :: acc, nextY, line)
      }
      val all = placed.reverse
      Placed(
        all.flatMap(_.nodes),
        all.flatMap(_.routes).toMap,
        all.flatMap(_.frames),
        all.flatMap(_.edges),
        dividers.reverse ++ all.flatMap(_.dividers),
      )

  private def shift(placed: Placed, dx: Double, dy: Double): Placed =
    placed.copy(
      nodes = placed.nodes.map(n => n.copy(center = Point(n.center.x + dx, n.center.y + dy))),
      routes = shiftRoutes(placed.routes, dx, dy),
      frames = placed.frames.map { frame =>
        frame.copy(
          rect = frame.rect.copy(x = frame.rect.x + dx, y = frame.rect.y + dy),
          dividers = frame.dividers.map(_ + dy),
        )
      },
      dividers = placed.dividers.map(_ + dy),
    )

  private def shiftRoutes(
      routes: Map[(NodeId, NodeId), List[Point]],
      dx: Double,
      dy: Double,
  ): Map[(NodeId, NodeId), List[Point]] =
    routes.map((k, pts) => k -> pts.map(p => Point(p.x + dx, p.y + dy)))
end CompoundLayout
