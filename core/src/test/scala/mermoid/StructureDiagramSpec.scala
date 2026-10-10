package mermoid

import zio.*
import zio.test.*

object StructureDiagramSpec extends ZIOSpecDefault:

  private def ranked(src: String): IO[String, DiagramScene] =
    ZIO.fromEither(MermaidParser.parse(src)).mapError(_.message).flatMap { diagram =>
      DiagramLayout.scene(diagram) match
        case Scene.Ranked(scene) => ZIO.succeed(scene)
        case other               => ZIO.fail(other.toString)
    }

  private def inside(node: LayoutNode, frame: PlacedFrame): Boolean =
    val left   = node.center.x - node.width / 2
    val right  = node.center.x + node.width / 2
    val top    = node.center.y - node.height / 2
    val bottom = node.center.y + node.height / 2
    left >= frame.rect.x && right <= frame.rect.x + frame.rect.w &&
    top >= frame.rect.y && bottom <= frame.rect.y + frame.rect.h

  def spec = suite("class and ER diagrams")(
    test("a class body splits attributes from operations") {
      val src =
        """classDiagram
          |  class BankAccount {
          |    +String owner
          |    +List~Int~ items
          |    +deposit(amount) bool
          |  }
          |""".stripMargin
      val resolved = MermaidParser.parse(src).flatMap {
        case diagram: Diagram.ClassDiagram => ClassModel.resolve(diagram)
        case _                             => Left(ParseError.Failed(0, Nil))
      }
      val node    = resolved.toOption.flatMap(_.nodes.find(_.id.value == "BankAccount"))
      val owner   = node.flatMap(_.attributes.find(_.name == "owner"))
      val items   = node.flatMap(_.attributes.find(_.name == "items"))
      val deposit = node.flatMap(_.operations.find(_.name == "deposit"))
      assertTrue(
        owner.exists(_.visibility.contains(ClassVisibility.Public)),
        owner.exists(_.typeName.contains("String")),
        items.exists(_.typeName.contains("List~Int~")),
        deposit.exists(_.operation),
        deposit.exists(_.returnType.contains("bool")),
        deposit.exists(_.parameters.contains("amount")),
      )
    },
    test("a namespace frame contains its class and a cross edge stays inside both frames") {
      val src =
        """classDiagram
          |  namespace Left {
          |    class A
          |  }
          |  namespace Right {
          |    class B
          |  }
          |  A --> B
          |""".stripMargin
      for scene <- ranked(src)
      yield
        val a     = scene.visibleNodes.find(_.id.value == "A")
        val b     = scene.visibleNodes.find(_.id.value == "B")
        val left  = scene.subgraphs.find(_.id == "Left")
        val right = scene.subgraphs.find(_.id == "Right")
        val edge  = scene.edges.exists(e => e.from.value == "A" && e.to.value == "B")
        val held  = a.zip(left).zip(b).zip(right).exists { case (((nodeA, frameA), nodeB), frameB) =>
          inside(nodeA, frameA) && inside(nodeB, frameB)
        }
        assertTrue(edge, held)
      end for
    },
    test("an ER crow's foot is one-and-only to zero-or-more, and the key is marked") {
      val src =
        """erDiagram
          |  CUSTOMER ||--o{ ORDER : places
          |  CUSTOMER {
          |    string name PK
          |    string sector
          |  }
          |""".stripMargin
      val resolved = MermaidParser.parse(src).flatMap {
        case diagram: Diagram.ErDiagram => ErModel.resolve(diagram)
        case _                          => Left(ParseError.Failed(0, Nil))
      }
      val edge = resolved.toOption.flatMap(_.edges.headOption)
      val name = resolved.toOption
        .flatMap(_.entities.find(_.id.value == "CUSTOMER"))
        .flatMap(_.attributes.find(_.name == "name"))
      for scene <- ranked(src)
      yield
        val svg = scene.edges.flatMap(_.mark).flatMap(_.kinds).map(_.markerId).toSet
        assertTrue(
          edge.exists(_.mark.exists(m => m.from.contains(MarkKind.ExactlyOne) && m.to.contains(MarkKind.ZeroOrMore))),
          name.exists(_.primary),
          svg.contains(MarkKind.ExactlyOne.markerId),
          svg.contains(MarkKind.ZeroOrMore.markerId),
          !svg.contains("arrowhead"),
        )
      end for
    },
  )
end StructureDiagramSpec
