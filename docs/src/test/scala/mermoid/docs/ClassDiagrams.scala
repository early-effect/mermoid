package mermoid.docs

import mermoid.ascent.MermoidAscent
import _root_.mermoid.Mermaid
import specular.*
import specular.ziotest.DocSpecSuite

/** `classDiagram`: classes, members, relations, namespaces. */
object ClassDiagrams extends DocSpecSuite:

  def doc = page("Class diagrams")(
    md"""
`classDiagram` draws types and the relations between them. A class is a stack of bands: the name, the attributes, and
the operations. An empty band stays visible unless the diagram says `hideEmptyMembersBox`.
""",
    example {
      MermoidAscent.svgDiagram(Mermaid("""classDiagram
                            |    class BankAccount {
                            |        +String owner
                            |        +BigDecimal balance
                            |        +deposit(amount) bool
                            |        +withdrawal(amount) int
                            |    }
                            |""".stripMargin))
    },
    section("Members")(
      md"""
`()` is what makes a member an operation. Anything else is an attribute. `+` `-` `#` `~` are public, private, protected,
and package. A trailing `*` is abstract and a trailing `$$` is static. `List~Int~` is a generic, on the class or on a
member. `Name : member` adds one member to a class that already exists.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""classDiagram
                            |    class Square~Shape~ {
                            |        +int id
                            |        +List~Int~ points
                            |        +getPoints() List~Int~
                            |    }
                            |""".stripMargin))
      },
    ),
    section("Relations")(
      md"""
The head is the UML glyph, not a flowchart arrow. `<|--` is inheritance, `*--` composition, `o--` aggregation, `-->`
association, `..>` dependency, `..|>` realization, `--` a solid link, `..` a dashed link. A quoted cardinality sits
beside the end it describes. `()--` is a lollipop interface.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""classDiagram
                            |    class Customer
                            |    class Order
                            |    class LineItem
                            |    Customer "1" --> "*" Order : places
                            |    Order "1" *-- "*" LineItem : contains
                            |""".stripMargin))
      },
    ),
    section("Stereotypes and namespaces")(
      md"""
`<<interface>>`, `<<abstract>>`, `<<enumeration>>`, and `<<service>>` are the usual annotations. Any other `<<name>>`
is kept as written. Two different stereotypes on one class is a parse error that names both.

`namespace Id { ... }` is a frame. A `direction` inside the diagram applies to every namespace as well as the page.
A class named in two namespaces is a parse error that names both.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""classDiagram
                            |    direction LR
                            |    namespace Billing {
                            |        class Invoice
                            |        class Payment
                            |        Invoice --> Payment : settled by
                            |    }
                            |    class Customer
                            |    Customer --> Invoice : receives
                            |""".stripMargin))
      },
    ),
  )
end ClassDiagrams
