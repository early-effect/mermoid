package mermoid.docs

import mermoid.ascent.MermoidAscent
import _root_.mermoid.Mermaid
import specular.*
import specular.ziotest.DocSpecSuite

/** `erDiagram`: entities, keys, and crow's feet. */
object ErDiagrams extends DocSpecSuite:

  def doc = page("ER diagrams")(
    md"""
`erDiagram` draws entities and the cardinality between them. The mark is a crow's foot, not an arrow. `||` is exactly
one, `|o` is zero or one, `}|` is one or more, `}o` is zero or more. The right-hand mark is the mirror (`o|`, `|{`,
`o{`). `..` draws the same feet on a dashed line.
""",
    example {
      MermoidAscent.svgDiagram(Mermaid("""erDiagram
                            |    CUSTOMER ||--o{ ORDER : places
                            |    ORDER ||--|{ LINE-ITEM : contains
                            |""".stripMargin))
    },
    section("Attributes")(
      md"""
An entity block lists attributes as `type name`, with `PK` and `FK` when the attribute is a key. A quoted string after
the name is a comment and stays on the attribute. `classDef` and `style` paint the entity the same way they paint a
flowchart node.
""",
      example {
        MermoidAscent.svgDiagram(Mermaid("""erDiagram
                            |    CUSTOMER ||--o{ ORDER : places
                            |    CUSTOMER {
                            |        string name PK
                            |        string sector
                            |    }
                            |    ORDER {
                            |        int orderNumber PK
                            |        string deliveryAddress FK
                            |    }
                            |""".stripMargin))
      },
    ),
  )
end ErDiagrams
