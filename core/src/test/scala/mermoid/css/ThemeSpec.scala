package mermoid.css

import zio.test.*

object ThemeSpec extends ZIOSpecDefault:

  def spec = suite("Theme")(
    suite("colors")(
      test("each ThemeName produces a ThemeColors") {
        val names          = ThemeName.values.toList
        val allHaveContent = names.forall { name =>
          val tc = Theme.colors(name)
          tc.primaryColor.nonEmpty && tc.fontFamily.nonEmpty
        }
        assertTrue(allHaveContent)
      }
    ),
    suite("toStylesheet")(
      test("Default theme produces non-empty stylesheet") {
        val ss = Theme.toStylesheet(ThemeName.Default)
        assertTrue(ss.variables.nonEmpty, ss.rules.nonEmpty)
      },
      test("Dark theme produces non-empty stylesheet") {
        val ss = Theme.toStylesheet(ThemeName.Dark)
        assertTrue(ss.variables.nonEmpty, ss.rules.nonEmpty)
      },
      test("Forest theme produces non-empty stylesheet") {
        val ss = Theme.toStylesheet(ThemeName.Forest)
        assertTrue(ss.variables.nonEmpty, ss.rules.nonEmpty)
      },
      test("Neutral theme produces non-empty stylesheet") {
        val ss = Theme.toStylesheet(ThemeName.Neutral)
        assertTrue(ss.variables.nonEmpty, ss.rules.nonEmpty)
      },
      test("stylesheet variables are exactly ThemeVar") {
        val ss = Theme.toStylesheet(ThemeName.Default)
        assertTrue(
          ss.variables.keySet == ThemeVar.values.map(_.cssName).toSet,
          ss.get(ThemeVar.NoteBg).contains(CssValue.Color("#ffffcc")),
          ThemeVar.MainBkg.description == "node fill",
          ThemeVar.Selection.cssName == "--mermoid-selection",
          ThemeVar.Line.cssVar == "var(--mermoid-line)",
          ThemeVar.Selection.cssVar(ThemeVar.Line.cssVar("#333")) ==
            "var(--mermoid-selection, var(--mermoid-line, #333))",
        )
      },
      test("stylesheet has a rule per inner PaintClass") {
        val ss            = Theme.toStylesheet(ThemeName.Default)
        val selectorNames = ss.rules.map(_.selector).collect { case CssSelector.Class(name) =>
          name
        }
        val expected = List(
          PaintClass.DiagramBg,
          PaintClass.NodeShape,
          PaintClass.EdgeLine,
          PaintClass.NoteRect,
          PaintClass.NodeLabel,
          PaintClass.EdgeLabel,
          PaintClass.EdgeLabelBg,
          PaintClass.NoteText,
          PaintClass.NoteConnector,
        ).map(_.cssName)
        assertTrue(expected.forall(selectorNames.contains))
      },
    ),
    suite("render round-trip")(
      test("Default theme renders to valid CSS") {
        val css = CssRenderer.render(Theme.toStylesheet(ThemeName.Default))
        assertTrue(
          css.contains(":root {"),
          css.contains("--mermoid-primary:"),
          css.contains(".node-shape {"),
          css.contains(".edge-line {"),
        )
      },
      test("variable resolution produces concrete values") {
        val css = CssRenderer.render(Theme.toStylesheet(ThemeName.Default), resolveVariables = true)
        assertTrue(
          !css.contains("var(--mermoid"),
          css.contains(".node-shape {"),
          css.contains("fill: #ECECFF;"),
        )
      },
      test("Dark theme renders with dark colors") {
        val css = CssRenderer.render(Theme.toStylesheet(ThemeName.Dark), resolveVariables = true)
        assertTrue(
          css.contains("fill: #1f2020;"),
          css.contains("stroke: #81B1DB;"),
        )
      },
    ),
  )
end ThemeSpec
