package mermoid.css

import zio.test.*

object ThemeSpec extends ZIOSpecDefault:

  def spec = suite("Theme")(
    suite("colors")(
      test("each ThemeName produces a ThemeColors") {
        val names          = List(ThemeName.Default, ThemeName.Dark, ThemeName.Forest, ThemeName.Neutral)
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
      test("stylesheet has expected CSS variable names") {
        val ss = Theme.toStylesheet(ThemeName.Default)
        assertTrue(
          ss.variables.contains("--mermoid-primary"),
          ss.variables.contains("--mermoid-text"),
          ss.variables.contains("--mermoid-font-family"),
          ss.variables.contains("--mermoid-note-bg"),
        )
      },
      test("stylesheet has rules for node-shape, edge-line, note-rect") {
        val ss            = Theme.toStylesheet(ThemeName.Default)
        val selectorNames = ss.rules.map(_.selector).collect { case CssSelector.Class(name) =>
          name
        }
        assertTrue(
          selectorNames.contains("node-shape"),
          selectorNames.contains("edge-line"),
          selectorNames.contains("note-rect"),
          selectorNames.contains("node-label"),
          selectorNames.contains("edge-label"),
          selectorNames.contains("edge-label-bg"),
          selectorNames.contains("note-text"),
          selectorNames.contains("note-connector"),
        )
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
