package mermoid

case class LayoutConfig(
    minNodeWidth: Double = 80.0,
    nodeHeight: Double = 50.0,
    nodePaddingH: Double = 24.0,
    charWidthEstimate: Double = 8.5,
    hSpacing: Double = 80.0,
    vSpacing: Double = 80.0,
    padding: Double = 40.0,
    arrowSize: Double = 8.0,
    cornerRadius: Double = 15.0,
    subroutineInset: Double = 8.0,
    cylinderRy: Double = 8.0,
    hexagonIndent: Double = 15.0,
    parallelogramSkew: Double = 15.0,
    trapezoidIndent: Double = 15.0,
    doubleCircleGap: Double = 5.0,
    selfLoopSize: Double = 30.0,
    selfLoopLabelPadding: Double = 10.0,
    fontSize: Int = 14,
    edgeLabelFontSize: Int = 12,
    fontFamily: String = "sans-serif",
)
