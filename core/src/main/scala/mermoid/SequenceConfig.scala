package mermoid

/** Spacing for a sequence diagram. Flowchart knobs (`hSpacing`, barycenter sweeps) do not apply. */
case class SequenceConfig(
    actorMinWidth: Double = 96,
    actorPadH: Double = 16,
    actorHeight: Double = 44,
    columnGap: Double = 56,
    rowPitch: Double = 52,
    headerGap: Double = 20,
    footerGap: Double = 28,
    activationWidth: Double = 10,
    selfHookWidth: Double = 28,
    selfHookDrop: Double = 16,
)
