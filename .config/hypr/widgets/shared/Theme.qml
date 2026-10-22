pragma Singleton
import Quickshell
import QtQuick

// Shared theme for all Hyprland widgets (workspaces / corner / window / smart /
// volume / notifications / recorder / whichkey …). Black surfaces, grey borders,
// the accents of Hyprland's active window border (appearance.lua): cyan → green.
// A cyan frame means "this has the keyboard", window or widget alike.
// Square corners and 2px borders like Hyprland (rounding = 0, border_size = 2).
QtObject {
    // ── Type ────────────────────────────────────────────────────────────────
    readonly property string fontFamily: "GeistMono Nerd Font"   // one face everywhere, like the terminal
    // Every text size is multiplied by this. Larger on the G9 (desktop, or a docked laptop),
    // where the widgets show whenever it is connected; 1 on the laptop panel.
    readonly property real textScale: Quickshell.screens.some(function(s) { return s.model === "Odyssey G91F" }) ? 1.3 : 1.0

    // ── Accent ──────────────────────────────────────────────────────────────
    readonly property color accent:  "#33ccff"   // = Hyprland active_border: focus, keys, selection
    readonly property color accent2: "#00ff99"   // = gradient end of Hyprland active_border
    readonly property color warning: "#e6b071"   // muted amber: critical, muted volume, DND count
    readonly property color danger:  "#e5675e"   // muted red: recording

    // Global corner rounding — 0 = sharp corners, like the rest of Hyprland.
    readonly property int radius: 0
    // Frame border thickness (px), like Hyprland's border_size.
    readonly property int borderWidth: 2
    // Internal element borders (preview tiles, etc.)
    readonly property int borderWidthInner: 1

    // Modal backdrop for the pickers: when one is open, everything behind it is
    // dimmed (this scrim) + blurred (the Hyprland layer_rule in modules/rules.lua)
    // so you focus on the widget — like dim_around. 0.0 = no dim, 1.0 = black.
    readonly property real dimAround: 0.45

    // ── Grayscale base ──────────────────────────────────────────────────────
    readonly property color base00: "#000000"   // surface
    readonly property color base01: "#0e0e10"   // raised
    readonly property color base02: "#18181b"   // hover / track
    readonly property color base03: "#2c2c31"   // strong border
    readonly property color base04: "#a0a0a6"   // secondary text
    readonly property color base05: "#f4f4f5"   // text
    readonly property color base06: "#bdbdc2"
    readonly property color base07: "#f2f2f3"
    readonly property color base08: accent        // workspace / primary marker
    readonly property color base09: base06
    readonly property color base0A: warning        // search/close hints
    readonly property color base0B: accent         // selection / insert / multi-select
    readonly property color base0C: accent
    readonly property color base0D: accent         // functions / window marker / section
    readonly property color base0E: base06
    readonly property color base0F: base03

    // ── Surfaces ────────────────────────────────────────────────────────────
    readonly property color surface:          base00
    readonly property color surfaceContainer: base00
    readonly property color surfaceDim:       base00
    readonly property color surfaceBright:    base02
    readonly property color surfaceVariant:   base03
    readonly property color outline:          base03                     // grey frame, = Hyprland inactive_border
    readonly property color outlineVariant:   Qt.rgba(1, 1, 1, 0.09)

    // ── Grid / cells (workspace grid, legacy) ───────────────────────────────
    readonly property color gridBackground: surface
    readonly property color gridBorder: outline
    readonly property color cellBackground: base01
    readonly property color cellBorder: outlineVariant
    readonly property color cellActiveBackground: Qt.rgba(accent.r, accent.g, accent.b, 0.12)
    readonly property color cellActiveBorder: accent
    readonly property color windowBackground: Qt.rgba(base03.r, base03.g, base03.b, 0.85)
    readonly property color windowBorder: outline
    readonly property color bgNumber: base04
    readonly property color cardBackground: surface
    readonly property color cardBorder: outline
    readonly property color activeCircle: accent
    readonly property color inactiveCircle: base02
    readonly property color activeText: base00
    readonly property color inactiveText: base04
    readonly property color activeDot: base05
    readonly property color inactiveDot: base03

    // ── Corner indicator ────────────────────────────────────────────────────
    readonly property color cornerBackground: surface
    readonly property color cornerBorder:     outline
    readonly property color cornerBorderColor: outline
    readonly property color cornerText:       base05

    // ── Pickers and popups (workspace / window / smart / menu / notifications) ──
    readonly property color pickerBackground: surface
    readonly property color pickerBorder: accent      // focused panels: cyan, like the active window
    readonly property color pickerInputBackground: base01
    readonly property color pickerInputBorder: outline
    readonly property color pickerInputFocusBorder: accent
    readonly property color pickerListBackground: surface
    readonly property color pickerItemSelected: Qt.rgba(accent.r, accent.g, accent.b, 0.18)
    readonly property color pickerItemHover: Qt.rgba(1, 1, 1, 0.07)
    readonly property color pickerText: base05
    readonly property color pickerSecondaryText: base04
    readonly property color pickerPlaceholder: base04
    readonly property color pickerHintText: Qt.rgba(base04.r, base04.g, base04.b, 0.9)
}
