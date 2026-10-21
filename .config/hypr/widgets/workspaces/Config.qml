pragma Singleton
import QtQuick

QtObject {
    // === Workspace Picker ===
    readonly property var startupWorkspaces: []        // Named workspaces to create on shell load, e.g., ["thesis", "work"]

    // Workspaces to exclude from picker (by exact name).
    // NOTE: all Hyprland "special:" workspaces are always hidden automatically,
    // so they don't need to be listed here.
    readonly property var pickerExcludedWorkspaces: ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

    // Windows on these workspaces are excluded from window picker
    // Use strings: "1", "2", "special:scratch", etc. A trailing "*" matches a prefix.
    readonly property var windowPickerExcludedWorkspaces: ["special:scratch_editor", "special:scratch_qalc", "special:scratch_yazi*", "w11"]

    function isWindowWorkspaceExcluded(ws) {
        return windowPickerExcludedWorkspaces.some(function(ex) {
            return ex.endsWith("*") ? ws.startsWith(ex.slice(0, -1)) : ws === ex
        })
    }

    // Target monitor for "move workspace" action (Ctrl+Enter in picker)
    // "auto" = move to first available other monitor
    // Or a monitor name ("DP-2") or model ("Mi Monitor": same screen whatever DP-x the laptop dock gives it)
    readonly property string moveWorkspaceTargetMonitor: "Mi Monitor"

    // Picker monitor - which monitor to show pickers on
    // "focused" = show on currently focused monitor (default)
    // Or a monitor name ("DP-1") or model ("Odyssey G91F", the G9 on desktop and docked laptops);
    // if it isn't connected, the first screen
    readonly property string pickerMonitor: "Odyssey G91F"

    // Preview workspace when navigating in window picker
    // When true, pressing ↑↓ will temporarily switch to the workspace of the highlighted window
    readonly property bool pickerPreviewOnNavigate: false

    // Minimum characters typed before preview triggers when filtering
    // 0 = preview immediately (same as arrow navigation)
    // Set to higher value (e.g., 3) to only preview after typing 3+ characters
    readonly property int pickerPreviewMinChars: 3

    // Smart picker: workspace mini-map preview
    // Shows a scaled map of the highlighted workspace's window layout in a pane
    // beside the picker as you navigate.
    readonly property bool pickerWorkspacePreview: true
    // When true, each window tile shows a live capture of the window (Hyprland
    // toplevel export). When false, tiles show only the app icon (option 1).
    readonly property bool pickerWorkspacePreviewLive: true
    // Workspace preview sits below the list. The mini-map keeps the monitor's
    // aspect ratio, fitted within these max dimensions (height is the usual
    // limit on a wide monitor; width caps it on a tall/square one).
    readonly property int pickerPreviewHeight: 520
    readonly property int pickerPreviewWidth: 2400
    // Rows visible before the list scrolls, so the preview below stays put.
    // Workspace picker: a single column. Smart picker: two columns.
    readonly property int pickerWorkspaceVisibleRows: 3
    readonly property int pickerSmartVisibleRows: 4
    readonly property int pickerWindowVisibleRows: 6

    // Exposé overview: width of each workspace mini-map cell (height follows the
    // monitor's aspect ratio). Bigger = fewer cells per row.
    readonly property int exposeCellWidth: 480

    // Picker UI
    readonly property int pickerWidth: 750
    readonly property int pickerMaxHeight: 1200
    readonly property int pickerFontSize: 20
    readonly property int pickerItemHeight: 60
    readonly property int pickerBorderRadius: Theme.radius
    readonly property int pickerInputHeight: 44
}
