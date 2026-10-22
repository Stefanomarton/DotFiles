import Quickshell
import Quickshell.Hyprland
import QtQuick

ShellRoot {

    // GlobalShortcut for workspace picker
    GlobalShortcut {
        name: "workspaces"
        description: "Toggle workspace picker"
        onPressed: WorkspaceManager.toggle()
    }

    // GlobalShortcut for window picker
    GlobalShortcut {
        name: "windows"
        description: "Toggle window picker"
        onPressed: WindowManager.toggle()
    }

    // GlobalShortcut for smart picker (unified)
    GlobalShortcut {
        name: "smart"
        description: "Toggle smart unified picker"
        onPressed: SmartPickerManager.toggle()
    }

    // GlobalShortcut for move-window picker
    GlobalShortcut {
        name: "movewindow"
        description: "Move focused window to a chosen workspace"
        onPressed: MoveWindowManager.toggle()
    }

    // GlobalShortcut for Exposé overview
    GlobalShortcut {
        name: "expose"
        description: "Toggle Exposé-style workspace overview"
        onPressed: ExposeManager.toggle()
    }

    // Close every picker (Super+Esc, "pulisci schermo" in modules/menu.lua)
    GlobalShortcut {
        name: "pickers-close"
        description: "Close every picker"
        onPressed: {
            WorkspaceManager.hide()
            WindowManager.hide()
            SmartPickerManager.hide()
            MoveWindowManager.hide()
            ExposeManager.hide()
        }
    }

    // Workspace picker popup
    WorkspacePicker {}

    // Window picker popup
    WindowPicker {}

    // Smart unified picker popup
    SmartPicker {}

    // Move-window picker popup
    MoveWindowPicker {}

    // Exposé overview
    Expose {}
}
