import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

// Which-key menu — hold Super: after Config.holdDelay every shortcut from modules/menu.lua
// appears, by section, and stays. Each entry's key is its shortcut without Super, so press
// it (Super released or still down) to run it; a chord (e.g. Shift+L for layouts) shows its
// keys next. Meanwhile Hyprland is in the "whichkey" submap (menu.lua), so Super+key reaches
// the menu instead of the shortcuts; quick Super+key combos never see it.
// The chord's own shortcut (Super+Shift+L) opens the menu on it directly.
// Backspace goes back, Esc or a click outside closes.
// Run with: qs -p .
ShellRoot {
    id: root

    property var menu: ({ sections: [] })
    property var chord: null      // the chord entry being shown, or null for the whole menu
    property bool held: false     // opened by holding Super: Hyprland is in the submap

    FileView {
        path: Config.menuFile
        watchChanges: true
        onFileChanged: reload()
        onLoaded: {
            try { root.menu = JSON.parse(text()) } catch (e) { console.warn("whichkey: bad menu file", e) }
        }
    }

    readonly property var chords: {
        var out = []
        menu.sections.forEach(function(s) { s.items.forEach(function(it) { if (it.items) out.push(it) }) })
        return out
    }
    // The entries the next key can pick
    readonly property var keyable: chord ? chord.items
        : [].concat.apply([], menu.sections.map(function(s) { return s.items }))

    function open(chordPath, byHold) {
        holdTimer.stop()
        chord = chordPath ? chords.find(function(c) { return c.path === chordPath }) ?? null : null
        held = byHold
        win.visible = true
        if (byHold) hypr('hl.dsp.submap("whichkey")')
    }
    function close() {
        win.visible = false
        if (held) { held = false; hypr('hl.dsp.submap("reset")') }
    }
    function choose(it) {
        if (it.items) { chord = it; return }
        var wasHeld = held
        win.visible = false
        held = false
        // leave the submap and run the entry in one go, once the overlay has let go of the keyboard
        hypr('function() ' + (wasHeld ? 'hl.dispatch(hl.dsp.submap("reset")); ' : '')
             + 'whichkey_run("' + it.path + '") end', 30)
    }

    // The pressed key in menu.lua's format: "N", "Shift+N", "Ctrl+Shift+S", "Enter"
    function keyName(event) {
        var k = ""
        if (event.key >= Qt.Key_A && event.key <= Qt.Key_Z) k = String.fromCharCode(event.key)
        else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) k = "Enter"
        else if (event.key === Qt.Key_Space) k = "Space"
        if (!k) return ""
        var m = event.modifiers, out = []
        if (m & Qt.ControlModifier) out.push("Ctrl")
        if (m & Qt.AltModifier) out.push("Alt")
        if (m & Qt.ShiftModifier) out.push("Shift")
        out.push(k)
        return out.join("+")
    }

    // hyprctl dispatch <lua expression>, optionally after a delay (ms). Each call is its own
    // detached process: with one shared Process, a call made while the previous one was still
    // running (the submap switch, then the chosen entry right after) was silently dropped.
    function hypr(expr, delay) {
        if (!delay) { Quickshell.execDetached(["hyprctl", "dispatch", expr]); return }
        pendingExpr = expr
        hyprDelay.restart()
    }
    property string pendingExpr: ""

    // Safety net: Hyprland in the menu's submap with no menu on screen (a late switch, a
    // crash) would leave every Super+key dead, so put it back right away.
    Connections {
        target: Hyprland
        function onRawEvent(event) {
            if (event.name === "submap" && event.data === "whichkey" && !win.visible)
                Quickshell.execDetached(["hyprctl", "dispatch", 'hl.dsp.submap("reset")'])
        }
    }
    Timer { id: hyprDelay; interval: 30; onTriggered: Quickshell.execDetached(["hyprctl", "dispatch", root.pendingExpr]) }

    // Hyprland sends pressed when Super goes down and released when it comes up.
    GlobalShortcut {
        name: "whichkey-hold"
        description: "Super held: open the menu after a moment"
        onPressed: if (!win.visible) { root.holdCancelled = false; startPos.running = true; holdTimer.restart() }
        onReleased: {
            holdTimer.stop()   // released before the delay: it was a quick combo
            if (win.visible && root.held) autoCloseTimer.restart()
        }
    }
    Timer { id: holdTimer; interval: Config.holdDelay; onTriggered: if (!win.visible) holdCheck.running = true }

    // Where the pointer was when Super went down, to tell a Super+mouse drag from a hold
    property var pressPos: null
    function parsePos(line) {
        var m = /(-?\d+)\D+(-?\d+)/.exec(line)
        return m ? { x: parseInt(m[1]), y: parseInt(m[2]) } : null
    }
    Process {
        id: startPos
        command: ["hyprctl", "cursorpos"]
        stdout: StdioCollector { onStreamFinished: root.pressPos = root.parsePos(text) }
    }
    // After the delay: open unless the pointer moved (dragging a window) or a panel that
    // has the keyboard is showing (a picker, the notification history, tofi).
    Process {
        id: holdCheck
        command: ["sh", "-c", "hyprctl cursorpos; hyprctl layers -j | jq -r '.. | .namespace? // empty'"]
        stdout: StdioCollector {
            onStreamFinished: {
                var lines = text.split("\n"), pos = root.parsePos(lines[0])
                var moved = pos && root.pressPos
                    && Math.max(Math.abs(pos.x - root.pressPos.x), Math.abs(pos.y - root.pressPos.y)) > Config.moveTolerance
                var blocked = lines.slice(1).some(function(ns) { return Config.blockers.indexOf(ns) >= 0 })
                if (!moved && !blocked && !root.holdCancelled && !win.visible) root.open("", true)
            }
        }
    }
    // Held open, Super released and then left alone: close by itself
    Timer {
        id: autoCloseTimer
        interval: root.chord ? Config.chordAutoClose : Config.autoClose
        onTriggered: if (win.visible && root.held) root.close()
    }
    // Sent by every direct shortcut (menu.lua, keybindings.lua): Super was for a combo
    property bool holdCancelled: false
    GlobalShortcut {
        name: "whichkey-cancel"
        description: "A shortcut fired: don't open the menu, close it if a hold opened it"
        onPressed: {
            root.holdCancelled = true
            holdTimer.stop()
            if (win.visible && root.held) root.close()
        }
    }
    GlobalShortcut {
        name: "whichkey-close"
        description: "Close the menu (Esc in the whichkey submap)"
        onPressed: { root.held = false; win.visible = false }
    }
    // One shortcut per chord ("whichkey-3-9"), bound by menu.lua to the chord's own key.
    Instantiator {
        model: root.chords
        delegate: GlobalShortcut {
            required property var modelData
            name: "whichkey-" + modelData.path.replace(/\./g, "-")
            description: "Which-key menu on a chord"
            onPressed: root.open(modelData.path, false)
        }
    }

    PanelWindow {
        id: win
        visible: false
        screen: {
            // the first of Config.monitors that is connected, otherwise the first screen
            for (var i = 0; i < Config.monitors.length; i++) {
                var want = Config.monitors[i]
                var s = Quickshell.screens.find(function(x) { return x.name === want || x.model === want })
                if (s) return s
            }
            return Quickshell.screens[0]
        }
        anchors.top: true
        anchors.bottom: true
        anchors.left: true
        anchors.right: true
        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
        WlrLayershell.namespace: "whichkey"
        exclusionMode: ExclusionMode.Ignore
        color: "transparent"

        onVisibleChanged: if (visible) keys.forceActiveFocus()

        MouseArea { anchors.fill: parent; onClicked: root.close() }   // click outside closes

        Item {
            id: keys
            anchors.fill: parent
            focus: true
            Keys.onPressed: function(event) {
                if (autoCloseTimer.running) autoCloseTimer.restart()
                if (event.key === Qt.Key_Escape) root.close()
                else if (event.key === Qt.Key_Backspace) {
                    if (root.chord) root.chord = null
                    else root.close()
                } else {
                    var name = root.keyName(event)
                    var it = name ? root.keyable.find(function(x) { return x.key === name }) : null
                    if (it) root.choose(it)
                }
                event.accepted = true
            }
        }

        Rectangle {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.bottom: parent.bottom
            anchors.bottomMargin: Config.bottomMargin
            width: Math.min(Math.round(Config.width * Theme.textScale), parent.width - 80)
            height: body.implicitHeight + 40
            color: Theme.pickerBackground
            border.color: Theme.pickerBorder
            border.width: Theme.borderWidth
            radius: Theme.radius

            MouseArea { anchors.fill: parent }   // clicks on the card don't close it

            ColumnLayout {
                id: body
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.top: parent.top
                anchors.margins: 20
                anchors.leftMargin: 28
                anchors.rightMargin: 28
                spacing: 16

                RowLayout {
                    Layout.fillWidth: true
                    Text {
                        Layout.fillWidth: true
                        text: root.chord ? "Super  " + root.chord.key + "  ›  " + root.chord.label : "Super"
                        color: Theme.pickerSecondaryText
                        font.pixelSize: Math.round((Config.fontSize - 3) * Theme.textScale)
                        font.family: Theme.fontFamily
                    }
                    Text {
                        text: (root.chord ? "Backspace indietro   " : "") + "Esc chiudi"
                        color: Theme.pickerHintText
                        font.pixelSize: Math.round((Config.fontSize - 3) * Theme.textScale)
                        font.family: Theme.fontFamily
                    }
                }

                Rectangle { Layout.fillWidth: true; implicitHeight: 1; color: Theme.outline }

                GridLayout {
                    Layout.fillWidth: true
                    columns: Config.columns
                    columnSpacing: 36
                    rowSpacing: 22

                    // The whole menu by section, or just the chord's keys
                    Repeater {
                        model: root.chord ? [{ title: root.chord.label, items: root.chord.items }] : root.menu.sections
                        delegate: ColumnLayout {
                            required property var modelData
                            Layout.fillWidth: true
                            Layout.preferredWidth: 1   // equal columns
                            Layout.alignment: Qt.AlignTop
                            spacing: 6

                            Text {
                                // section heading: small, spaced capitals, quiet
                                text: modelData.title.toUpperCase()
                                color: Theme.pickerSecondaryText
                                font.pixelSize: Math.round((Config.fontSize - 5) * Theme.textScale)
                                font.family: Theme.fontFamily
                                font.letterSpacing: 1.5
                                bottomPadding: 4
                            }
                            Repeater {
                                model: modelData.items
                                delegate: Item {
                                    required property var modelData
                                    Layout.fillWidth: true
                                    implicitHeight: entry.implicitHeight + 6

                                    Rectangle {
                                        anchors.fill: parent
                                        color: Theme.pickerItemHover
                                        visible: entryMouse.containsMouse
                                    }
                                    RowLayout {
                                        id: entry
                                        anchors.left: parent.left
                                        anchors.right: parent.right
                                        anchors.verticalCenter: parent.verticalCenter
                                        anchors.leftMargin: 4
                                        spacing: 12
                                        Text {
                                            Layout.preferredWidth: Math.round(Config.keyWidth * Theme.textScale)
                                            text: modelData.key
                                            color: Theme.accent2
                                            font.pixelSize: Math.round((Config.fontSize - 1) * Theme.textScale)
                                            font.family: Theme.fontFamily
                                            font.bold: true
                                        }
                                        Text {
                                            Layout.fillWidth: true
                                            text: modelData.items ? modelData.label + "  ›" : modelData.label
                                            color: Theme.pickerText
                                            font.pixelSize: Math.round((Config.fontSize - 1) * Theme.textScale)
                                            font.family: Theme.fontFamily
                                            elide: Text.ElideRight
                                        }
                                    }
                                    MouseArea {
                                        id: entryMouse
                                        anchors.fill: parent
                                        hoverEnabled: true
                                        onClicked: root.choose(modelData)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
