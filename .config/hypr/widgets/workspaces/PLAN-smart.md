# Smart picker unico — piano di implementazione

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** un solo picker (Super+S) con lista unica ordinata e anteprima a lato, al posto di Workspace, Finestre, Sposta finestra e tofi-run (spec: `SPEC-smart.md`).

**Architecture:** ordinamento puro in `rank.js` (`.pragma library`, testato con node); `SmartPickerManager.qml` prende i dati dagli oggetti IPC di `Quickshell.Hyprland` e da `DesktopEntries` (nessun processo) e agisce con `Hyprland.dispatch` (dispatcher Lua; più azioni = una `function() … end`); `SmartPicker.qml` è la sola UI.

**Tech Stack:** Quickshell 0.3.0 (QML/Qt 6), Hyprland 0.56 con config Lua, node per il test.

## Global Constraints

- Cartella: `~/.dotfiles/.config/hypr/widgets/workspaces/` (= `~/.config/hypr/widgets/workspaces/`).
- Modificare i file **in place** (Edit/Write o `open(p,'w')`): `sed -i`/`git checkout` sostituiscono il file e il live reload di Quickshell lo perde.
- Niente commit durante il lavoro: il repo `.dotfiles` pubblica ogni commit (hook post-commit). Un solo commit alla fine, dopo l'ok di Stefano.
- Font Geist Mono: niente glifo ⌫ nei testi, scrivere "Backspace".
- Tasti: non simularli mentre Stefano usa il PC.
- Indirizzi delle finestre per i dispatcher: `address:0x…`.
- Verifica runtime: `qs log -p ~/.config/hypr/widgets/workspaces | tail -20` senza errori nuovi dopo il reload; `HYPRLAND_INSTANCE_SIGNATURE=$(ls -t /run/user/1000/hypr/ | head -1)` prima di `hyprctl`.

---

### Task 1: Ordinamento (`rank.js`) con test

**Files:**
- Create: `widgets/workspaces/rank.js`
- Test: `widgets/workspaces/rank_test.js`

**Interfaces:**
- Produces: `Rank.parse(text) -> {letter: ""|"w"|"p"|"a"|"m"|">", query: string (minuscolo, trim), raw: string (trim, maiuscole intatte)}`;
  `Rank.build(text, data) -> [row]` con `data = {windows:[{address,title,cls,workspace,focus,icon}], workspaces:[{name, windowTitles:[string]}], apps:[{name,generic,keywords,id,wmClass,ref,icon}], currentWorkspace, currentAddress}`;
  `row = {kind: "ws"|"win"|"app"|"cmd"|"new", title, subtitle, score, icon}` più `name` (ws/new), `address`+`workspace` (win), `ref` (app), `command` (cmd).

- [ ] **Step 1: scrivere il test**

`rank_test.js`:

```js
// node rank_test.js — loads rank.js (a QML .pragma library) and checks the ranking.
const fs = require("fs"), vm = require("vm"), assert = require("assert")
const src = fs.readFileSync(__dirname + "/rank.js", "utf8").replace(/^\.pragma library/m, "")
const R = {}
vm.runInNewContext(src, R)

const data = {
    currentWorkspace: "web", currentAddress: "0x1",
    workspaces: [
        { name: "web", windowTitles: ["GitHub", "zsh"] },
        { name: "notes", windowTitles: ["todo.org"] },
        { name: "webdev", windowTitles: [] },
    ],
    windows: [
        { address: "0x1", title: "GitHub", cls: "firefox", workspace: "web", focus: 0, icon: "firefox" },
        { address: "0x2", title: "todo.org", cls: "emacs", workspace: "notes", focus: 2, icon: "emacs" },
        { address: "0x3", title: "zsh", cls: "kitty", workspace: "web", focus: 1, icon: "kitty" },
    ],
    apps: [
        { name: "Firefox", generic: "Web Browser", keywords: "internet", id: "firefox", wmClass: "firefox", ref: "ff", icon: "firefox" },
        { name: "Firefox Developer Edition", generic: "Web Browser", keywords: "", id: "firefox-devedition", wmClass: "firefox-aurora", ref: "ffdev", icon: "x" },
        { name: "Notes", generic: "Note taking", keywords: "", id: "org.gnome.Notes", wmClass: "", ref: "notesapp", icon: "x" },
    ],
}
const kinds = rows => rows.map(r => r.kind + ":" + r.title)

// empty: windows by last use without the current one, then workspaces without the current one, no apps
assert.deepStrictEqual(kinds(R.build("", data)), ["win:zsh", "win:todo.org", "ws:notes", "ws:webdev"])
// an app with an open window is hidden (its window shows), a closed one is offered; create row last
assert.deepStrictEqual(kinds(R.build("fire", data)), ["win:GitHub", "app:Firefox Developer Edition", "new:fire"])
// on equal score a workspace beats an app
assert.deepStrictEqual(kinds(R.build("note", data)), ["ws:notes", "app:Notes", "new:note"])
// the current workspace sinks below the others; no create row when the name exists
assert.deepStrictEqual(kinds(R.build("w web", data)), ["ws:webdev", "ws:web"])
// move mode: workspaces and the create row only
assert.deepStrictEqual(kinds(R.build("m proj", data)), ["new:proj"])
// "a " with no text lists the closed apps alphabetically
assert.deepStrictEqual(kinds(R.build("a ", data)), ["app:Firefox Developer Edition", "app:Notes"])
// shell command keeps case and spaces
assert.deepStrictEqual(R.build("> notify-send Hi", data).map(r => [r.kind, r.command]), [["cmd", "notify-send Hi"]])
assert.deepStrictEqual(R.build(">", data), [])
// payloads
assert.strictEqual(R.build("p todo", data)[0].address, "0x2")
assert.strictEqual(R.build("fire", data)[1].ref, "ffdev")
assert.strictEqual(R.build("New Proj", data)[0].name, "New Proj")
console.log("rank ok")
```

- [ ] **Step 2: eseguirlo, deve fallire**

Run: `node ~/.config/hypr/widgets/workspaces/rank_test.js`
Expected: FAIL, `ENOENT … rank.js`.

- [ ] **Step 3: scrivere `rank.js`**

```js
.pragma library
// Pure ranking for the Smart picker: no Qt here, so rank_test.js runs it with node.

// Tie order when scores are equal
var KIND_ORDER = { ws: 0, win: 1, app: 2, new: 3, cmd: 4 }

// "w " workspaces · "p " windows · "a " apps · "m " move the current window · ">" shell command
function parse(text) {
    var t = text || ""
    if (t.charAt(0) === ">") return { letter: ">", query: "", raw: t.slice(1).trim() }
    var m = t.match(/^([wpam]) /)
    var rest = m ? t.slice(2) : t
    return { letter: m ? m[1] : "", query: rest.trim().toLowerCase(), raw: rest.trim() }
}

function fuzzy(text, q) {
    var j = 0
    for (var i = 0; i < text.length && j < q.length; i++) if (text[i] === q[j]) j++
    return j === q.length
}

// exact > prefix > contains > fuzzy; 0 = no match
function score(text, q) {
    var t = (text || "").toLowerCase()
    if (t === q) return 1000
    if (t.startsWith(q)) return 500
    if (t.includes(q)) return 100
    return fuzzy(t, q) ? 50 : 0
}

function wsSubtitle(titles) {
    if (titles.length === 0) return "Empty workspace"
    var s = titles.slice(0, 3).join(", ")
    return titles.length > 3 ? s + " +" + (titles.length - 3) + " more" : s
}

function build(text, data) {
    var p = parse(text)
    if (p.letter === ">")
        return p.raw ? [{ kind: "cmd", title: p.raw, subtitle: "Run command", score: 1,
                          command: p.raw, icon: "utilities-terminal" }] : []

    var q = p.query, rows = [], i, s
    function want(kind) {
        if (p.letter === "w" || p.letter === "m") return kind === "ws" || kind === "new"
        if (p.letter === "p") return kind === "win"
        if (p.letter === "a") return kind === "app"
        return true
    }

    if (want("ws")) for (i = 0; i < data.workspaces.length; i++) {
        var ws = data.workspaces[i]
        var current = ws.name === data.currentWorkspace
        if (!q) s = current ? 0 : 1
        else {
            s = score(ws.name, q)
            if (s && current) s = Math.max(1, s - 600)   // switching to it is a no-op
        }
        if (s) rows.push({ kind: "ws", name: ws.name, title: ws.name, subtitle: wsSubtitle(ws.windowTitles),
                           score: s, icon: "folder" })
    }

    if (want("win")) for (i = 0; i < data.windows.length; i++) {
        var w = data.windows[i]
        if (!q) s = w.address === data.currentAddress ? 0 : 900 - Math.min(w.focus, 800)   // last used first
        else s = Math.max(score(w.title, q), score(w.cls, q))
        if (s) rows.push({ kind: "win", address: w.address, workspace: w.workspace, title: w.title || w.cls,
                           subtitle: w.cls + " — " + w.workspace, score: s, icon: w.icon })
    }

    // Apps only while typing (or with "a "), and only if no window of theirs is open
    if (want("app") && (q || p.letter === "a")) {
        var open = {}
        for (i = 0; i < data.windows.length; i++) open[data.windows[i].cls.toLowerCase()] = true
        for (i = 0; i < data.apps.length; i++) {
            var a = data.apps[i], id = a.id.toLowerCase()
            if (open[a.wmClass.toLowerCase()] || open[id] || open[id.split(".").pop()]) continue
            s = q ? Math.max(score(a.name, q), Math.min(100, score(a.generic + " " + a.keywords, q))) : 1
            if (s) rows.push({ kind: "app", ref: a.ref, title: a.name, subtitle: a.generic || "Application",
                               score: s, icon: a.icon })
        }
    }

    rows.sort(function(x, y) {
        return y.score - x.score || KIND_ORDER[x.kind] - KIND_ORDER[y.kind] || x.title.localeCompare(y.title)
    })

    // Typed a name no workspace has: offer to create it, last
    if (want("new") && q && !data.workspaces.some(function(ws) { return ws.name.toLowerCase() === q }))
        rows.push({ kind: "new", name: p.raw, title: p.raw, subtitle: "Create workspace", score: 0, icon: "folder-new" })
    return rows
}
```

- [ ] **Step 4: eseguirlo, deve passare**

Run: `node ~/.config/hypr/widgets/workspaces/rank_test.js`
Expected: `rank ok`

---

### Task 2: `SmartPickerManager.qml` sui dati nativi

**Files:**
- Rewrite: `widgets/workspaces/SmartPickerManager.qml`

**Interfaces:**
- Consumes: `Rank.parse`, `Rank.build` (Task 1).
- Produces (usati da `SmartPicker.qml`): `pickerVisible`, `filterText`, `selectedIndex`, `selectedWindows`, `parsed`, `moveMode`, `openedTitle`, `results`, `selected`, `windowsOn(wsName) -> [window]`, `monitorFor(wsName) -> {x,y,width,height}|null`, `isSelected(addr)`, `toggleSelection(addr)`, `activate(row, modifiers)`, `closeItem(row)`, `show()`, `hide()`, `toggle()`. `window = {address,title,cls,workspace,focus,icon,x,y,w,h,wayland}`.

- [ ] **Step 1: riscrivere il file**

```qml
pragma Singleton
import Quickshell
import Quickshell.Hyprland
import QtQuick
import "rank.js" as Rank

// Smart picker state and actions. Data comes from Quickshell's Hyprland IPC objects
// and DesktopEntries (no processes); actions go through Hyprland.dispatch with the
// Lua dispatchers of the Hyprland config (several at once = one Lua function).
QtObject {
    id: manager

    property bool pickerVisible: false
    property string filterText: ""
    property int selectedIndex: 0
    property var selectedWindows: []      // addresses picked with Tab

    // Captured when the picker opens: target of "m " and where to stay
    property string openedAddress: ""
    property string openedTitle: ""
    property string openedWorkspace: ""

    readonly property var parsed: Rank.parse(filterText)
    readonly property bool moveMode: parsed.letter === "m"
    readonly property string currentWorkspaceName: Hyprland.activeWorkspace?.name ?? ""
    readonly property string currentMonitorName: Hyprland.focusedMonitor?.name ?? ""

    // Visible windows, with geometry and focus order from the last IPC refresh (show())
    readonly property var windows: {
        var out = []
        var tls = Hyprland.toplevels.values
        for (var i = 0; i < tls.length; i++) {
            var t = tls[i]
            var o = t.lastIpcObject || {}
            if (o.hidden) continue
            var cls = o["class"] || ""
            var at = o.at || [0, 0], sz = o.size || [0, 0]
            out.push({
                address: hexAddress(t.address),
                title: t.title || o.title || "",
                cls: cls,
                workspace: t.workspace ? t.workspace.name : "",
                focus: o.focusHistoryID ?? 999,
                icon: normalizeAppId(cls),
                x: at[0], y: at[1], w: sz[0], h: sz[1],
                wayland: t.wayland
            })
        }
        return out
    }

    // "special" / "special:*" workspaces are always hidden
    readonly property var workspaces: Hyprland.workspaces.values.filter(function(ws) {
        return ws.name && ws.name !== "special" && !ws.name.startsWith("special:")
            && Config.pickerExcludedWorkspaces.indexOf(ws.name) < 0
    })

    readonly property var apps: DesktopEntries.applications.values
        .filter(function(e) { return !e.noDisplay })
        .map(function(e) {
            return { name: e.name, generic: e.genericName || "", keywords: (e.keywords || []).join(" "),
                     id: e.id, wmClass: e.startupWmClass || "", ref: e,
                     icon: e.icon || "application-x-executable" }
        })

    readonly property var results: {
        if (!pickerVisible) return []
        var shown = windows.filter(function(w) { return !Config.isWindowWorkspaceExcluded(w.workspace) })
        return Rank.build(filterText, {
            windows: shown,
            workspaces: workspaces.map(function(ws) {
                return { name: ws.name, windowTitles: shown.filter(function(w) { return w.workspace === ws.name })
                                                           .map(function(w) { return w.title || w.cls }) }
            }),
            apps: apps,
            currentWorkspace: currentWorkspaceName,
            currentAddress: openedAddress
        })
    }
    readonly property var selected: results[selectedIndex] ?? null

    onFilterTextChanged: selectedIndex = 0   // Enter = first row
    onResultsChanged: if (selectedIndex >= results.length) selectedIndex = Math.max(0, results.length - 1)

    // === Helpers ===
    function hexAddress(a) { a = String(a); return a.startsWith("0x") ? a : "0x" + a }

    function normalizeAppId(appId) {
        if (!appId) return "application-x-executable"
        return appId.replace(/-browser$/, "").replace(/-wayland$/, "").replace(/-x11$/, "")
    }

    function windowsOn(wsName) {
        return windows.filter(function(w) { return w.workspace === wsName })
    }

    // Logical bounds of the monitor a workspace is on, for the preview
    function monitorFor(wsName) {
        var ws = Hyprland.workspaces.values.find(function(w) { return w.name === wsName })
        var m = ws ? ws.monitor : null
        if (!m || !m.width) return null
        var sc = m.scale || 1
        return { x: m.x, y: m.y, width: m.width / sc, height: m.height / sc }
    }

    // Alt+Enter target: Config.moveWorkspaceTargetMonitor (name or model) if it is another
    // monitor, else the first other one
    function otherMonitor() {
        var cfg = Config.moveWorkspaceTargetMonitor
        var others = Hyprland.monitors.values.filter(function(m) { return m.name !== currentMonitorName })
        var pick = others.find(function(m) {
            return m.name === cfg || (m.lastIpcObject && m.lastIpcObject.model === cfg)
        })
        return (pick || others[0])?.name ?? ""
    }

    function isSelected(address) { return selectedWindows.indexOf(address) >= 0 }

    function toggleSelection(address) {
        selectedWindows = isSelected(address)
            ? selectedWindows.filter(function(a) { return a !== address })
            : selectedWindows.concat([address])
    }

    function clearSelection() { selectedWindows = [] }

    // === Dispatch ===
    function q(s) { return "'" + String(s).replace(/\\/g, "\\\\").replace(/'/g, "\\'") + "'" }
    function focusWs(name)  { return "hl.dsp.focus({ workspace = " + q("name:" + name) + " })" }
    function focusWin(addr) { return "hl.dsp.focus({ window = " + q("address:" + addr) + " })" }
    function moveWin(addr, ws) { return "hl.dsp.window.move({ workspace = " + q(ws) + ", window = " + q("address:" + addr) + " })" }
    function closeWin(addr) { return "hl.dsp.window.close({ window = " + q("address:" + addr) + " })" }
    function moveWs(name, mon) { return "hl.dsp.workspace.move({ workspace = " + q("name:" + name) + ", monitor = " + q(mon) + " })" }

    function run(calls) {
        if (calls.length === 0) return
        Hyprland.dispatch(calls.length === 1 ? calls[0]
            : "function() " + calls.map(function(c) { return "hl.dispatch(" + c + ")" }).join("; ") + " end")
    }

    // === Open / close ===
    function show() {
        Hyprland.refreshToplevels()
        Hyprland.refreshWorkspaces()
        Hyprland.refreshMonitors()
        var t = Hyprland.activeToplevel
        openedAddress = t ? hexAddress(t.address) : ""
        openedTitle = t ? t.title : ""
        openedWorkspace = currentWorkspaceName
        filterText = ""
        selectedIndex = 0
        clearSelection()
        pickerVisible = true
    }

    function hide() {
        pickerVisible = false
        filterText = ""
        clearSelection()
    }

    function toggle() { if (pickerVisible) hide(); else show() }

    // === Actions ===
    // Enter / click. Ctrl = bring here, Alt = workspace to the other monitor,
    // Shift = follow (move mode). With Tab-selected windows: a workspace row moves
    // them there, Ctrl+Enter anywhere brings them here.
    function activate(row, modifiers) {
        if (!row) return
        var ctrl = modifiers & Qt.ControlModifier, alt = modifiers & Qt.AltModifier
        var shift = modifiers & Qt.ShiftModifier
        var sel = selectedWindows

        if (ctrl && sel.length > 0) {
            run(sel.map(function(a) { return moveWin(a, "e+0") }))
        } else if (row.kind === "ws" || row.kind === "new") {
            if (moveMode) {
                if (!openedAddress) return
                var calls = [moveWin(openedAddress, "name:" + row.name)]
                calls = calls.concat(shift ? [focusWs(row.name), focusWin(openedAddress)] : [focusWs(openedWorkspace)])
                run(calls)
            } else if (sel.length > 0) {
                run(sel.map(function(a) { return moveWin(a, "name:" + row.name) }).concat([focusWs(openedWorkspace)]))
            } else if (ctrl && row.kind === "ws") {
                run([moveWs(row.name, currentMonitorName), focusWs(row.name)])
            } else if (alt && row.kind === "ws") {
                var mon = otherMonitor()
                if (mon) run([moveWs(row.name, mon)])
            } else {
                run([focusWs(row.name)])   // a missing named workspace is created
            }
        } else if (row.kind === "win") {
            run(ctrl ? [moveWin(row.address, "e+0"), focusWin(row.address)] : [focusWin(row.address)])
        } else if (row.kind === "app") {
            row.ref.execute()
        } else if (row.kind === "cmd") {
            Quickshell.execDetached(["zsh", "-c", row.command])
        }
        hide()
    }

    // Alt+Backspace: close the window (or the selected ones), or every window of a
    // workspace. The picker stays open; the lists update from Hyprland's events.
    function closeItem(row) {
        if (!row) return
        if (row.kind === "win") {
            var addrs = selectedWindows.length > 0 ? selectedWindows : [row.address]
            run(addrs.map(closeWin))
            clearSelection()
        } else if (row.kind === "ws") {
            run(windowsOn(row.name).map(function(w) { return closeWin(w.address) }))
        }
    }
}
```

- [ ] **Step 2: verificare il caricamento**

Run: `sleep 2; qs log -p ~/.config/hypr/widgets/workspaces | tail -20`
Expected: `Configuration Loaded`; nessun `ReferenceError`/`TypeError` che citi `SmartPickerManager.qml` o `rank.js`. Errori in `SmartPicker.qml` su nomi spariti (`workspaceResults`, `typeWindow`, …) sono attesi fino al Task 3.

---

### Task 3: `SmartPicker.qml` — lista unica + anteprima a lato

**Files:**
- Rewrite: `widgets/workspaces/SmartPicker.qml`
- Modify: `widgets/workspaces/Config.qml` (`pickerSmartVisibleRows`)

**Interfaces:**
- Consumes: tutto il blocco "Produces" del Task 2.

- [ ] **Step 1: righe visibili in `Config.qml`**

Sostituire

```qml
    // Rows visible before the list scrolls, so the preview below stays put.
    // Workspace picker: a single column. Smart picker: two columns.
    readonly property int pickerWorkspaceVisibleRows: 3
    readonly property int pickerSmartVisibleRows: 4
    readonly property int pickerWindowVisibleRows: 6
```

con

```qml
    // Rows of the Smart list (fixed height, so nothing moves while typing)
    readonly property int pickerSmartVisibleRows: 8
```

- [ ] **Step 2: riscrivere `SmartPicker.qml`**

```qml
import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

// The Smart picker (Super+S): one ranked list of workspaces, windows, apps and
// shell commands, with a live mini-map of the highlighted row's workspace beside it.
PanelWindow {
    id: picker

    visible: SmartPickerManager.pickerVisible

    // Configured monitor (name or model), or the focused one
    screen: {
        if (Config.pickerMonitor === "focused") {
            return Quickshell.screens.find(function(s) {
                return Hyprland.focusedMonitor !== null && s.name === Hyprland.focusedMonitor.name
            }) ?? Quickshell.screens[0]
        }
        return Quickshell.screens.find(function(s) {
            return s.name === Config.pickerMonitor || s.model === Config.pickerMonitor
        }) ?? Quickshell.screens[0]
    }

    anchors.top: true
    anchors.bottom: true
    anchors.left: true
    anchors.right: true

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
    WlrLayershell.namespace: "smart-picker"

    exclusionMode: ExclusionMode.Ignore
    color: Qt.rgba(0, 0, 0, Theme.dimAround)   // dims the backdrop (blurred via layer_rule)

    // Warm up the Wayland toplevel list at startup, so live captures are ready at first open
    Component.onCompleted: { var warm = ToplevelManager.toplevels }

    // Click outside closes (delayed so the opening click doesn't count)
    property bool focusGrabActive: false
    Timer {
        id: focusGrabDelay
        interval: 100
        onTriggered: picker.focusGrabActive = true
    }
    onVisibleChanged: {
        if (visible) {
            focusGrabDelay.start()
            input.forceActiveFocus()
        } else {
            focusGrabActive = false
        }
    }
    HyprlandFocusGrab {
        windows: [picker]
        active: picker.focusGrabActive
        onCleared: SmartPickerManager.hide()
    }

    readonly property var item: SmartPickerManager.selected
    // Preview beside the list when the screen has room, else below it
    readonly property bool wide: width >= Config.pickerWidth + previewPane.implicitWidth + 80

    readonly property string hints: {
        var n = SmartPickerManager.selectedWindows.length
        if (SmartPickerManager.moveMode) return "Enter move · Shift+Enter move and follow · Esc"
        if (n > 0) return n + " selected · Enter on a workspace: move there · Ctrl+Enter: here · Alt+Backspace close · Esc"
        var k = item ? item.kind : ""
        if (k === "win") return "Enter focus · Ctrl+Enter here · Tab select · Alt+Backspace close · Esc"
        if (k === "ws") return "Enter go · Ctrl+Enter to this monitor · Alt+Enter other monitor · Alt+Backspace close all · Esc"
        if (k === "app") return "Enter launch · Esc"
        if (k === "cmd") return "Enter run · Esc"
        if (k === "new") return "Enter create · Esc"
        return "w· p· a· m· + space to narrow · > command · Esc"
    }

    Rectangle {
        id: card
        anchors.centerIn: parent
        width: layout.implicitWidth + 24
        height: layout.implicitHeight + 24
        radius: Config.pickerBorderRadius
        color: Theme.pickerBackground
        border.color: Theme.pickerBorder
        border.width: Theme.borderWidth

        GridLayout {
            id: layout
            anchors.centerIn: parent
            columns: picker.wide ? 2 : 1
            columnSpacing: 16
            rowSpacing: 16

            ColumnLayout {
                Layout.preferredWidth: Config.pickerWidth
                Layout.alignment: Qt.AlignTop
                spacing: 8

                // Active mode
                Text {
                    Layout.fillWidth: true
                    horizontalAlignment: Text.AlignHCenter
                    text: SmartPickerManager.moveMode
                          ? "Move \"" + (SmartPickerManager.openedTitle || "no focused window") + "\" to…"
                          : ({ w: "Workspaces", p: "Windows", a: "Applications", ">": "Run command" })[SmartPickerManager.parsed.letter] || "Smart"
                    color: SmartPickerManager.moveMode ? Theme.base0A : Theme.pickerText
                    font.pixelSize: Math.round(14 * Theme.textScale)
                    font.family: Theme.fontFamily
                    font.bold: true
                    elide: Text.ElideRight
                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: Config.pickerInputHeight
                    radius: Theme.radius
                    color: Theme.pickerInputBackground
                    border.color: Theme.pickerInputFocusBorder
                    border.width: 2

                    TextInput {
                        id: input
                        anchors.fill: parent
                        anchors.margins: 12
                        verticalAlignment: TextInput.AlignVCenter
                        color: Theme.pickerText
                        font.pixelSize: Math.round(Config.pickerFontSize * Theme.textScale)
                        font.family: Theme.fontFamily
                        clip: true
                        focus: true

                        text: SmartPickerManager.filterText
                        onTextChanged: SmartPickerManager.filterText = text

                        Text {
                            anchors.fill: parent
                            verticalAlignment: Text.AlignVCenter
                            visible: !input.text
                            text: "Search workspaces, windows, apps…"
                            color: Theme.pickerPlaceholder
                            font: input.font
                        }

                        Keys.onUpPressed: SmartPickerManager.selectedIndex = Math.max(0, SmartPickerManager.selectedIndex - 1)
                        Keys.onDownPressed: SmartPickerManager.selectedIndex =
                            Math.max(0, Math.min(SmartPickerManager.results.length - 1, SmartPickerManager.selectedIndex + 1))
                        Keys.onReturnPressed: function(event) { SmartPickerManager.activate(picker.item, event.modifiers) }
                        Keys.onEnterPressed: function(event) { SmartPickerManager.activate(picker.item, event.modifiers) }
                        Keys.onEscapePressed: SmartPickerManager.hide()
                        Keys.onTabPressed: function(event) {
                            if (picker.item && picker.item.kind === "win") SmartPickerManager.toggleSelection(picker.item.address)
                            event.accepted = true
                        }
                        Keys.onPressed: function(event) {
                            if ((event.key === Qt.Key_Backspace || event.key === Qt.Key_Delete)
                                && (event.modifiers & Qt.AltModifier)) {
                                SmartPickerManager.closeItem(picker.item)
                                event.accepted = true
                            }
                        }
                    }
                }

                ListView {
                    id: list
                    Layout.fillWidth: true
                    Layout.preferredHeight: Config.pickerSmartVisibleRows * Config.pickerItemHeight
                    clip: true
                    boundsBehavior: Flickable.StopAtBounds
                    model: SmartPickerManager.results
                    currentIndex: SmartPickerManager.selectedIndex
                    onCurrentIndexChanged: positionViewAtIndex(currentIndex, ListView.Contain)

                    delegate: Rectangle {
                        id: row
                        required property var modelData
                        required property int index
                        readonly property bool picked: modelData.kind === "win" && SmartPickerManager.isSelected(modelData.address)
                        readonly property bool current: index === SmartPickerManager.selectedIndex

                        width: list.width
                        height: Config.pickerItemHeight
                        radius: Theme.radius
                        color: picked ? Theme.base0B
                             : current ? Theme.pickerItemSelected
                             : mouse.containsMouse ? Theme.pickerItemHover : "transparent"

                        RowLayout {
                            anchors.fill: parent
                            anchors.leftMargin: 12
                            anchors.rightMargin: 12
                            spacing: 12

                            Rectangle {
                                Layout.preferredWidth: 4
                                Layout.preferredHeight: 24
                                radius: Theme.radius
                                color: row.picked ? Theme.base00 : Theme.base08
                                visible: row.current || row.picked
                            }

                            Image {
                                Layout.preferredWidth: 24
                                Layout.preferredHeight: 24
                                source: Quickshell.iconPath(row.modelData.icon)
                                sourceSize: Qt.size(24, 24)
                                fillMode: Image.PreserveAspectFit
                                smooth: true
                                mipmap: true
                            }

                            ColumnLayout {
                                Layout.fillWidth: true
                                spacing: 2
                                Text {
                                    Layout.fillWidth: true
                                    text: row.modelData.title
                                    color: row.picked ? Theme.base00 : Theme.pickerText
                                    font.pixelSize: Math.round(Config.pickerFontSize * Theme.textScale)
                                    font.family: Theme.fontFamily
                                    font.bold: row.modelData.kind === "ws"
                                    elide: Text.ElideRight
                                }
                                Text {
                                    Layout.fillWidth: true
                                    text: row.modelData.subtitle
                                    color: row.picked ? Theme.base01 : Theme.pickerSecondaryText
                                    font.pixelSize: Math.round((Config.pickerFontSize - 4) * Theme.textScale)
                                    font.family: Theme.fontFamily
                                    elide: Text.ElideRight
                                }
                            }

                            // Kind tag: ws · win · app · cmd · new
                            Text {
                                text: row.modelData.kind
                                color: row.picked ? Theme.base01 : Theme.pickerHintText
                                font.pixelSize: Math.round(13 * Theme.textScale)
                                font.family: Theme.fontFamily
                            }
                        }

                        MouseArea {
                            id: mouse
                            anchors.fill: parent
                            hoverEnabled: true
                            onClicked: function(m) {
                                SmartPickerManager.selectedIndex = row.index
                                SmartPickerManager.activate(row.modelData, m.modifiers)
                            }
                        }
                    }

                    Text {
                        anchors.centerIn: parent
                        visible: list.count === 0
                        text: SmartPickerManager.parsed.letter === ">" ? "Type a command" : "Nothing open"
                        color: Theme.pickerSecondaryText
                        font.pixelSize: Math.round(Config.pickerFontSize * Theme.textScale)
                        font.family: Theme.fontFamily
                    }
                }

                Text {
                    Layout.fillWidth: true
                    text: picker.hints
                    color: Theme.pickerSecondaryText
                    font.pixelSize: Math.round(13 * Theme.textScale)
                    font.family: Theme.fontFamily
                    elide: Text.ElideRight
                }
            }

            // Live mini-map of the highlighted row's workspace (empty for apps and commands;
            // the box keeps its size so nothing shifts)
            Rectangle {
                id: previewPane
                visible: Config.pickerWorkspacePreview
                Layout.alignment: Qt.AlignTop

                readonly property string wsName: !picker.item ? ""
                    : picker.item.kind === "ws" ? picker.item.name
                    : picker.item.kind === "win" ? picker.item.workspace : ""
                readonly property string selectedAddress: picker.item && picker.item.kind === "win" ? picker.item.address : ""
                readonly property var wins: wsName ? SmartPickerManager.windowsOn(wsName) : []
                readonly property var mon: wsName ? SmartPickerManager.monitorFor(wsName) : null
                // The real monitor, or the bounding box of the windows
                readonly property var frame: {
                    if (mon && mon.width > 0 && mon.height > 0) return mon
                    if (wins.length === 0) return null
                    var minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity
                    for (var i = 0; i < wins.length; i++) {
                        var w = wins[i]
                        minX = Math.min(minX, w.x); minY = Math.min(minY, w.y)
                        maxX = Math.max(maxX, w.x + w.w); maxY = Math.max(maxY, w.y + w.h)
                    }
                    return { x: minX, y: minY, width: Math.max(1, maxX - minX), height: Math.max(1, maxY - minY) }
                }
                readonly property bool hasPreview: wins.length > 0 && frame !== null

                readonly property int pad: 14
                // Fixed size from the picker screen's aspect ratio
                readonly property real aspect: picker.screen && picker.screen.width > 0
                    ? picker.screen.height / picker.screen.width : 9 / 16
                readonly property real mapW: Math.min(Config.pickerPreviewWidth - pad * 2, Config.pickerPreviewHeight / aspect)
                readonly property real mapH: mapW * aspect

                implicitWidth: mapW + pad * 2
                implicitHeight: pad + header.height + 8 + mapH + pad
                radius: Theme.radius
                color: "transparent"

                Column {
                    anchors.fill: parent
                    anchors.margins: previewPane.pad
                    spacing: 8

                    Row {
                        id: header
                        width: parent.width
                        spacing: 8
                        Text {
                            text: previewPane.wsName
                            color: Theme.pickerText
                            font.pixelSize: Math.round(15 * Theme.textScale)
                            font.family: Theme.fontFamily
                            font.bold: true
                            elide: Text.ElideRight
                            width: parent.width - countLabel.width - parent.spacing
                        }
                        Text {
                            id: countLabel
                            visible: previewPane.hasPreview
                            text: previewPane.wins.length + (previewPane.wins.length === 1 ? " window" : " windows")
                            color: Theme.pickerSecondaryText
                            font.pixelSize: Math.round(13 * Theme.textScale)
                            font.family: Theme.fontFamily
                            anchors.verticalCenter: parent.verticalCenter
                        }
                    }

                    Rectangle {
                        width: previewPane.mapW
                        height: previewPane.mapH
                        radius: Theme.radius
                        color: Theme.pickerListBackground
                        clip: true

                        Repeater {
                            model: previewPane.hasPreview ? previewPane.wins : []

                            delegate: Rectangle {
                                id: tile
                                required property var modelData
                                readonly property var f: previewPane.frame
                                readonly property bool isSelected: modelData.address === previewPane.selectedAddress

                                x: (modelData.x - f.x) / f.width * previewPane.mapW
                                y: (modelData.y - f.y) / f.height * previewPane.mapH
                                width: Math.max(6, modelData.w / f.width * previewPane.mapW)
                                height: Math.max(6, modelData.h / f.height * previewPane.mapH)
                                z: isSelected ? 1 : 0
                                radius: Theme.radius
                                color: Theme.pickerItemHover
                                border.color: isSelected ? Theme.pickerInputFocusBorder : Theme.outline
                                border.width: isSelected ? 3 : Theme.borderWidthInner
                                clip: true

                                ScreencopyView {
                                    id: cap
                                    anchors.fill: parent
                                    anchors.margins: tile.border.width
                                    captureSource: Config.pickerWorkspacePreviewLive ? tile.modelData.wayland : null
                                    live: true
                                    paintCursor: false
                                    constraintSize: Qt.size(Math.max(1, tile.width), Math.max(1, tile.height))
                                    visible: captureSource !== null && hasContent
                                }

                                // App icon when there is no live capture
                                Image {
                                    anchors.centerIn: parent
                                    width: Math.min(28, parent.width * 0.6)
                                    height: width
                                    source: Quickshell.iconPath(tile.modelData.icon)
                                    sourceSize: Qt.size(32, 32)
                                    fillMode: Image.PreserveAspectFit
                                    visible: !cap.visible
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
```

- [ ] **Step 3: verificare il caricamento**

Run: `sleep 2; qs log -p ~/.config/hypr/widgets/workspaces | tail -20`
Expected: `Configuration Loaded`, nessun errore che citi `SmartPicker.qml`, `SmartPickerManager.qml` o `rank.js`.

- [ ] **Step 4: aprirla una volta (solo con l'ok di Stefano)**

Run: `hyprctl dispatch 'hl.dsp.global("quickshell:smart")'`, poi `grim -o "$(hyprctl monitors -j | jq -r '.[]|select(.focused).name')" /tmp/claude-1000/smart.png` e guardare lo screenshot; chiudere con lo stesso `hyprctl dispatch`.
Expected: lista a sinistra con finestre recenti e workspace, anteprima a destra; `qs log` senza errori nuovi.

---

### Task 4: togliere i vecchi picker e spostare le scorciatoie

**Files:**
- Delete: `widgets/workspaces/WorkspacePicker.qml`, `WindowPicker.qml`, `WindowManager.qml`, `MoveWindowPicker.qml`, `MoveWindowManager.qml`
- Modify: `widgets/workspaces/shell.qml`, `widgets/workspaces/Config.qml`, `modules/menu.lua:53,67-70`, `modules/rules.lua:138`, `widgets/whichkey/Config.qml:22`

- [ ] **Step 1: `shell.qml`** — riscrivere così:

```qml
import Quickshell
import Quickshell.Hyprland
import QtQuick

ShellRoot {

    // Smart picker (Super+S): workspaces, windows, apps, commands
    GlobalShortcut {
        name: "smart"
        description: "Toggle the smart picker"
        onPressed: SmartPickerManager.toggle()
    }

    // Exposé overview
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
            SmartPickerManager.hide()
            ExposeManager.hide()
        }
    }

    SmartPicker {}
    Expose {}
}
```

- [ ] **Step 2: cancellare i file**

Run: `cd ~/.config/hypr/widgets/workspaces && rm WorkspacePicker.qml WindowPicker.qml WindowManager.qml MoveWindowPicker.qml MoveWindowManager.qml && grep -rn 'WindowManager\|MoveWindowManager\|WorkspacePicker\|WindowPicker\|MoveWindowPicker' --include=*.qml .`
Expected: nessun output.

- [ ] **Step 3: `Config.qml`, voci rimaste senza uso**

Run: `cd ~/.config/hypr/widgets/workspaces && for p in pickerPreviewOnNavigate pickerPreviewMinChars pickerMaxHeight startupWorkspaces; do echo "$p: $(grep -l "$p" *.qml | grep -v Config.qml | tr '\n' ' ')"; done`
Per ogni voce senza file dopo i due punti, togliere da `Config.qml` la riga `readonly property …` e i commenti sopra che la descrivono.

- [ ] **Step 4: `modules/menu.lua`** — togliere

```lua
        { "Esegui comando",       "pkill tofi || tofi-run | zsh",                     S .. "S" },
```

e sostituire

```lua
        { "Scegli workspace",      qs("workspaces"),           S .. "P" },
        { "Scegli finestra",       qs("windows"),              S .. "W" },
        { "Ricerca smart",         qs("smart"),                S .. "R" },
        { "Sposta finestra",       qs("movewindow"),           S .. "M" },
```

con

```lua
        { "Cerca e apri",          qs("smart"),                S .. "S" },
```

- [ ] **Step 5: `modules/rules.lua:138`**

```lua
    match = { namespace = "^(smart-picker|expose)$" },
```

- [ ] **Step 6: `widgets/whichkey/Config.qml:22`**

```qml
        "smart-picker",
```

(al posto di `"workspaces-picker", "window-picker", "smart-picker", "move-window-picker",`).

- [ ] **Step 7: verificare**

Run: `export HYPRLAND_INSTANCE_SIGNATURE=$(ls -t /run/user/1000/hypr/ | head -1); hyprctl reload >/dev/null; sleep 2; hyprctl configerrors; hyprctl binds -j | jq -r '.[] | select(.modmask==64 and (.key=="S" or .key=="W" or .key=="P" or .key=="R" or .key=="M")) | "\(.key) \(.arg)"' | sort -u; qs log -p ~/.config/hypr/widgets/workspaces | tail -5; jq -c '.. | objects | select(.label? == "Cerca e apri")' $XDG_RUNTIME_DIR/whichkey.json`
Expected: `configerrors` vuoto; tra i bind con Super solo `S`, nessun W/P/R/M; `Configuration Loaded`; la voce `Cerca e apri` con `"key":"S"`.

---

### Task 5: documentazione

**Files:**
- Rewrite: `widgets/workspaces/AGENTS.md`
- Modify: `widgets/workspaces/PROGRESS.md`, `widgets/workspaces/SPEC-smart.md` (riga Alt+Backspace), `~/.config/hypr/CLAUDE.md` (tabella widget, punto aperto sul picker dei workspace)

- [ ] **Step 1: `AGENTS.md`** — riscrivere così:

````markdown
# AGENTS.md

**Workspaces widget** — Quickshell widget for Hyprland: the Smart picker and Exposé.

## Files

```
shell.qml               shortcuts (smart, expose, pickers-close) + windows
Config.qml / Theme.qml  settings / colors (Theme.qml → ../shared/Theme.qml)
rank.js                 pure ranking of the Smart list (.pragma library)
rank_test.js            node rank_test.js — asserts on rank.js
SmartPickerManager.qml  Smart data (Quickshell.Hyprland, DesktopEntries) and actions
SmartPicker.qml         Smart UI: one list + live workspace preview
WorkspaceManager.qml    workspace/window data used by Exposé
ExposeManager.qml / Expose.qml   Exposé overview
SPEC-smart.md           what the Smart picker does
```

Run: `qs -p .` (started by `modules/autostart.lua`). Shortcuts: Super+S = `quickshell:smart`,
Super+E = `quickshell:expose` (both in `modules/menu.lua`).

## Smart picker

One list ranked by match (exact > prefix > contains > fuzzy; ties: workspace > window > app);
Enter activates the highlighted row, the first one after typing. Behaviour and keys: `SPEC-smart.md`.

- Empty search: windows by last use (`focusHistoryID`), then project workspaces; the current ones excluded.
- Prefixes: `w ` workspaces · `p ` windows · `a ` apps · `m ` move the focused window · `>` shell command.
- Apps with an open window are hidden: typing an app name finds its window if open, else launches it.

## Architecture

- No processes on open: `Hyprland.toplevels/workspaces/monitors` (event driven) plus
  `refreshToplevels()` in `show()` for `lastIpcObject` (class, geometry, focus order).
- Actions: `Hyprland.dispatch` with the Lua dispatchers; several at once are sent as one
  `function() hl.dispatch(…); … end`.
- Preview tiles capture `HyprlandToplevel.wayland` with `ScreencopyView`
  (needs `ToplevelManager` warmed up, done in `SmartPicker.qml`).
- After changing `rank.js`, run `node rank_test.js`.

## Quirks (Quickshell 0.3.0)

- Use `WlrLayershell.xxx` attached properties and `exclusionMode: ExclusionMode.Ignore`.
- `pragma Singleton` QML files are auto-registered by name.
- Edit files in place: replacing them (`sed -i`) breaks the live reload.
````

- [ ] **Step 2: `PROGRESS.md`** — sostituire la sezione `## The three pickers` (tabella inclusa) con:

```markdown
## The picker

One Smart picker (Super+S) replaced the workspace, window and move-window pickers and
tofi-run on 2026-10-23: one ranked list + live preview beside it. See `SPEC-smart.md`.
```

e nella riga `Keybinds in modules/keybindings.lua: …` sostituire il testo dei tasti con
`` `SUPER+S` smart · `SUPER+E` exposé (in `modules/menu.lua`). ``

- [ ] **Step 3: `SPEC-smart.md`** — nella tabella Tasti sostituire la riga Alt+Backspace con

```markdown
| Alt+Backspace | chiude (o le selezionate) | chiude tutte le sue finestre | — | — |
```

- [ ] **Step 4: `~/.config/hypr/CLAUDE.md`**

Riga della tabella widget:

```markdown
| `workspaces` | Smart picker (Super+S), exposé, `pickers-close` | ordinamento in `rank.js`: `node rank_test.js` |
```

e togliere da "Aperto" il punto `- Picker dei workspace mostra "No workspaces" …` con la riga che segue (il picker non c'è più).

- [ ] **Step 5: verifica finale**

Run: `node ~/.config/hypr/widgets/workspaces/rank_test.js && qs log -p ~/.config/hypr/widgets/workspaces | tail -5 && cd ~/.dotfiles && git status --short`
Expected: `rank ok`, `Configuration Loaded`, solo i file di questo piano (più quelli delle scorciatoie di oggi) tra i modificati.
Poi chiedere a Stefano di provare i tasti e se fare il commit.
