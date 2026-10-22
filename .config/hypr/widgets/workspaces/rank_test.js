// node rank_test.js — loads rank.js (a QML .pragma library) and checks the ranking.
const fs = require("fs"), vm = require("vm"), assert = require("assert")
const src = fs.readFileSync(__dirname + "/rank.js", "utf8").replace(/^\.pragma library/m, "")
const ctx = {}
vm.runInNewContext(src, ctx)
// copy results out of the vm realm, or deepStrictEqual sees foreign Array prototypes
const R = { build: (text, data) => JSON.parse(JSON.stringify(ctx.build(text, data))) }

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
        { name: "Firefox", generic: "Web Browser", hay: "web browser internet", id: "firefox", wmClass: "firefox", ref: "ff", icon: "firefox" },
        { name: "Firefox Developer Edition", generic: "Web Browser", hay: "web browser", id: "firefox-devedition", wmClass: "firefox-aurora", ref: "ffdev", icon: "x" },
        { name: "Notes", generic: "Note taking", hay: "note taking", id: "org.gnome.Notes", wmClass: "", ref: "notesapp", icon: "x" },
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
// description/keywords match as substring only: "browser" finds the closed browser, "wbr" (fuzzy) does not
assert.deepStrictEqual(kinds(R.build("a browser", data)), ["app:Firefox Developer Edition"])
assert.deepStrictEqual(kinds(R.build("a wbr", data)), [])
// a window with no class (not refreshed yet, some XWayland) must not hide apps without StartupWMClass
const noClass = Object.assign({}, data, { windows: data.windows.concat([{ address: "0x9", title: "?", cls: "", workspace: "web", focus: 5, icon: "x" }]) })
assert.deepStrictEqual(kinds(R.build("a notes", noClass)), ["app:Notes"])
console.log("rank ok")
