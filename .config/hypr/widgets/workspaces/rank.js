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
        for (i = 0; i < data.windows.length; i++) if (data.windows[i].cls) open[data.windows[i].cls.toLowerCase()] = true
        for (i = 0; i < data.apps.length; i++) {
            var a = data.apps[i], id = a.id.toLowerCase()
            if ((a.wmClass && open[a.wmClass.toLowerCase()]) || open[id] || open[id.split(".").pop()]) continue
            // hay = description + keywords, lowercased once by the caller; substring only,
            // fuzzy there matched nearly every short query
            s = q ? Math.max(score(a.name, q), a.hay.includes(q) ? 100 : 0) : 1
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
