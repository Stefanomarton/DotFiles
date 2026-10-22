#!/usr/bin/env bash
# Swap the windows between the workspaces active on the G9 and the Xiaomi
# (by description: through the laptop dock they get other DP-x names); without them,
# between the two screens when there are exactly two (laptop + office monitor)
# (the workspaces stay on their monitor, only the windows move)
set -euo pipefail

# match by name: named workspaces have no "id" in hyprctl -j
IFS=$'\t' read -r a b < <(hyprctl -j monitors | jq -r '
    def ws(d): [.[] | select(.description | startswith(d)) | .activeWorkspace.name][0];
    ws("Samsung Electric Company Odyssey G91F") as $g9 | ws("Xiaomi Corporation Mi Monitor") as $mi
    | if $g9 and $mi then [$g9, $mi]
      elif length == 2 then [.[0].activeWorkspace.name, .[1].activeWorkspace.name]
      else [] end | @tsv')
[ -n "$a" ] && [ -n "$b" ] || exit 0   # un monitor solo: niente da scambiare

# addresses are collected before moving anything, so no temp workspace is needed
cmds=$(hyprctl -j clients | jq -r --arg a "$a" --arg b "$b" '
    .[] | select(.workspace.name == $a or .workspace.name == $b)
        | "dispatch hl.dsp.window.move({ workspace = \(if .workspace.name == $a then $b else $a end | if test("^[0-9]+$") then . else "\"name:\(.)\"" end), follow = false, window = \"address:\(.address)\" })"' \
    | paste -sd';')

[ -n "$cmds" ] && hyprctl --batch "$cmds"
