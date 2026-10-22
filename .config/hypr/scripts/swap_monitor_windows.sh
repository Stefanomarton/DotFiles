#!/usr/bin/env bash
# Swap the windows between the workspaces active on the G9 and the Xiaomi
# (by description: through the laptop dock they get other DP-x names)
# (the workspaces stay on their monitor, only the windows move)
set -euo pipefail

# match by name: named workspaces have no "id" in hyprctl -j
IFS=$'\t' read -r a b < <(hyprctl -j monitors | jq -r '
    [ (.[] | select(.description|startswith("Samsung Electric Company Odyssey G91F")).activeWorkspace.name),
      (.[] | select(.description|startswith("Xiaomi Corporation Mi Monitor")).activeWorkspace.name) ] | @tsv')
[ -n "$a" ] && [ -n "$b" ] || exit 0   # fuori dal dock c'è un monitor solo

# addresses are collected before moving anything, so no temp workspace is needed
cmds=$(hyprctl -j clients | jq -r --arg a "$a" --arg b "$b" '
    .[] | select(.workspace.name == $a or .workspace.name == $b)
        | "dispatch hl.dsp.window.move({ workspace = \(if .workspace.name == $a then $b else $a end | if test("^[0-9]+$") then . else "\"name:\(.)\"" end), follow = false, window = \"address:\(.address)\" })"' \
    | paste -sd';')

[ -n "$cmds" ] && hyprctl --batch "$cmds"
