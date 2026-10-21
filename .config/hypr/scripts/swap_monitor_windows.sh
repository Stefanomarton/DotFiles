#!/usr/bin/env bash
# Swap the windows between the workspaces active on DP-1 and DP-2
# (the workspaces stay on their monitor, only the windows move)
set -euo pipefail

# match by name: named workspaces have no "id" in hyprctl -j
IFS=$'\t' read -r a b < <(hyprctl -j monitors | jq -r '
    [ (.[] | select(.name=="DP-1").activeWorkspace.name),
      (.[] | select(.name=="DP-2").activeWorkspace.name) ] | @tsv')

# addresses are collected before moving anything, so no temp workspace is needed
cmds=$(hyprctl -j clients | jq -r --arg a "$a" --arg b "$b" '
    .[] | select(.workspace.name == $a or .workspace.name == $b)
        | "dispatch hl.dsp.window.move({ workspace = \(if .workspace.name == $a then $b else $a end | if test("^[0-9]+$") then . else "\"name:\(.)\"" end), follow = false, window = \"address:\(.address)\" })"' \
    | paste -sd';')

[ -n "$cmds" ] && hyprctl --batch "$cmds"
