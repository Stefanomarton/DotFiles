#!/usr/bin/env bash
# Super+A: uno yazi scratchpad per workspace, anche per quelli creati al volo.
# Alla prima apertura scrive la voce pypr (eredita da "yazi") in
# ~/.config/pypr/yazi-workspaces/ e ricarica pypr.
# ponytail: le voci di workspace spariti restano lì, innocue; cancellale a mano se vuoi

ws=$(hyprctl activeworkspace -j | jq -r .name)
id="yazi-${ws//[^A-Za-z0-9_-]/_}"
conf="$HOME/.config/pypr/yazi-workspaces/$id.toml"

if [[ ! -f $conf ]]; then
    printf '[scratchpads.%s]\nuse = "yazi"\ncommand = "kitty --class kitty-%s yazi"\nclass = "kitty-%s"\n' \
        "$id" "$id" "$id" > "$conf"
    pypr reload
fi
pypr toggle "$id"
