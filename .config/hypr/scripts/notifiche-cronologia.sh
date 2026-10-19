#!/usr/bin/env bash
# Cronologia dunst in tofi: la notifica scelta torna a schermo.
sel=$(dunstctl history | jq -r '.data[0][] | "\(.id.data)\t\(.appname.data): \(.summary.data) — \(.body.data | gsub("\n"; " ") | .[0:80])"' \
  | tofi --prompt-text "notifiche: ") || exit 0
[ -n "$sel" ] && dunstctl history-pop "${sel%%$'\t'*}"
