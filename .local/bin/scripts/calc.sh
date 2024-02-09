#!/usr/bin/env bash
DMENU_OPTIONS="-l 5"

menu="dmenu $DMENU_OPTIONS"

# Grab the answer
if [ -n "$1" ]; then
    answer=$(echo "$1" | qalc +u8 -color=never -terse | awk '!/^>/ && !/^$/ {gsub(/^[ \t]+|[ \t]+$/, "", $0); print}')
fi

# Determine args to pass to dmenu/rofi
while [[ $# -gt 0 && $1 != "--" ]]; do
    shift
done
[[ $1 == "--" ]] && shift

action=$(echo -e "Copy to clipboard\nClear\nClose" | $menu "$@" -p "$answer")

case $action in
    "Clear") $0 ;;
    "Copy to clipboard") echo -n "$answer" | xclip -selection clipboard ;;
    "Close") ;;
    "") ;;
    *) $0 "$answer $action" "--dmenu=$menu" "--" "$@" ;;
esac
