#!/usr/bin/env bash
# ws-check.sh  <ws-number>
# Prints one ● for every normal window on the given workspace,
# forever, one line per update → perfect for eww deflisten.
#
#   e.g.  ./ws-check.sh 3
#         ●●                (3 windows)
#         ●                 (1 window, after you closed two)
#         <empty line>      (no windows left)
#
# Requires: jq, hyprctl 0.40+.

ws="$1" # workspace number passed in from eww
circle='.'

print_circles() {
    win_count=$(hyprctl clients -j |
        jq --argjson ws "$ws" \
            '[.[] | select(.workspace.id == $ws)] | length')

    if ((win_count)); then
        printf '%*s\n' "$win_count" '' | tr ' ' "$circle"
    else
        echo # print empty line for zero windows
    fi
}

# initial value
print_circles

# update 4 times a second; tweak if you want
while sleep 0.25; do
    print_circles
done
