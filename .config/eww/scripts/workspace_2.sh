#!/usr/bin/env bash

#define icons for workspaces 1-9
ic=(0 1 2 3 4 5 6 7 8 9)

workspaces() {

	unset -v \
  o1 o2 o3 o4 o5 o6 o7 o8 o9 \
  f1 f2 f3 f4 f5 f6 f7 f8 f9

# Get occupied workspaces and remove workspace -99 aka scratchpad if it exists
# a="$(hyprctl workspaces | grep ID | awk '{print $3}')"
# a="$(echo "${a//-99/}" | sed '/^[[:space:]]*$/d')"
ows="$(hyprctl workspaces -j | jq '.[] | del(select(.id == -99)) | .id')"

for num in $ows; do
	export o"$num"="$num"
done

# Get Focused workspace for current monitor ID
arg="$1"
num="$(hyprctl monitors -j | jq --argjson arg "$arg" '.[] | select(.id == $arg).activeWorkspace.id')"
export f"$num"="$num"

echo 	"(eventbox :onscroll \"echo {} | sed -e 's/up/-1/g' -e 's/down/+1/g' | xargs hyprctl dispatch workspace\" \
          (box	:class \"workspace\" :spacing 15 :orientation \"h\" :space-evenly \"false\" 	\
              (button :class \"w0$o8$f8\" \"${ic[8]}\") \
              (button :class \"w0$o9$f9\" \"${ic[9]}\") \
          )\
        )"
}

workspaces $1 
socat -u UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock - | while read -r line; do
workspaces $1
done
