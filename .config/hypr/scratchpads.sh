#!/bin/bash
hyprctl keyword windowrule "workspace special:1 silent, Explorer" exit 1 &&
		if pgrep -x "ranger" > /dev/null 2>&1 ; then
			hyprctl dispatch togglespecialworkspace 1
			exit 1
		fi

		hyprctl dispatch exec "kitty --detach --class Explorer -e ranger" > /dev/null
		hyprctl dispatch togglespecialworkspace 1

