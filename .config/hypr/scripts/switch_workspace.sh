#!/usr/bin/env bash
ewww close-popup
hyprctl --batch dispatch workspace $1
hyprctl --batch dispatch animatefocused
eww open-many ws-popup-1 ws-popup-0 ws-popup-2
sleep $2
eww close ws-popup-1 ws-popup-0 ws-popup-2
