#!/bin/sh
wlr-randr --output HDMI-A-1 --transform 90 --pos 0,0 --output DP-1 --mode 3440x1440@144 --preferred --adaptive-sync enabled --pos 1080,0 &
wl-paste --watch cliphist store &
insync start &
gammastep & 
