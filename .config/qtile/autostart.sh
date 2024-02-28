#!/bin/sh
wlr-randr --output DP-1 --mode 1920x1080 --pos 0,0 --output eDP-1 --mode 1920x1200 --pos 0,1080 
emacs --daemon 
kmonad ~/.config/kmonad/laptop.kbd &
insync start &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
dbus-update-activation-environment XDG_CURRENT_DESKTOP &
dbus-update-activation-environment --all &
