#!/bin/sh
emacs --daemon
virsh --connect qemu:///system start "win11"
conky -c ~/.config/conky/conky.lua 
picom
