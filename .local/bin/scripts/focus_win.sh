#!/bin/bash

wmctrl -R $(wmctrl -l | sed "s/.*$HOSTNAME //" | dmenu -l 5 -p "Window") 
