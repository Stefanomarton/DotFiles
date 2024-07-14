#!/bin/bash

# Check VM

## Define the name of the VM
vm_name="work"

### Check if the VM is running
if ! virsh -c qemu:///system dominfo "$vm_name" | grep -q "State:\s*running"; then

	### Start VM using virsh
	virsh -c qemu:///system start "$vm_name"

	killall looking-glass-client

	sleep 10
	hyprctl dispatch exec "[workspace name:w11 silent; noanim] looking-glass-client"

else

	### Command to start Looking Glass
	killall looking-glass-client
	hyprctl dispatch exec "[workspace name:w11 silent; noanim] looking-glass-client"

fi

# Check Emacs server

## Emacs server is running helper
function is_emacs_server_running() {
	emacsclient -e "(message \"\")" >/dev/null 2>&1
}

if ! is_emacs_server_running; then
	## Start emacs server if it's not already running
	emacs --daemon
fi

# Kmonad

## Start kmonad if hostname is laptop
if [ "$(hostnamectl hostname)" = "laptop" ]; then
	sleep 1 && kmonad .config/kmonad/laptop.kbd
fi

# Eww
eww daemon
eww open bg-widget

# ollama
if [ "$(hostnamectl hostname)" = "desktop" ]; then
	ollama serve
	disown
fi

# Dropbox
# dropbox
