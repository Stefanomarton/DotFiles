#!/bin/bash



# Check VM

## Define the name of the VM
vm_name="win11"

### Check if the VM is running
if ! virsh -c qemu:///system dominfo "$vm_name" | grep -q "State:\s*running"; then

    ### Start VM using virsh
    virsh -c qemu:///system start "$vm_name"

fi

## Check looking glass
if ! pgrep -f "looking-glass-client" > /dev/null; then

    ### Command to start Looking Glass
    hyprctl dispatch exec "[workspace name:w11 silent;noanim] looking-glass-client"

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



