#!/bin/sh
emacs --daemon
virsh --connect qemu:///system start "win11"
