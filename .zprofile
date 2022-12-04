export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
case "$(tty)" in

"/dev/tty1")
pgrep awesome || startx "$XDG_CONFIG_HOME/X11/.xinitrc" ;;

"/dev/tty2")
pgrep Hyprland || Hyprland 
sudo evremap remap .config/evremap/evremap.toml > /dev/null &&
;;

esac
