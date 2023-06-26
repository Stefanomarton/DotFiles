export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
case "$(tty)" in

"/dev/tty1")
pgrep Hyprland || Hyprland ;;

"/dev/tty2")
pgrep dwm || startx "$XDG_CONFIG_HOME/X11/.xinitrc" ;;

esac
