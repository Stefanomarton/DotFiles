export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
case "$(tty)" in

"/dev/tty1")
pgrep dwm || startx "$XDG_CONFIG_HOME/X11/.xinitrc" ;;

"/dev/tty2")
pgrep Hyprland || Hyprland
;;

esac
