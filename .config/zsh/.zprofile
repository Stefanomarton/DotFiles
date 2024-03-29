export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
HOSTNAME=$(hostnamectl hostname)


if [ "$(tty)" = "/dev/tty1" ]; then
    case "$HOSTNAME" in
    "desktop")
        # Start awesome if not running
        pgrep qtile || startx "$XDG_CONFIG_HOME/X11/.xinitrc"
        # pgrep qtile || qtile start -b wayland -c ~/.config/qtileDesktop/config.py
        ;;
    "laptop")
        pgrep qtile || qtile start -b wayland ;;
    esac
fi
