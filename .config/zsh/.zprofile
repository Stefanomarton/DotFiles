export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
HOSTNAME=$(hostnamectl hostname)


if [ "$(tty)" = "/dev/tty1" ]; then
    case "$HOSTNAME" in
    "desktop")
        Hyprland
        ;;
    "laptop")
        Hyprland
    esac
fi
