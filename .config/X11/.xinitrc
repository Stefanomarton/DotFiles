xrdb -merge ~/.config/X11/.Xresources &

case "$HOSTNAME" in 

desktop)

    xrandr --output DP-4 --primary --mode 3440x1440 -r 144 --pos 1080x0 --rotate normal --output DVI-D-0 --mode 1920x1080 --pos 4520x0 --rotate right --output DP-0 --off --output DP-1 --off --output HDMI-0 --mode 1920x1080 -r 75 --pos 0x0 --rotate left --output DP-2 --off --output DP-3 --off --output DP-5 --off &

    sleep 10 &
    xmodmap ~/.config/X11/.XmodmapDesktop & 
    exec awesome
    ;;

laptop)
    xmodmap ~/.config/X11/.XmodmapLaptop &
    exec awesome --config /home/stefanomarton/.config/awesome-laptop/rc.lua
  ;;

esac

