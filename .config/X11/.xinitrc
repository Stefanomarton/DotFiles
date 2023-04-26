case $HOSTNAME in
laptop)
# xrdb -merge ~/.config/X11/.Xresources &
feh --no-fehbg --bg-fill /home/stefanom/.local/share/Wallpapers/NordicWallpapers/earth-in-space.png 
xmodmap ~/.config/X11/.XmodmapDesktop &
flameshot &
redshift -l 45.69651:9.1818 &
insync start &
xbanish -s -t 2 &
xinput --set-prop 11 337 1
xinput --set-prop 11 316 1
# dbus-update-activation-environment --all &
# gnome-keyring-daemon --start --components=secrets &
# gnome-keyring-daemon --login &
kmonad ~/.config/kmonad/laptop.kbd &
xset r rate 150 50 &
exec dwm ;;
desktop)
xrdb -merge ~/.config/X11/.Xresources &
xrandr --output DP-1 --primary -r 144 --mode 3440x1440 --pos 1920x0 --rotate normal --output DP-2 --off --output HDMI-1 --mode 1920x1080 --pos 0x114 --rotate normal --output HDMI-2 --off --output DVI-D-1 --off
sleep 10 &
#feh --no-fehbg --bg-fill /home/stefanomarton/Media/ThemeThings/NordicWallpapers/super-mario.png
picom &
xmodmap ~/.config/X11/.XmodmapDesktop &
xset r rate 150 50 &
ferdium &
flameshot &
redshift -l 45.69651:9.1818 &
insync start &
xbanish -s -t 2 &
dbus-update-activation-environment --all &
gnome-keyring-daemon --start --components=secrets &
gnome-keyring-daemon --login &
exec awesome --config .config/awesome/rc.lua ;;
esac
