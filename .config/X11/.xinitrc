xrdb -merge ~/.config/X11/.Xresources &
xrandr --output DisplayPort-0 --off --output DisplayPort-1 -r 144 --primary --mode 3440x1440 --pos 1920x291 --rotate normal --output HDMI-A-0 --mode 1920x1080 --pos 0x340 --rotate normal --output HDMI-A-1 --off --output DVI-D-0 --mode 1920x1080 --pos 5360x0 --rotate right
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
exec awesome --config .config/awesome/rc.lua
