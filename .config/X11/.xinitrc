case $HOSTNAME in

laptop)
# xrdb -merge ~/.config/X11/.Xresources &
feh --no-fehbg --bg-fill /home/stefanom/.local/share/Wallpapers/NordicWallpapers/ign_evening.png
picom &
xmodmap ~/.config/X11/.XmodmapDesktop &
flameshot &
redshift -l 45.69651:9.1818 &
insync start &
xbanish -s -t 2 &
xinput --set-prop 10 318 1 &
xinput --set-prop 10 345 1 &
# dbus-update-activation-environment --all &
# gnome-keyring-daemon --start --components=secrets &
# gnome-keyring-daemon --login &
kmonad ~/.config/kmonad/laptop.kbd &
sleep 10 &
xset r rate 150 50 
~/.config/X11/laptop/statusbar.sh &
exec dwm 
;;

desktop)
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.config/X11/.Xresources
xrandr --output DP-1 --primary --mode 3440x1440 -r 144 --pos 1386x1144 --rotate normal --output DP-2 --off --output HDMI-1 --mode 1920x1080 --pos 2146x64 --rotate inverted --output HDMI-2 --mode 1920x1080 --pos 306x904 --rotate left --output DVI-D-1 --off
# feh --no-fehbg --bg-fill /home/stefanom/.local/share/Wallpapers/NordicWallpapers/ign_evening.png
gammastep &
# picom & 
# sleep 10 &
flameshot &
# redshift -l 45.69651:9.1818 &
insync start &
syncthing &
xbanish -s -t 2 &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
dbus-update-activation-environment XDG_CURRENT_DESKTOP &
dbus-update-activation-environment --all &
# xset r rate 150 50 &
# xmodpap ~/.config/X11/.XmodmapDesktop
# xsetxkbmap -layout us -variant intl
# xinput set-prop "Hazel Bad Wings Mouse" 296 -0.5
exec qtile start -c ~/.config/qtileDesktop/config.py
# exec awesome
;;

esac
