xrdb -merge ~/.config/X11/.Xresources &

xrandr --output DVI-D-0 --mode 1920x1080 --pos 4520x0 --rotate right --output DP-0 --off --output DP-1 --off --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate left --output DP-2 --off --output DP-3 --off --output DP-4 --primary --mode 3440x1440 --pos 1080x240 --rotate normal --rate 144 --output DP-5 --off

    sleep 10 &
		feh --no-fehbg --bg-fill /home/stefanomarton/Media/ThemeThings/NordicWallpapers/ign_unsplash35.png &
		picom &
    xmodmap ~/.config/X11/.XmodmapDesktop & 
		xset r rate 150 50 &
		ferdium &
		flameshot &
		insync start &
		while true; do 
				xsetroot -name "$(date +"%A %B %d, %H:%M")"
				sleep 1s
		done &
    exec dwm

