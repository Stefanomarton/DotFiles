xrdb -merge ~/.config/X11/.Xresources &
xrandr --output DisplayPort-0 --primary --mode 3440x1440 --pos 1080x0 --rotate normal -r 144 --output DisplayPort-1 --off --output HDMI-A-0 --mode 1920x1080 --pos 0x0 --rotate left --output HDMI-A-1 --off --output DVI-D-0 --mode 1920x1080 --pos 4520x0 --rotate right
    sleep 10 &
		feh --no-fehbg --bg-fill /home/stefanomarton/Media/ThemeThings/NordicWallpapers/super-mario.png 
		picom &
    xmodmap ~/.config/X11/.XmodmapDesktop & 
		xset r rate 150 50 &
		ferdium &
		flameshot &
		redshift -l 45.69651:9.1818 &
		insync start &
		while true; do 
				xsetroot -name "$(date +"%A %B %d, %H:%M")"
				sleep 1s
		done &
    exec dwm

