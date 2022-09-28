

-- Standard awesome library
local awful = require("awful")

awful.spawn.with_shell("xset r rate 300 60")
awful.spawn.with_shell("nm-applet")
awful.spawn.with_shell("python /opt/thefanclub/overgrive/overgrive")
-- awful.spawn.with_shell("xrandr --output DVI-D-0 --mode 1920x1080 --pos 4520x0 --rotate right --output DP-0 --off --output DP-1 --off --output HDMI-0 --mode 1920x1080 -r 75 --pos 0x0 --rotate left --output DP-2 --off --output DP-3 --off --output DP-4 --primary --mode 3440x1440 -r 144 --pos 1080x0 --rotate normal --output DP-5 --off")
awful.spawn.once("picom")
awful.spawn.once("discord")
awful.spawn.with_shell("xmodmap .Xmodmap")
awful.spawn.with_shell("feh --bg-fill --no-fehbg ~/.config/awesomevecchio/arch.jpeg")

awful.spawn.with_shell("python ~/.config/awesome-laptop/utils/autohidewibox.py")
