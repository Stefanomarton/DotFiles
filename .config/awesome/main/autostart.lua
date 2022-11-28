-- Standard awesome library
local awful = require("awful")

awful.spawn.with_shell("xset r rate 300 60")
awful.spawn.with_shell("nm-applet")
awful.spawn.once("insync")
-- awful.spawn.with_shell("python /opt/thefanclub/overgrive/overgrive")
-- awful.spawn.once("picom")
awful.spawn.once("discord")
awful.spawn.with_shell("xmodmap ~/.config/X11/.XmodmapDesktop")
awful.spawn.with_shell("feh --bg-fill --no-fehbg -z ~/DotFiles/Media/ThemeThings/NordicWallpapers/street_blues.png")
awful.spawn.once("ferdium")
awful.spawn.once("flameshot")
