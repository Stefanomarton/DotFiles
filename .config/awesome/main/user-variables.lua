-- {{{ Global Variable Definitions
-- moved here in module as local variable
-- }}}

local home = os.getenv("HOME")

local _M = {
  -- This is used later as the default terminal and editor to run.
  -- terminal = "xterm",
  terminal = "kitty",
  editor = "nvim", 
  -- editor_cmd = terminal .. " -e " .. editor,
  modkey = "Mod4",
  Alt = "Mod1",
  browser = "firefox",
  filemanager = "nautilus",

  -- user defined wallpaper
  wallpaper = nil,
  --wallpaper = home .. "/Pictures/your-wallpaper-here.jpg",
}

return _M

