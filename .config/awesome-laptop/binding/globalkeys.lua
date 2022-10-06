-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local awesomebuttons = require("awesome-buttons.awesome-buttons")
require("main.scratchpad")


-- Custom awesome library 
local bling = require("bling")
local logout_popup = require("deco.logout-widget.logout-popup")

-- local hotkeys_popup = require("awful.hotkeys_popup").widget
local hotkeys_popup = require("awful.hotkeys_popup")
-- Menubar library
local menubar = require("menubar")

-- Resource Configuration
local modkey = RC.vars.modkey
local terminal = RC.vars.terminal
local browser = RC.vars.browser
local filemanager = RC.vars.filemanager

local _M = {}

-- reading
-- https://awesomewm.org/wiki/Global_Keybindings

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local globalkeys = gears.table.join(
    awful.key({ modkey,           }, "i",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Tag browsing
    -- awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
    --           {description = "view previous", group = "tag"}),
    -- awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
    --           {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),


    awful.key({ modkey }, "Left", function()
	        awful.client.focus.bydirection("left")
	        if client.focus then client.focus:raise() end
                                                    end),
    awful.key({ modkey }, "Down", function()
	        awful.client.focus.bydirection("down")
	        if client.focus then client.focus:raise() end
                                                    end),
    awful.key({ modkey }, "Up", function()
	        awful.client.focus.bydirection("up")
	        if client.focus then client.focus:raise() end
                                                    end),
    awful.key({ modkey }, "Right", function()
	        awful.client.focus.bydirection("right")
	        if client.focus then client.focus:raise() end 
                                                    end),
    
    
    awful.key({ modkey,           }, "w", function () RC.mainmenu:show() end,
              {description = "show main menu", group = "awesome"}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", function() logout_popup.launch() end,
              {description = "Logout_Menu", group = "awesome"}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Layout manipulation
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    -- awful.key({ modkey,           }, "v", function () awful.layout.inc( 1)                end,
    --           {description = "select next", group = "layout"}),
    awful.key({ modkey,    }, "v", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              {description = "restore minimized", group = "client"}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- -- Resize
    -- --awful.key({ modkey, "Control" }, "Left",  function () awful.client.moveresize( 20,  20, -40, -40) end),
    -- --awful.key({ modkey, "Control" }, "Right", function () awful.client.moveresize(-20, -20,  40,  40) end),
    -- awful.key({ modkey, "Control" }, "Down",  
    --           function () awful.client.moveresize( 0, 0, 0, -20) end),
    -- awful.key({ modkey, "Control" }, "Up",    
    --           function () awful.client.moveresize( 0, 0, 0,  20) end),
    -- awful.key({ modkey, "Control" }, "Left",  
    --           function () awful.client.moveresize( 0, 0, -20, 0) end),
    -- awful.key({ modkey, "Control" }, "Right", 
    --           function () awful.client.moveresize( 0, 0,  20, 0) end),
    --
    -- -- Move
    -- awful.key({ modkey, "Shift"   }, "Down",  
    --           function () awful.client.moveresize(  0,  20,   0,   0) end),
    -- awful.key({ modkey, "Shift"   }, "Up",    
    --           function () awful.client.moveresize(  0, -20,   0,   0) end),
    -- awful.key({ modkey, "Shift"   }, "Left",  
    --           function () awful.client.moveresize(-20,   0,   0,   0) end),
    -- awful.key({ modkey, "Shift"   }, "Right", 
    --           function () awful.client.moveresize( 20,   0,   0,   0) end),
    --
    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Menubar
    awful.key({ modkey }, "s", function() awful.util.spawn("rofi -show drun -theme ~/.config/rofi/dracula.rasi")  end,
              {description = "show the menubar", group = "launcher"}),

    -- Scratchpad
    awful.key({ modkey }, "u", function() term_scratch:toggle() end, 
              {description = "Ranger", group = "Scratchpad"}),
    awful.key({ modkey }, "t", function() term:toggle() end,
              {description = "Term", group = "Scratchpad"}),
    awful.key({ modkey }, "/", function() mixer:toggle() end,
              {description = "Mixer", group = "Scratchpad"}),
    awful.key({ modkey }, "g", function() galculator:toggle() end,
              {description = "Galculator", group = "Scratchpad"}),
    awful.key({ modkey }, "y", function() ytm:toggle() end,
              {description = "YTMusic", group = "Scratchpad"}),

    -- Launch program
    awful.key({ modkey, }, "a", function() awful.spawn(browser) end,
    { description = "open firefox", group = "launcher" }),
    awful.key({ modkey, }, "e", function() awful.spawn.with_shell(filemanager) end,
    { description = "open filemanager", group = "launcher" }),
    awful.key({ modkey, "Shift" }, "s", function() awful.spawn.with_shell("flameshot gui") end,
    { description = "screenshot", group = "launcher" }),

    -- Launch Scripts
    --   awful.key({modkey,"Control"},"l",function () awful.spawn.with_shell("cd ~/GoogleDrive/Università/Libri && zathura $(ls | rofi -dmenu -i -theme ~/.config/rofi/dracula.rasi)")end,
    -- { description = "open book", group = "Scripts" })

      awful.key({modkey,"Control"},"l",function () awful.spawn.with_shell("/bin/bash -c /home/stefanomarton/Scripts/libriv2")end,
    { description = "open book", group = "Scripts" }),


-- AudioControl
awful.key ( { }, "XF86AudioRaiseVolume", function() awful.spawn.with_shell"amixer -D pulse sset Master 5%+" end,
{ description = "VolumeUp", group = "System Control" }),

awful.key ( { }, "XF86AudioLowerVolume", function() awful.spawn.with_shell"amixer -D pulse sset Master 5%-" end,
{ description = "VolumeDown", group = "System Control" }),

awful.key ( { }, "XF86AudioMute", function() awful.spawn.with_shell"amixer set Master toggle" end,
{ description = "Mute", group = "System Control" }),

awful.key({}, "XF86MonBrightnessUp", function ()
  awful.spawn.with_shell"xbacklight -inc 5" 
end,
{ description = "Brightness Up", group = "System Control" }),

awful.key({}, "XF86MonBrightnessDown", function ()
  awful.spawn.with_shell"xbacklight -dec 5" 
end,
{ description = "Brightness Down", group = "System Control" })


  )
   

  return globalkeys
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
