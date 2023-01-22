pcall(require, "luarocks.loader")
local gfs = require("gears.filesystem")
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local ruled = require("ruled")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
require("awful.hotkeys_popup.keys")

-- requiring keybindings

-- {{{ Error handling
naughty.connect_signal("request::display_error", function(message, startup)
	naughty.notification({
		urgency = "critical",
		title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
		message = message,
	})
end)
-- }}}

-- Themes define colours, icons, font and wallpapers.
local theme_path = string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), "nord")
beautiful.init(theme_path)

--Set wallpaper
local function set_wallpaper(s)
	if beautiful.wallpaper then
		local wallpaper = beautiful.wallpaper
		if type(wallpaper) == "function" then
			wallpaper = wallpaper(s)
		end
		gears.wallpaper.tiled(wallpaper, s)
	end
end
set_wallpaper()

local bling = require("module.bling")
require("keybindings")

-- Variale settings
terminal = "kitty"
editor = "nvim"
editor_cmd = terminal .. " -e " .. editor
modkey = "Mod4"
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
	{
		"hotkeys",
		function()
			hotkeys_popup.show_help(nil, awful.screen.focused())
		end,
	},
	{ "manual", terminal .. " -e man awesome" },
	{ "edit config", editor_cmd .. " " .. awesome.conffile },
	{ "restart", awesome.restart },
	{
		"quit",
		function()
			awesome.quit()
		end,
	},
}

mymainmenu = awful.menu({
	items = {
		{ "awesome", myawesomemenu, beautiful.awesome_icon },
		{ "open terminal", terminal },
	},
})

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon, menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Tag layout
-- Table of layouts to cover with awful.layout.inc, order matters.
tag.connect_signal("request::default_layouts", function()
	awful.layout.append_default_layouts({
		-- awful.layout.suit.floating,
		awful.layout.suit.tile,
		awful.layout.suit.tile.left,
		awful.layout.suit.tile.bottom,
		awful.layout.suit.tile.top,
		awful.layout.suit.fair,
		awful.layout.suit.fair.horizontal,
		awful.layout.suit.spiral,
		awful.layout.suit.spiral.dwindle,
		awful.layout.suit.max,
		awful.layout.suit.max.fullscreen,
		awful.layout.suit.magnifier,
		awful.layout.suit.corner.nw,
	})
end)
-- }}}

-- {{{ Wibar

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Defining tags per monitor

local sharedtags = require("module.sharedtags")
local tags = sharedtags({
	{ name = "1", screen = 1, layout = bling.layout.centered },
	{ name = "2", screen = 1, layout = bling.layout.centered },
	{ name = "3", screen = 1, layout = bling.layout.centered },
	{ name = "4", screen = 1, layout = bling.layout.centered },
	{ name = "5", screen = 1, layout = bling.layout.centered },
	{ name = "6", screen = 1, layout = bling.layout.centered },
	{ name = "7", screen = 2, layout = awful.layout.suit.spiral, gap = 5 },
	{ name = "8", screen = 2, layout = awful.layout.suit.spiral, gap = 5 },
	{ name = "9", screen = 3, layout = awful.layout.suit.spiral, gap = 5 },
	{ name = "10", screen = 3, layout = awful.layout.suit.spiral, gap = 5 },
	-- { layout = awful.layout.layouts[1] },
	-- { screen = 2, layout = awful.layout.layouts[1] },
})
for i = 1, 9 do
	awful.keyboard.append_global_keybindings({
		-- View tag only.
		awful.key({ modkey }, "#" .. i + 9, function()
			local screen = awful.screen.focused()
			local tag = tags[i]
			if tag then
				sharedtags.jumpto(tag, screen)
			end
		end, { description = "view tag #" .. i, group = "tag" }),

		-- Toggle tag display.
		awful.key({ modkey, "Control" }, "#" .. i + 9, function()
			local screen = awful.screen.focused()
			local tag = tags[i]
			if tag then
				sharedtags.viewtoggle(tag, screen)
			end
		end, { description = "toggle tag #" .. i, group = "tag" }),

		-- Move client to tag.
		awful.key({ modkey, "Shift" }, "#" .. i + 9, function()
			if client.focus then
				local tag = tags[i]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
		end, { description = "move focused client to tag #" .. i, group = "tag" }),

		-- Move client to tag and focus it
		awful.key({ "Mod1" }, "#" .. i + 9, function()
			if client.focus then
				local tag = tags[i]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
			local screen = awful.screen.focused()
			local tag = tags[i]
			if tag then
				sharedtags.jumpto(tag, screen)
			end
		end, { description = "move focused client to tag #" .. i, group = "tag" }),

		-- Toggle tag on focused client.
		awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function()
			if client.focus then
				local tag = tags[i]
				if tag then
					client.focus:toggle_tag(tag)
				end
			end
		end, { description = "toggle focused client on tag #" .. i, group = "tag" }),
	})
end

awful.screen.connect_for_each_screen(function(s)
	local function set_wallpaper(s)
		-- Wallpaper
		if beautiful.wallpaper then
			local wallpaper = beautiful.wallpaper
			-- If wallpaper is a function, call it with the screen
			if type(wallpaper) == "function" then
				wallpaper = wallpaper(s)
			end
			gears.wallpaper.maximized(wallpaper, s, true)
		end
	end

	if s == screen.primary then
		-- Create a promptbox for each screen
		s.mypromptbox = awful.widget.prompt()

		-- Create an imagebox widget which will contain an icon indicating which layout we're using.
		-- We need one layoutbox per screen.
		s.mylayoutbox = awful.widget.layoutbox({
			screen = s,
			buttons = {
				awful.button({}, 1, function()
					awful.layout.inc(1)
				end),
				awful.button({}, 3, function()
					awful.layout.inc(-1)
				end),
				awful.button({}, 4, function()
					awful.layout.inc(-1)
				end),
				awful.button({}, 5, function()
					awful.layout.inc(1)
				end),
			},
		})

		-- Create a taglist widget
		s.mytaglist = awful.widget.taglist({
			screen = s,
			filter = awful.widget.taglist.filter.all,
			buttons = {
				awful.button({}, 1, function(t)
					t:view_only()
				end),
				awful.button({ modkey }, 1, function(t)
					if client.focus then
						client.focus:move_to_tag(t)
					end
				end),
				awful.button({}, 3, awful.tag.viewtoggle),
				awful.button({ modkey }, 3, function(t)
					if client.focus then
						client.focus:toggle_tag(t)
					end
				end),
				awful.button({}, 4, function(t)
					awful.tag.viewprev(t.screen)
				end),
				awful.button({}, 5, function(t)
					awful.tag.viewnext(t.screen)
				end),
			},
		})

		-- Create a tasklist widget
		s.mytasklist = awful.widget.tasklist({
			screen = s,
			filter = awful.widget.tasklist.filter.currenttags,
			buttons = {
				awful.button({}, 1, function(c)
					c:activate({ context = "tasklist", action = "toggle_minimization" })
				end),
				awful.button({}, 3, function()
					awful.menu.client_list({ theme = { width = 250 } })
				end),
				awful.button({}, 4, function()
					awful.client.focus.byidx(-1)
				end),
				awful.button({}, 5, function()
					awful.client.focus.byidx(1)
				end),
			},
		})

		-- Create the wibox
		s.mywibox = awful.wibar({
			position = "top",
			screen = s,
			widget = {
				layout = wibox.layout.align.horizontal,
				{ -- Left widgets
					layout = wibox.layout.fixed.horizontal,
					mylauncher,
					s.mytaglist,
					s.mypromptbox,
				},
				s.mytasklist, -- Middle widget
				{ -- Right widgets
					layout = wibox.layout.fixed.horizontal,
					-- mykeyboardlayout,
					wibox.widget.systray(),
					mytextclock,
					s.mylayoutbox,
				},
			},
		})
	end
end)

-- }}}

-- {{{ Mouse bindings
-- awful.mouse.append_global_mousebindings({
-- 	awful.button({}, 3, function()
-- 		mymainmenu:toggle()
-- 	end),
-- 	awful.button({}, 4, awful.tag.viewprev),
-- 	awful.button({}, 5, awful.tag.viewnext),
-- })
-- }}}

-- {{{ Rules
-- Rules to apply to new clients.
ruled.client.connect_signal("request::rules", function()
	-- All clients will match this rule.
	ruled.client.append_rule({
		id = "global",
		rule = {},
		properties = {
			focus = awful.client.focus.filter,
			raise = true,
			screen = awful.screen.preferred,
			placement = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	})

	-- Floating clients.
	ruled.client.append_rule({
		id = "floating",
		rule_any = {
			instance = { "copyq", "pinentry" },
			class = {
				"Arandr",
				"Blueman-manager",
				"Gpick",
				"Kruler",
				"Sxiv",
				"Tor Browser",
				"Wpa_gui",
				"veromix",
				"xtightvncviewer",
			},
			-- Note that the name property shown in xprop might be set slightly after creation of the client
			-- and the name shown there might not match defined rules here.
			name = {
				"Event Tester", -- xev.
			},
			role = {
				"AlarmWindow", -- Thunderbird's calendar.
				"ConfigManager", -- Thunderbird's about:config.
				"pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
			},
		},
		properties = { floating = true },
	})

	-- Add titlebars to normal clients and dialogs
	ruled.client.append_rule({
		id = "titlebars",
		rule_any = { type = { "normal", "dialog" } },
		properties = { titlebars_enabled = true },
	})
end)
-- }}}

-- {{{ Notifications

ruled.notification.connect_signal("request::rules", function()
	-- All notifications will match this rule.
	ruled.notification.append_rule({
		rule = {},
		properties = {
			screen = awful.screen.preferred,
			implicit_timeout = 5,
		},
	})
end)

naughty.connect_signal("request::display", function(n)
	naughty.layout.box({ notification = n })
end)

-- }}}

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
	c:activate({ context = "mouse_enter", raise = false })
end)

client.connect_signal("request::manage", function(c)
	c.shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, 5)
	end
end)

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function(c)
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	if not awesome.startup then
		awful.client.setslave(c)
	end

	if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end
end)
