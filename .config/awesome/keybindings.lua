pcall(require, "luarocks.loader")
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
require("scratchpads")
local lain = require("lain")
local bling = require("module.bling")

-- {{{ Key bindings

modkey = "Mod4"

-- General Awesome keys
awful.keyboard.append_global_keybindings({
	awful.key({ "Mod1" }, "h", hotkeys_popup.show_help, { description = "show help", group = "awesome" }),
	awful.key({ modkey }, "w", function()
		mymainmenu:show()
	end, { description = "show main menu", group = "awesome" }),

	awful.key({ modkey, "Control" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
	awful.key({ modkey, "Shift" }, "q", awesome.quit, { description = "quit awesome", group = "awesome" }),
	-- awful.key({ modkey }, "x", function()
	-- 	awful.prompt.run({
	-- 		prompt = "Run Lua code: ",
	-- 		textbox = awful.screen.focused().mypromptbox.widget,
	-- 		exe_callback = awful.util.eval,
	-- 		history_path = awful.util.get_cache_dir() .. "/history_eval",
	-- 	})
	-- end, { description = "lua execute prompt", group = "awesome" }),
	awful.key({ modkey }, "Return", function()
		awful.spawn(terminal)
	end, { description = "open a terminal", group = "launcher" }),
	-- awful.key({ modkey }, "r", function()
	-- 	awful.screen.focused().mypromptbox:run()
	-- end, { description = "run prompt", group = "launcher" }),
	-- awful.key({ modkey }, "p", function()
	-- 	menubar.show()
	-- end, { description = "show the menubar", group = "launcher" }),
	awful.key({ modkey, "Shift" }, "b", function()
		local s = awful.screen.focused()
		s.mywibox.visible = not s.mywibox.visible
	end, { description = "toggle wibox", group = "awesome" }),
})

-- Client and Tabs Bindings
--TODO
awful.keyboard.append_global_keybindings({
	awful.key({ "Mod1" }, "a", function()
		bling.module.tabbed.pick_with_dmenu()
	end, { description = "pick client to add to tab group", group = "tabs" }),
	awful.key({ "Mod1" }, "s", function()
		bling.module.tabbed.iter()
	end, { description = "iterate through tabbing group", group = "tabs" }),
	awful.key({ "Mod1" }, "d", function()
		bling.module.tabbed.pop()
	end, {
		description = "remove focused client from tabbing group",
		group = "tabs",
	}),
	awful.key({ modkey }, "h", function()
		awful.client.focus.bydirection("left")
		if client.focus then
			client.focus:raise()
		end
	end),
	awful.key({ modkey }, "j", function()
		awful.client.focus.bydirection("down")
		if client.focus then
			client.focus:raise()
		end
	end),
	awful.key({ modkey }, "k", function()
		awful.client.focus.bydirection("up")
		if client.focus then
			client.focus:raise()
		end
	end),
	awful.key({ modkey }, "l", function()
		awful.client.focus.bydirection("right")
		if client.focus then
			client.focus:raise()
		end
	end),
	awful.key({ modkey, "Shift" }, "j", function()
		awful.client.swap.byidx(1)
	end, { description = "swap with next client by index", group = "client" }),
	awful.key({ modkey, "Shift" }, "k", function()
		awful.client.swap.byidx(-1)
	end, {
		description = "swap with previous client by index",
		group = "client",
	}),
	awful.key({ modkey }, "u", awful.client.urgent.jumpto, { description = "jump to urgent client", group = "client" }),
})

-- Tags related keybindings
awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "Left", awful.tag.viewprev, { description = "view previous", group = "tag" }),
	awful.key({ modkey }, "Right", awful.tag.viewnext, { description = "view next", group = "tag" }),
	awful.key({ modkey }, "Escape", awful.tag.history.restore, { description = "go back", group = "tag" }),
})

-- Layout selection
awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "t", function()
		awful.layout.set(awful.layout.suit.tile)
	end, { description = "Tile layouts", group = "layout" }),
	awful.key({ modkey }, "c", function()
		awful.layout.set(bling.layout.centered)
	end, { description = "Centered layout", group = "layout" }),
	awful.key({ modkey }, "d", function()
		awful.layout.set(bling.layout.mstab)
	end, { description = "Tabbed", group = "layout" }),
})

-- Run Program shortcuts
awful.keyboard.append_global_keybindings({
	awful.key({ modkey, "Control" }, "s", function()
		awful.spawn.with_shell("flameshot gui")
	end, { description = "Flameshot", group = "launcher" }),
	awful.key({ modkey }, "f", function()
		awful.spawn("firefox")
	end, { description = "Firefox", group = "launcher" }),
	awful.key({ modkey }, "s", function()
		awful.spawn.with_shell("dmenu_run -l 5")
	end, { description = "dmenu", group = "launcher" }),
	awful.key({ modkey, "Shift" }, "s", function()
		awful.spawn("rofi -show windowcd")
	end, { description = "dmenu", group = "launcher" }),
	awful.key({ modkey,  }, "b", function()
		awful.spawn("rofi -show filebrowser")
	end, { description = "dmenu", group = "launcher" }),
	awful.key({ modkey, "Control" }, "f", function()
				awful.spawn.with_shell("bash -c ~/.local/bin/scripts/searchOnFirefox.sh")
	end, { description = "Search on Firefox", group = "launcher" }),
	awful.key({ modkey }, "b", function()
		awful.spawn.with_shell("bash -c ~/.local/bin/scripts/libriv2")
	end, { description = "Open a book", group = "launcher" }),
})

-- Scratchpads
awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "a", function()
		ranger:toggle()
	end, { description = "Ranger", group = "Scratchpads" }),
	awful.key({ modkey }, "y", function()
		music:toggle()
	end, { description = "Ncspot", group = "Scratchpads" }),
	awful.key({ modkey }, "g", function()
		lazygit:toggle()
	end, { description = "lazygit", group = "Scratchpads" }),
	awful.key({ modkey }, "r", function()
		repo:toggle()
	end, { description = "repo", group = "Scratchpads" }),
	awful.key({ modkey }, "n", function()
		emacs:toggle()
	end, { description = "emacs", group = "Scratchpads" }),
})

-- Focus related keybindings
awful.keyboard.append_global_keybindings({
	-- Focusing by direction
	awful.key({ modkey }, "e", function()
		client.focus = awful.client.getmaster()
		client.focus:raise()
	end),
	awful.key({ modkey }, "j", function()
		awful.client.focus.byidx(1)
	end, { description = "focus next by index", group = "client" }),
	awful.key({ modkey }, "k", function()
		awful.client.focus.byidx(-1)
	end, { description = "focus previous by index", group = "client" }),
	awful.key({ modkey }, "Tab", function()
		awful.client.focus.history.previous()
		if client.focus then
			client.focus:raise()
		end
	end, { description = "go back", group = "client" }),
	awful.key({ modkey }, ",", function()
		awful.screen.focus_relative(1)
	end, { description = "focus the next screen", group = "screen" }),
	awful.key({ modkey }, ".", function()
		awful.screen.focus_relative(-1)
	end, { description = "focus the previous screen", group = "screen" }),
})

-- Layout related keybindings
awful.keyboard.append_global_keybindings({
	awful.key({ modkey, "Shift" }, "j", function()
		awful.client.swap.byidx(1)
	end, { description = "swap with next client by index", group = "client" }),
	awful.key({ modkey, "Shift" }, "k", function()
		awful.client.swap.byidx(-1)
	end, { description = "swap with previous client by index", group = "client" }),
	awful.key({ modkey }, "u", awful.client.urgent.jumpto, { description = "jump to urgent client", group = "client" }),
	awful.key({ modkey, "Control" }, "l", function()
		awful.tag.incmwfact(0.05)
	end, { description = "increase master width factor", group = "layout" }),
	awful.key({ modkey, "Control" }, "h", function()
		awful.tag.incmwfact(-0.05)
	end, { description = "decrease master width factor", group = "layout" }),
	awful.key({ modkey, "Shift" }, "i", function()
		awful.tag.incnmaster(1, nil, true)
	end, { description = "increase the number of master clients", group = "layout" }),
	awful.key({ modkey, "Shift" }, "d", function()
		awful.tag.incnmaster(-1, nil, true)
	end, { description = "decrease the number of master clients", group = "layout" }),
	awful.key({ modkey, "Control" }, ",", function()
		awful.layout.inc(1)
	end, { description = "select next", group = "layout" }),
	awful.key({ modkey, "Control" }, ".", function()
		awful.layout.inc(-1)
	end, { description = "select previous", group = "layout" }),
})

-- awful.keyboard.append_global_keybindings({
-- 	awful.key({
-- 		modifiers = { modkey },
-- 		keygroup = "numrow",
-- 		description = "only view tag",
-- 		group = "tag",
-- 		on_press = function(index)
-- 			local screen = awful.screen.focused()
-- 			local tag = screen.tags[index]
-- 			if tag then
-- 				tag:view_only()
-- 			end
-- 		end,
-- 	}),
-- 	awful.key({
-- 		modifiers = { modkey, "Control" },
-- 		keygroup = "numrow",
-- 		description = "toggle tag",
-- 		group = "tag",
-- 		on_press = function(index)
-- 			local screen = awful.screen.focused()
-- 			local tag = screen.tags[index]
-- 			if tag then
-- 				awful.tag.viewtoggle(tag)
-- 			end
-- 		end,
-- 	}),
-- 	awful.key({
-- 		modifiers = { modkey, "Shift" },
-- 		keygroup = "numrow",
-- 		description = "move focused client to tag",
-- 		group = "tag",
-- 		on_press = function(index)
-- 			if client.focus then
-- 				local tag = client.focus.screen.tags[index]
-- 				if tag then
-- 					client.focus:move_to_tag(tag)
-- 				end
-- 			end
-- 		end,
-- 	}),
-- 	awful.key({
-- 		modifiers = { modkey, "Control", "Shift" },
-- 		keygroup = "numrow",
-- 		description = "toggle focused client on tag",
-- 		group = "tag",
-- 		on_press = function(index)
-- 			if client.focus then
-- 				local tag = client.focus.screen.tags[index]
-- 				if tag then
-- 					client.focus:toggle_tag(tag)
-- 				end
-- 			end
-- 		end,
-- 	}),
-- })

-- {{{ Mouse bindings
awful.mouse.append_global_mousebindings({
	awful.button({}, 3, function()
		mymainmenu:toggle()
	end),
	awful.button({ modkey }, 4, awful.tag.viewnext),
	awful.button({ modkey }, 5, awful.tag.viewprev),
})

client.connect_signal("request::default_mousebindings", function()
	awful.mouse.append_client_mousebindings({
		awful.button({}, 1, function(c)
			c:activate({ context = "mouse_click" })
		end),
		awful.button({ modkey }, 1, function(c)
			c:activate({ context = "mouse_click", action = "mouse_move" })
		end),
		awful.button({ modkey }, 3, function(c)
			c:activate({ context = "mouse_click", action = "mouse_resize" })
		end),
	})
end)

client.connect_signal("request::default_keybindings", function()
	awful.keyboard.append_client_keybindings({
		awful.key({ modkey }, "m", function(c)
			c.fullscreen = not c.fullscreen
			c:raise()
		end, { description = "toggle fullscreen", group = "client" }),
		awful.key({ modkey }, "q", function(c)
			c:kill()
		end, { description = "close", group = "client" }),
		awful.key(
			{ modkey, "Shift" },
			"f",
			awful.client.floating.toggle,
			{ description = "toggle floating", group = "client" }
		),
		awful.key({ modkey }, "v", function(c)
			c:swap(awful.client.getmaster())
		end, { description = "move to master", group = "client" }),

		-- Move client to next screen
		awful.key({ modkey, "Shift" }, ".", function(c)
			c:move_to_screen(c.screen.index - 1)
		end, { description = "move to screen left", group = "client" }),

		-- Move client to previous screen
		awful.key({ modkey, "Shift" }, ",", function(c)
			c:move_to_screen(c.screen.index + 1)
		end, { description = "move to screen right", group = "client" }),
	})
end)

-- }}}
