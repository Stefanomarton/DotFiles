-- Standard awesome library
local gears = require("gears")
local awful     = require("awful")
local beautiful = require("beautiful")

-- Wibox handling library
local wibox = require("wibox")

-- Custom Local Library: Common Functional Decoration
local deco = {
  wallpaper = require("deco.wallpaper"),
  taglist   = require("deco.taglist"),
  tasklist  = require("deco.tasklist")
}

local taglist_buttons  = deco.taglist()
local tasklist_buttons = deco.tasklist()

local _M = {}

-- Custom widget
local cpu_widget = require("deco.widget.cpu-widget.cpu-widget")
local volume_widget = require("deco.widget.volume-widget.volume")
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- {{{ Wibar
-- Create a textclock widget

awful.screen.connect_for_each_screen(function(s)

  if s == screen.primary
  then

  -- Wallpaper
  -- set_wallpaper(s)

  -- Create a promptbox for each screen
  -- s.mypromptbox = awful.widget.prompt()


   mytextclock = wibox.widget.textclock()

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
    awful.button({ }, 1, function () awful.layout.inc( 1) end),
    awful.button({ }, 3, function () awful.layout.inc(-1) end),
    awful.button({ }, 4, function () awful.layout.inc( 1) end),
    awful.button({ }, 5, function () awful.layout.inc(-1) end)
  ))

  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = taglist_buttons,
    style   = {
        shape = function(cr, width, height)
                gears.shape.circle(cr, 12, 12)
            end, 
        spacing = 30,
        align = "center",
      },
      layout  = wibox.layout.fixed.horizontal
  }

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = tasklist_buttons,
          style   = {
        shape  = gears.shape.rounded_rect,
        spacing = 50,
        layout= {
          valign = "center",
          halign = "center",
        }
       },
                }

-- Create the systray
local systray = wibox.widget.systray()
systray:set_base_size(20)

  -- Create the wibox
  s.mywibox = awful.wibar({ position = "top", screen = 1, height = 35, bg = "beautiful.bg_normal", widht= 500})

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    expand = "none",
    { -- Left widgets
      layout = wibox.layout.align.horizontal,
      -- wibox.container.margin(wibox.container.place(RC.launcher),10,10,10,5),
      -- s.mypromptbox,
      wibox.container.margin(wibox.container.background(wibox.container.place(s.mylayoutbox),beautiful.bg_normal,gears.shape.rounded_rect),1200,4,5,3),
      wibox.container.margin(wibox.container.background(wibox.container.place(mytextclock), beautiful.bg_normal, gears.shape.rounded_rect), 15,0,5,3),
    },
      wibox.container.margin(wibox.container.background(wibox.container.margin(wibox.container.place(s.mytaglist),5,0,8,0), beautiful.bg_normal, gears.shape.rounded_rect),0,0,5,3),
    -- s.mytasklist, -- Middle widget
    { -- Right widgets
      layout = wibox.layout.align.horizontal,
      wibox.container.margin(wibox.container.background(wibox.container.margin(cpu_widget({ color = '#1ef956' }),5,5),beautiful.bg_normal, gears.shape.rounded_rect ),10,10,5,3),

wibox.container.margin(wibox.container.background(wibox.container.margin(
        volume_widget {
          widget_type = "horizontal_bar",
          with_icon = false,
          mute_color = "#ff0000",
          main_color = "#F1FA8C",
        },5,10,0,0),beautiful.bg_normal, gears.shape.rounded_rect),2,2,5,3),
      
    wibox.container.margin(wibox.container.background(wibox.container.margin(wibox.container.place(systray),10,10,3,3),beautiful.bg_normal, gears.shape.rounded_rect),5,1340,5,3)
    },
  }
-- Create the wibox
  s.mywibox2 = awful.wibar({ position = "bottom", screen = 1, bg = "beautiful.bg_normal",height = 30})

s.mywibox2:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
      layout = wibox.layout.align.horizontal,
      -- RC.launcher,
      -- s.mytaglist,
      -- s.mypromptbox,
    },
    wibox.container.margin(wibox.container.place(s.mytasklist),0,0,5,5),
     -- Middle widget
    { -- Right widgets
      layout = wibox.layout.align.horizontal,
      -- wibox.container.margin(wibox.container.place(systray),0,40,0,0),
      -- wibox.container.margin(wibox.container.place(mytextclock),0,40,0,0),
      -- wibox.container.margin(wibox.container.place(s.mylayoutbox),0,40,0,0)
    },
  }

end
end)
-- }}}
