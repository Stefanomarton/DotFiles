--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
-- and https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/

-- Example window rules that are useful

local suppressMaximizeRule = hl.window_rule({
    -- Ignore maximize requests from all apps. You'll probably like this.
    name  = "suppress-maximize-events",
    match = { class = ".*" },
    suppress_event = "maximize",
})
-- suppressMaximizeRule:set_enabled(false)

hl.window_rule({
    -- Fix some dragging issues with XWayland
    name  = "fix-xwayland-drags",
    match = {
        class      = "^$",
        title      = "^$",
        xwayland   = true,
        float      = true,
        fullscreen = false,
        pin        = false,
    },

    no_focus = true,
})

hl.window_rule({
      name = "dim-aroud-scratchpads",
      match = {
         class = "kitty-yazi.*",

      },
      dim_around = true,
})

hl.window_rule({
      name = "nextcloud-floating",
      match = {
         class = "com.nextcloud.desktopclient.nextcloud",
      },
         stay_focused = false,
         float = true,
         center = true,
         monitor = screens.main,
})

hl.window_rule({
      name = "float-picker",
      match = {
         class = "xdg-desktop-portal-gtk"
      },
      float = true,
      center = true,
      size = {"(monitor_w*0.5)", "(monitor_h*0.5)"},
      monitor = screens.main
})


-- Zotero: every window but the main one is a popup (dialogs, Sci-Hub, "Progress" toasts)
hl.window_rule({
      name = "zotero-popups",
      match = {
         class = "^Zotero$",
      },
      float = true,
      center = true,
})

hl.window_rule({
      name = "zotero-main",
      match = {
         class = "^Zotero$",
         title = "^(.* - )?Zotero$",
      },
      tile = true,
})

-- the "Progress" toasts (PDF downloads, sync): bottom-right corner, without taking focus
hl.window_rule({
      name = "zotero-progress",
      match = {
         class = "^Zotero$",
         title = "^Progress$",
      },
      no_initial_focus = true,
      move = {"(monitor_w-window_w-20)", "(monitor_h-window_h-20)"},
})


hl.on("window.title", function(w)
    local prefix = "Bitwarden"
    if w.title:sub(1, #prefix) == prefix then
        local monitor = hl.get_active_monitor()
        if monitor == nil then
            return
        end
        local win = { width = 500, height = 800 }
        local pos = {
            x = math.floor((monitor.width  - win.width)  / 2),
            y = math.floor((monitor.height - win.height) / 2),
        }
        hl.dispatch(hl.dsp.window.float({ action = "enable", window = w }))
        hl.dispatch(hl.dsp.window.resize({ x = win.width, y = win.height, relative = false, window = w }))
        hl.dispatch(hl.dsp.window.move({ x = pos.x, y = pos.y, relative = false, window = w }))
    end
end)


--------------------
---- LAYER RULES ----
--------------------

-- Modal backdrop: when a picker is open, blur the WHOLE desktop behind it so it
-- stands out (paired with the dim scrim, Theme.dimAround). ignore_alpha = 0 blurs
-- the full-screen overlay, not just the card. Only the pickers (not the always-on
-- corner or the transient volume OSD).
hl.layer_rule({
    name  = "widget-blur",
    match = { namespace = "^(smart-picker|workspaces-picker|window-picker|move-window-picker|expose)$" },
    blur         = true,
    ignore_alpha = 0.0,
})

-- Popup di dettatura (scripts/dettatura-emacs.sh): frame Emacs chiamato "dettatura"
hl.window_rule({
    name  = "dettatura-popup",
    match = { class = "emacs", title = "^dettatura$" },
    float   = true,
    center  = true,
    size    = {"(monitor_w*0.4)", "(monitor_h*0.35)"},
    monitor = screens.main,
})
