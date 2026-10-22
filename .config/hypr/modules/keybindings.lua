---------------------
---- KEYBINDINGS ----
---------------------

-- mainMod is defined in modules/vars.lua.
-- Apps and actions (with their shortcuts) are in modules/menu.lua, the menu shown while Super is held;
-- here only movement, workspaces, mouse and volume. See https://wiki.hypr.land/Configuring/Basics/Binds/

-- Movement, workspaces, volume and mouse. A function because the which-key submap
-- (modules/menu.lua) binds them again, with `after` closing the menu, so they keep
-- working however long Super is held. Here `after` tells the menu a shortcut fired, so
-- holding Super after Super+1 doesn't open it.
function navigation_binds(after)
    local function bind(keys, action, opts)
        if after and not (opts and opts.mouse) then   -- drag/resize must stay mouse dispatchers
            local run = action
            action = function()
                if type(run) == "function" then run() else hl.dispatch(run) end
                after()
            end
        end
        hl.bind(keys, action, opts)
    end

    bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
    bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

    -- Move focus with mainMod + arrow keys
    bind(mainMod .. " + j",  hl.dsp.focus({ direction = "left" }))
    bind(mainMod .. " + slash", hl.dsp.focus({ direction = "right" }))
    bind(mainMod .. " + l",    hl.dsp.focus({ direction = "up" }))
    bind(mainMod .. " + k",  hl.dsp.focus({ direction = "down" }))

    bind(mainMod .. "+ page_down", hl.dsp.window.cycle_next({next = false}))
    bind(mainMod .. "+ page_up", hl.dsp.window.cycle_next())

    -- Switch workspaces with mainMod + [0-9]
    -- Move active window to a workspace with mainMod + SHIFT + [0-9]
    for i = 1, 10 do
        local key = i % 10 -- 10 maps to key 0
        bind(mainMod .. " + " .. key,             hl.dsp.focus({ workspace = i}))
        bind(" + ALT + " .. key,     hl.dsp.window.move({ workspace = i, follow=false }))
    end

    -- Volume (repeats while held; capped at 100%)
    bind(mainMod .. " + Up",   hl.dsp.exec_cmd("wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+"), { repeating = true })
    bind(mainMod .. " + Down", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),        { repeating = true })

    bind("mouse_right", hl.dsp.layout("move -500"), {mouse = true})
    bind("mouse_left", hl.dsp.layout("move +500"), {mouse = true})


    bind(mainMod .. "+ SHIFT + slash",
            hl.dsp.window.swap({direction = "right"}))

    bind(mainMod .. "+ SHIFT + j",
            hl.dsp.window.swap({direction = "left"}))

    bind(mainMod .. "+ SHIFT + k",
            hl.dsp.window.resize({x = 200, y = 0, relative = true}))
end

-- Send a Quickshell shortcut from inside a Lua function bind. Not hl.dispatch(hl.dsp.global()):
-- a function bind that dispatches `global` is run again when the key is released, so Super+A
-- opened yazi on press and closed it on release. Through hyprctl it is not tied to the key.
function qs_send(name)
    hl.dispatch(hl.dsp.exec_cmd([[hyprctl dispatch 'hl.dsp.global("quickshell:]] .. name .. [[")']]))
end

navigation_binds(function() qs_send("whichkey-cancel") end)


-- Entering the submap
hl.bind(mainMod .. " + CTRL + W", function()
    hl.dispatch(hl.dsp.submap("clean"))
    hl.dispatch(hl.dsp.focus({ workspace = "name:w11" }))
end)

-- Defining the submap
hl.define_submap("clean", function()
    -- Exiting the submap
        hl.bind(mainMod .. " + CTRL + W", function()
        hl.dispatch(hl.dsp.submap("reset"))
        hl.dispatch(hl.dsp.focus({ workspace = "previous_per_monitor" }))
        end)
end)


-- hl.bind(mainMod .. " + comma", hl.dsp.layout("swapcol l"))jjj
