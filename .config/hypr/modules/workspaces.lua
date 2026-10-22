--------------------
---- WORKSPACES ----
--------------------

-- https://wiki.hypr.land/Configuring/Workspace-Rules/
-- 1-6 sul G9, 7-9 sullo Xiaomi: sul fisso e sui portatili in dock. Fuori dalla dock
-- i monitor non ci sono e i workspace restano sul pannello.

for i = 1, 6 do
    hl.workspace_rule({ workspace = i, monitor = screens.main, persistent = true })
end

for i = 7, 9 do
    hl.workspace_rule({ workspace = i, monitor = screens.side, persistent = true })
end

if host == "desktop" then
    -- Looking Glass VM workspace
    hl.workspace_rule({
        workspace = "name:w11",
        monitor   = screens.main,
        no_rounding = true,
        decorate  = false,
        no_border = true,
        gaps_in   = 0,
        gaps_out  = 0,
        on_created_empty = "looking-glass-client -F",
    })
end
