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

-- VM Windows a schermo intero nel workspace w11 (Super+Ctrl+W: vm_enter in keybindings.lua).
-- Fisso: Looking Glass, lanciato dal workspace (on_created_empty).
-- tm-laptop: on_created_empty non parte, quindi vm_enter lancia vm_launch se il viewer non c'è:
-- virt-viewer in kiosk, avvia la VM se è spenta e si chiude quando si spegne.
local vm_on_created = { desktop = "looking-glass-client -F" }
vm_launch = ({
    ["tm-laptop"] = "pgrep -x virt-viewer >/dev/null || { virsh -c qemu:///system start tm-windows11 2>/dev/null; "
                 .. "virt-viewer -c qemu:///system --attach --wait --kiosk --kiosk-quit on-disconnect tm-windows11; }",
})[host]

if vm_on_created[host] or vm_launch then
    hl.workspace_rule({
        workspace = "name:w11",
        monitor   = screens.main,
        no_rounding = true,
        decorate  = false,
        no_border = true,
        gaps_in   = 0,
        gaps_out  = 0,
        on_created_empty = vm_on_created[host],
    })
end

if vm_launch then
    hl.window_rule({
        name  = "vm-viewer",
        match = { class = "^virt-viewer$" },
        workspace = "name:w11",
    })
end

-- agent-deck: web UI a tutto schermo (servizio utente agent-deck-web), Super+Del
hl.workspace_rule({
    workspace = "name:agents",
    monitor   = screens.main,
    layout    = "monocle",
    no_rounding = true,
    decorate  = false,
    no_border = true,
    gaps_in   = 0,
    gaps_out  = 0,
})
-- la web UI (aperta da Super+Del, menu.lua) finisce sempre qui
hl.window_rule({
    name      = "agent-deck-web",
    match     = { class = "chrome-127\\.0\\.0\\.1__.*" },
    workspace = "name:agents",
})
