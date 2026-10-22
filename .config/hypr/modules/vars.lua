-----------------------------
---- SHARED VARIABLES ----
-----------------------------

-- Global (no `local`) so every required module can reference them,
-- the same way Hyprland exposes the `hl` API globally.

mainMod     = "SUPER" -- main modifier ("Windows" key)
terminal    = "kitty"

-- Current machine, so monitors/workspaces can differ per host (desktop, laptop, ...).
local function detect_host()
    local f = io.open("/etc/hostname", "r")
    if f then
        local h = f:read("*l")
        f:close()
        if h then
            h = h:gsub("%s+", "")
            if h ~= "" then return h end
        end
    end
    return os.getenv("HOSTNAME") or "unknown"
end

host = detect_host()

-- Come gli host NixOS (~/.nixos/hosts). laptop: pannello eDP-1 e tastiera interna con kmonad;
-- personal: feature personali (Nextcloud, Home Assistant), mai su tm-laptop.
local profiles = {
    desktop       = { laptop = false, personal = true  },
    laptop        = { laptop = true,  personal = true  },
    ["tm-laptop"] = { laptop = true,  personal = false },
}
profile = profiles[host] or { laptop = false, personal = false }

-- I due monitor del fisso arrivano anche ai portatili dalla dock HP, ma con un DP-x
-- diverso: si riconoscono dalla descrizione (basta l'inizio).
screens = {
    main  = "desc:Samsung Electric Company Odyssey G91F", -- G9 5120x1440, il principale
    side  = "desc:Xiaomi Corporation Mi Monitor",         -- 3440x1440, sopra il G9
    panel = "eDP-1",                                      -- pannello dei portatili, sotto il G9
}
