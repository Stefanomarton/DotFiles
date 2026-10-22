-----------------
---- LAYOUTS ----
-----------------

-- Master con i deck di carte ai lati (G9, Xiaomi), o master a sinistra e deck a destra (portatili).
-- Le carte di un deck sono impilate in verticale: di ognuna resta visibile una linguetta in alto,
-- e la carta col focus viene disegnata sopra le altre. Il focus al passaggio del mouse sceglie la carta
-- disegnata sopra solo con la patch di Hyprland in ~/.nixos/assets/hyprland-tiled-hit-order.patch.
-- Per workspace: colonne da sinistra a destra, ognuna con le sue finestre dall'alto in basso
-- (indirizzi), per Super+j/slash/k/l, perché il focus per direzione non distingue carte
-- sovrapposte; master per Super+Space; larghezza della master cambiata con Super+Ctrl+j/slash
-- (keybindings.lua).
deck_columns = {}
deck_master = {}
deck_ratio = {}

local TAB = 35 -- altezza della linguetta di ogni carta

local function master_ratio(ctx, ws)
    local default = #ctx.targets == 2 and 0.5 or ctx.area.w >= 3000 and 0.45 or 0.6
    return deck_ratio[ws] or default
end

-- carte ctx.targets[from..to] nella colonna (x, w), ognuna una linguetta più in basso della precedente
local function place_deck(ctx, from, to, x, w)
    local h = ctx.area.h - (to - from) * TAB
    for i = from, to do
        ctx.targets[i]:place({
            x = math.floor(x),
            y = math.floor(ctx.area.y + (i - from) * TAB),
            w = math.floor(w),
            h = math.floor(h)
        })
    end
end

-- indirizzi delle finestre ctx.targets[from..to]
local function deck_addresses(ctx, from, to)
    local list = {}
    for i = from, to do
        local w = ctx.targets[i].window
        if w then list[#list + 1] = w.address end
    end
    return list
end

hl.layout.register("custom_center2", {
    recalculate = function(ctx)
        local n = #ctx.targets

        if n == 0 then return end

        local first = ctx.targets[1].window
        local ws = first and first.workspace and first.workspace.id
        if ws then
            deck_columns[ws] = nil
            deck_master[ws] = first.address
        end
        local ratio = master_ratio(ctx, ws)

        -- I margini laterali fissi hanno senso solo sugli ultrawide (G9, Xiaomi): sul pannello
        -- di un portatile una finestra sola resterebbe larga pochi pixel.
        local wide = ctx.area.w >= 3000

        if n == 1 then
            local gap = wide and 900 or 0
            ctx.targets[1]:place({
                x = math.floor(ctx.area.x + gap),
                y = math.floor(ctx.area.y),
                w = math.floor(ctx.area.w - (2 * gap)),
                h = math.floor(ctx.area.h)
            })

        elseif n == 2 then
            local gap = wide and 200 or 0
            local avail = ctx.area.w - (2 * gap)
            local first_w = math.floor(avail * ratio)

            ctx.targets[1]:place({
                x = math.floor(ctx.area.x + gap),
                y = math.floor(ctx.area.y),
                w = first_w,
                h = math.floor(ctx.area.h)
            })
            ctx.targets[2]:place({
                x = math.floor(ctx.area.x + gap + first_w),
                y = math.floor(ctx.area.y),
                w = math.floor(avail - first_w),
                h = math.floor(ctx.area.h)
            })

        elseif not wide then
            -- 3+ finestre su uno schermo stretto: master a sinistra, le altre in un deck a destra
            local master_w = math.floor(ctx.area.w * ratio)

            if ws then deck_columns[ws] = { { first.address }, deck_addresses(ctx, 2, n) } end

            ctx.targets[1]:place({
                x = math.floor(ctx.area.x),
                y = math.floor(ctx.area.y),
                w = master_w,
                h = math.floor(ctx.area.h)
            })
            place_deck(ctx, 2, n, ctx.area.x + master_w, ctx.area.w - master_w)

        else
            -- 3+ finestre: master al centro, deck a sinistra e a destra
            local master_w = math.floor(ctx.area.w * ratio)
            local side_w = math.floor((ctx.area.w - master_w) / 2)

            local left_count = math.ceil((n - 1) / 2)

            if ws then
                deck_columns[ws] = {
                    deck_addresses(ctx, 2, left_count + 1),
                    { first.address },
                    deck_addresses(ctx, left_count + 2, n),
                }
            end

            place_deck(ctx, 2, left_count + 1, ctx.area.x, side_w)
            ctx.targets[1]:place({
                x = math.floor(ctx.area.x + side_w),
                y = math.floor(ctx.area.y),
                w = master_w,
                h = math.floor(ctx.area.h)
            })
            if n > left_count + 1 then
                place_deck(ctx, left_count + 2, n, ctx.area.x + side_w + master_w, side_w)
            end
        end
    end,

    -- "ratio +0.05" / "ratio -0.05": allarga o stringe la master del workspace
    layout_msg = function(ctx, msg)
        local delta = tonumber(msg:match("^ratio%s+([+-]?[%d.]+)$") or "")
        local first = ctx.targets[1] and ctx.targets[1].window
        if not delta then return "custom_center2: messaggio sconosciuto (uso: ratio +0.05)" end
        if not first then return end
        local ws = first.workspace.id
        deck_ratio[ws] = math.max(0.2, math.min(0.8, master_ratio(ctx, ws) + delta))
    end,
})

-- Built-in layout engine tuning
-- https://wiki.hypr.land/Configuring/Dwindle-Layout/
hl.config({
    dwindle = {
        preserve_split = true,
    },
})

-- https://wiki.hypr.land/Configuring/Master-Layout/
hl.config({
    master = {
        new_status            = "slave",
        orientation           = "center",
        focus_master_on_close = true,
        mfact                 = 0.45,
    },
})

-- https://wiki.hypr.land/Configuring/Scrolling-Layout/
hl.config({
    scrolling = {
        fullscreen_on_one_column = true,
        column_width             = 0.35,
        focus_fit_method         = 1,
        follow_focus             = true,
        follow_min_visible       = 0.5,
    },
})
