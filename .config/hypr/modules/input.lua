---------------
---- INPUT ----
---------------

-- https://wiki.hypr.land/Configuring/Variables/#input
hl.config({
    input = {
        kb_layout  = "custom_xkb_layout",
        kb_variant = "",
        kb_model   = "",
        kb_options = "fkeys:basic_13-24",
        kb_rules   = "",

        repeat_delay = 100,
        repeat_rate  = 50,

        natural_scroll = true,
        follow_mouse   = 1,
        mouse_refocus  = false, -- il focus segue il mouse solo attraversando un bordo: le attivazioni (clic su notifica) non vengono annullate
        sensitivity    = 0, -- -1.0 - 1.0, 0 means no modification.

        touchpad = {
            natural_scroll       = true,
            scroll_factor        = 1,
            disable_while_typing = true,
        },
    },
})
