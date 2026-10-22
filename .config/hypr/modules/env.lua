---------------------------
---- ENV & PERMISSIONS ----
---------------------------

-- https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/
hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")

-- https://wiki.hypr.land/Configuring/Advanced-and-Cool/Permissions/
-- Changes here require a Hyprland restart (not applied on-the-fly).
-- NixOS: the binaries live in /nix/store (xdph runs as .xdg-desktop-portal-hyprland-wrapped)
hl.permission("/nix/store/[a-z0-9]{32}-grim-[^/]*/bin/grim", "screencopy", "allow")
hl.permission("/nix/store/[a-z0-9]{32}-xdg-desktop-portal-hyprland-[^/]*/libexec/.*xdg-desktop-portal-hyprland.*", "screencopy", "allow")
