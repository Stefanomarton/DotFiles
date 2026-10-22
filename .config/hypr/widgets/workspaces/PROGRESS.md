# Workspaces Widget — Progress

A [Quickshell](https://quickshell.org) (0.3.0) widget for Hyprland: the Smart picker
(workspaces, windows, apps, shell commands) and the Exposé overview.

- **Location**: `~/.dotfiles/.config/hypr/widgets/workspaces/`
  (Stow-symlinked to `~/.config/hypr/widgets/workspaces/`; moved here 2026-07-11)
- **Launched** from `~/.config/hypr/modules/autostart.lua`: `qs -p ~/.config/hypr/widgets/workspaces/`
- **Shortcuts** (in `modules/menu.lua`): `SUPER+S` smart · `SUPER+E` exposé.
- Usage/architecture: `AGENTS.md`; behaviour and keys of the Smart picker: `SPEC-smart.md`.

## The picker

One Smart picker (Super+S) replaced the workspace, window and move-window pickers and
tofi-run on 2026-10-23: one ranked list + live preview beside it. See `SPEC-smart.md`.

- [x] One list ranked by match, Enter = highlighted row (the first after typing); empty search =
      windows by last use, then project workspaces.
- [x] Prefixes `w ` `p ` `a ` `m ` (move the focused window) and `>` (shell command).
- [x] Focuser + launcher: apps from `DesktopEntries`, hidden while they have an open window.
- [x] No processes on open: data from `Quickshell.Hyprland`, actions via `Hyprland.dispatch`.
- [x] Live workspace mini-map beside the list (below on narrow screens), highlighted window outlined.
- [x] `1`–`10` and `special:` workspaces hidden.

## Config knobs (`Config.qml`)

- `pickerWorkspacePreview` / `pickerWorkspacePreviewLive` — preview on/off, live vs icons
- `pickerPreviewHeight` (520) / `pickerPreviewWidth` (2400, max) — preview size (fit-box)
- `pickerSmartVisibleRows` (8) — rows of the list
- `pickerExcludedWorkspaces` (`1`–`10`), `windowPickerExcludedWorkspaces` — hidden workspaces / windows
- `moveWorkspaceTargetMonitor` — Alt+Enter target monitor (name or model)

## Notes / open items

- **Duplicate copy**: an older, divergent copy using the *classic* `hyprctl dispatch`
  syntax lives at `~/.marton-drive/personal/projects/workspaces-widget/` (its own
  git repo). It's now redundant — safe to delete once this dotfiles copy is confirmed.
- Sibling widgets live under `~/.config/hypr/widgets/`: `corner/` (launched from
  `autostart.lua`) and `pomodoro/` (standalone: `qs -p ~/.config/hypr/widgets/pomodoro/`).
- Possible next steps: MRU workspace ordering, per-tile window titles in the preview.
