from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile.backend.wayland import InputConfig

import os
import subprocess
import json

# Pywal Colors
colors = os.path.expanduser('~/.cache/wal/colors.json')
colordict = json.load(open(colors))
ColorZ=(colordict['colors']['color0'])
ColorA=(colordict['colors']['color1'])
ColorB=(colordict['colors']['color2'])
ColorC=(colordict['colors']['color3'])
ColorD=(colordict['colors']['color4'])
ColorE=(colordict['colors']['color5'])
ColorF=(colordict['colors']['color6'])
ColorG=(colordict['colors']['color7'])
ColorH=(colordict['colors']['color8'])
ColorI=(colordict['colors']['color9'])

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.Popen([home])

mod = "mod4"
terminal = "kitty"

keys = [
    # Switch between windows
    Key([mod], "Slash",
        lazy.layout.right(),
        # lazy.layout.next().when(layout="columns"),
        desc="Move focus to left"),

    Key([mod], "j",
        lazy.layout.left(),
        desc="Move focus to right"),

    Key([mod], "k",
        lazy.layout.down(),
        desc="Move focus down"),

    Key([mod], "l",
        lazy.layout.up(),
        desc="Move focus up"),

    Key([mod], "v",
        lazy.layout.next(),
        desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "j", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "Slash", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "j", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "Slash", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "k", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "l", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    Key(
        [mod],
        "d",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "comma",
        lazy.next_screen(),
        desc='Keyboard focus to next monitor'
        ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "s", lazy.spawn("tofi-drun | zsh", shell=True), desc="Tofi launcher"),
    Key([mod, "shift"], "s", lazy.spawn('grim -g "$(slurp)" - | wl-copy', shell=True), desc="Tofi launcher"),
    Key([mod], "f", lazy.spawn("zsh -c 'export MOZ_ENABLE_WAYLAND=1 && floorp'"), desc="Firefox Browser"),
    Key([mod, "shift"], "Return", lazy.spawn("emacsclient -c"), desc="emacs"),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod],
        "m",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),
    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
    Key([mod], "b", lazy.hide_show_bar(), desc="Hides the bar"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    # Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
]

groups = [
    Group(i) for i in "123456789"
]


for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            Key(["mod1"], i.name, lazy.window.togroup(i.name),
                desc="move focused window to group {}".format(i.name)),
        ]
    )

groups.append(ScratchPad('filemanager',[DropDown('filemanager','zsh -c "kitty -e ranger"', y=0.05, height=0.8, opacity=1),]))

keys.extend([
    Key([mod],'a',lazy.group['filemanager'].dropdown_toggle('filemanager')),
])


layouts = [
    layout.Columns(
        # margin=0,
        align=3,
        split=True, 
        # fair=False,
        num_columns=3,
        # wrap_focus_stacks=True,
        # wrap_focus_columns=True,
        # border_on_single=False,
        # insert_postion=0,

        border_focus=ColorB,
        border_normal=ColorZ,
        border_focus_stack=ColorB,
        border_normal_stack=ColorZ,
        border_width=2,
    ),
    layout.Stack(
        num_stacks=2,
        autosplit=False,
        border_focus=ColorB,
        border_normal=ColorZ,
        border_width=2,
    ),
    # layout.Max(),
    # layout.Tile(),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="JetBrainsMono Nerd Font",
    fontsize=12,
    padding=3,
)

extension_defaults = widget_defaults.copy()

screens = [
    # Screen(
    #     wallpaper="~/.local/share/Wallpapers/Nordic/ign_mountains.png",
    # ),
    Screen(
        wallpaper="~/.local/share/Wallpapers/Nordic/ign_mountains.png",
        wallpaper_mode="fill",
        top=bar.Bar(
            [
                widget.Spacer(length=5),
                widget.CurrentLayout(),
                widget.Spacer(length=5),
                widget.GroupBox(
                    highlight_method='block',
                    background=ColorZ,
                ),
                widget.Prompt(),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.Systray(),
                widget.Clock(format="%a %d-%m-%Y  %I:%M"),
                widget.Spacer(length=10),
                widget.Wlan(format='{essid} {percent:2.0%}'),
                widget.Spacer(length=5),
                widget.Battery(format='{char} {percent:2.0%}'),
                widget.Spacer(length=5),
                widget.StatusNotifier(),
                widget.Spacer(length=5),
                # widget.QuickExit(),
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
            background=ColorZ,
        ),
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        # x11_drag_polling_rate = 60,
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = True
floating_layout = layout.Floating(
    border_focus = ColorA,
    border_normal = ColorC,
    border_width = 2,
    margin = 2,
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = False
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = {
        "type:keyboard": InputConfig(dwt=True, kb_repeat_delay=150, kb_repeat_rate=50, kb_variant="intl", kb_layout="us"),
        "*": InputConfig(tap=True, natural_scroll=True),
    }

wmname = "LG3D"
