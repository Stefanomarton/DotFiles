import json
import os
import re
import subprocess

import xcffib.xproto
from libqtile import bar, hook, layout, qtile, widget, extension
from libqtile.config import (
    Click,
    Drag,
    DropDown,
    Group,
    Key,
    KeyChord,
    Match,
    ScratchPad,
    Screen,
)
from libqtile.lazy import lazy

from qtile_extras.popup.toolkit import PopupRelativeLayout, PopupText

# from libqtile.utils import guess_terminal

# import socket
# import distutils.spawn


def show_group_name(qtile, name, hide):
    # group_name = qtile.current_group.label
    name = name.upper()
    controls = [
        PopupText(
            can_focus=False,
            text=name,
            pos_x=0.25,
            pos_y=0.25,
            width=0.5,
            height=0.5,
            h_align="center",
            fontsize=20,
            font="JuliaMono",
            wrap=True,
        ),
    ]

    global layout
    layout = PopupRelativeLayout(
        qtile,
        width=250,
        height=50,
        controls=controls,
        background=ColorBG,
        initial_focus=None,
        hide_on_timeout=hide,
    )

    # show popup
    layout.show(centered=True)


# Pywal Colors
colors = os.path.expanduser("~/.cache/wal/colors.json")
colordict = json.load(open(colors))
ColorZ = colordict["colors"]["color0"]
ColorA = colordict["colors"]["color1"]
ColorB = colordict["colors"]["color2"]
ColorC = colordict["colors"]["color3"]
ColorD = colordict["colors"]["color4"]
ColorE = colordict["colors"]["color5"]
ColorF = colordict["colors"]["color6"]
ColorG = colordict["colors"]["color7"]
ColorH = colordict["colors"]["color8"]
ColorI = colordict["colors"]["color9"]
ColorFG = colordict["special"]["foreground"]
ColorBG = colordict["special"]["background"]


# picom conflict
@hook.subscribe.startup
def autostart():
    qtile.cmd_hide_show_bar("bottom")


@hook.subscribe.startup_once
def autostart_once():
    home = os.path.expanduser("~/.config/qtileDesktop/autostart.sh")
    subprocess.Popen([home])


@hook.subscribe.setgroup
def send_notification():
    show_group_name(qtile, qtile.current_group.label, 0.5)
    # group_name = qtile.current_group.label
    # # group_name = qtile.current_group.name
    # notification_text = group_name.upper()
    # # Use dunstify to send a notification
    # # qtile.cmd_spawn(f"dunstify -a qtile {notification_text} -r 123 -t 600", shell=True)


@hook.subscribe.enter_chord
def enter_chord(chord_name):
    chord_name = chord_name.upper()
    # qtile.cmd_spawn(f"dunstify -a qtile {chord_name} -r 124", shell=True)
    show_group_name(qtile, chord_name.upper(), 0)


@hook.subscribe.leave_chord
def leave_chord():
    # qtile.cmd_spawn("dunstify -C 124", shell=True)
    # PopupRelativeLayout(qtile).kill

    layout.kill()


@hook.subscribe.client_focus
def client_focused(window):
    global previous_focused
    if len(previous_focused) < 2:
        previous_focused.append(window)
    elif previous_focused[1] != window:
        previous_focused[0] = previous_focused[1]
        previous_focused[1] = window
    # logger.info(f"FOCUSED {window}, {previous_focused}")


@lazy.function
def focus_previous_window(qtile):
    global previous_focused
    if len(previous_focused) == 2:
        group = previous_focused[0].group
        qtile.current_screen.set_group(group)
        # logger.info(f"FOCUS PREVIOUS {previous_focused[0]}")
        group.focus(previous_focused[0])


@lazy.group.function
def toggle_max_monadtall(group):
    layout = group.layout.name
    if layout == "max":
        group.setlayout("monadthreecol")
    elif layout == "monadthreecol":
        group.setlayout("max")


# taken from
# https://github.com/qtile/qtile-examples/blob/master/roger/config.py#L34
# and adapted
def pull_window_group_here(**kwargs):
    """Switch to the *next* window matched by match_window_re with the given
    **kwargs
    If you have multiple windows matching the args, switch_to will
    cycle through them.
    (Those semantics are similar to the fvwm Next commands with
    patterns)
    """

    def callback(qtile):
        windows = windows_matching_shuffle(qtile, **kwargs)
        if windows:
            window = windows[0]
            qtile.current_screen.set_group(window.group)
            window.group.focus(window, False)
            window.focus(window, False)

    return lazy.function(callback)


def window_switch_to_screen_or_pull_group(**kwargs):
    """If the group of the window matched by match_window_re with the
    given **kwargs is in a visible on another screen, switch to the
    screen, otherwise pull the group to the current screen
    """

    def callback(qtile):
        windows = windows_matching_shuffle(qtile, **kwargs)
        if windows:
            window = windows[0]
            if window.group != qtile.current_group:
                if window.group.screen:
                    qtile.cmd_to_screen(window.group.screen.index)
                qtile.current_screen.set_group(window.group)
            window.group.focus(window, False)

    return lazy.function(callback)


switch_window = window_switch_to_screen_or_pull_group


def pull_window_here(**kwargs):
    """pull the matched window to the current group and focus it
    matching behaviour is the same as in switch_to
    """

    def callback(qtile):
        windows = windows_matching_shuffle(qtile, **kwargs)
        if windows:
            window = windows[0]
            window.togroup(qtile.current_group.name)
            qtile.current_group.focus(window, False)

    return lazy.function(callback)


def windows_matching_shuffle(qtile, **kwargs):
    """return a list of windows matching window_match_re with **kwargs,
    ordered so that the current Window (if it matches) comes last
    """
    windows = sorted(
        [
            w
            for w in qtile.windows_map.values()
            if w.group and window_match_re(w, **kwargs)
        ],
        key=lambda ww: ww.window.wid,
    )
    idx = 0
    if qtile.current_window is not None:
        try:
            idx = windows.index(qtile.current_window)
            idx += 1
        except ValueError:
            pass
    if idx >= len(windows):
        idx = 0
    return windows[idx:] + windows[:idx]


def window_match_re(window, wmname=None, wmclass=None, role=None):
    """
    match windows by name/title, class or role, by regular expressions
    Multiple conditions will be OR'ed together
    """

    if not (wmname or wmclass or role):
        raise TypeError("at least one of name, wmclass or role must be specified")
    ret = False
    if wmname:
        ret = ret or re.match(wmname, window.name)
    try:
        if wmclass:
            cls = window.window.get_wm_class()
            if cls:
                for v in cls:
                    ret = ret or re.match(wmclass, v)
        if role:
            rol = window.window.get_wm_window_role()
            if rol:
                ret = ret or re.match(role, rol)
    except (xcffib.xproto.WindowError, xcffib.xproto.AccessError):
        return False
    return ret


# def create_keychord_list(excluded_keys):
#     # mapping = [
#     #     Key([], lazy.ungrab_chord())
#     # ]
#     return [Key([], key, lazy.ungrab_chord()) for key in excluded_keys] for
# i in "abcdefghijklmnopqrstuvwxyz" if i not in excluded_keychords_keys

# excluded_keychords_keys = {"j", "n", "l", "k", "m"}
# key_list = create_keychord_list(excluded_keychords_keys)


def modifier_window_commands(match, spawn, *keys, shell=False):
    spawn_func = (
        lazy.spawn(f"zsh -c '{spawn}'", shell=True) if shell else lazy.spawn(spawn)
    )

    # Use switch_window by default (just mod)
    # Use pull_window_here with additional ctrl
    # spawn new window with additional shift
    # Use pull_window_group_here with additional shift (mod, "shift", "control")
    mapping = (
        ([mod], switch_window(**match)),
        ([mod, "control"], pull_window_here(**match)),
        ([mod, "shift"], spawn_func),
        ([mod, "shift", "control"], pull_window_group_here(**match)),
    )
    return [Key(mods, key, command) for mods, command in mapping for key in keys]


mod = "mod4"
terminal = "kitty"

keys = (
    # Switch between windows
    modifier_window_commands(
        {"wmclass": "Emacs"}, "~/.config/qtileDesktop/launch_emacs.sh", "e", shell=True
    )
    + modifier_window_commands(
        {"wmclass": "floorp"},
        "floorp",
        "f",
    )
    + modifier_window_commands({"wmclass": "spotify"}, "spotify-launcher", "r")
    + modifier_window_commands(
        {"wmname": ".*pdf$", "wmclass": "(Zathura|Evince|Acroread|Xpdf|Okular)"},
        # {"wmname": ".*pdf$", "wmclass": "(zathura|Evince|Acroread|Xpdf|Okular)"},
        "zathura",
        "p",
    )
    + modifier_window_commands(
        {"wmname": "ticktick", "wmclass": "ticktick"}, "ticktick", "t"
    )
    + modifier_window_commands({"wmname": "mpv", "wmclass": "mpv"}, "mpv", "z")
    + [
        Key([mod], "j", lazy.layout.grow(), lazy.layout.grow_left()),
        Key([mod], "Slash", lazy.layout.shrink(), lazy.layout.grow_right()),
        Key([mod], "l", lazy.layout.previous(), desc="Move focus down"),
        Key([mod], "k", lazy.layout.next(), desc="Move focus up"),
        Key(
            [mod, "shift"],
            "j",
            lazy.layout.shuffle_left(),
            desc="Move window to the left",
        ),
        Key(
            [mod, "shift"],
            "Slash",
            lazy.layout.shuffle_right(),
            desc="Move window to the right",
        ),
        Key([mod, "shift"], "k", lazy.layout.shuffle_down(), desc="Move window down"),
        Key([mod, "shift"], "l", lazy.layout.shuffle_up(), desc="Move window up"),
        # Grow windows. If current window is on the edge of screen and direction
        # will be to screen edge - window would shrink.
        KeyChord(
            [mod],
            "BackSpace",
            [
                Key([], "j", lazy.layout.grow(), lazy.layout.grow_left()),
                Key([], "Slash", lazy.layout.shrink(), lazy.layout.grow_right()),
                Key(
                    [],
                    "n",
                    lazy.layout.normalize(),
                    lazy.layout.reset().when(layout="monadthreecol"),
                ),
                Key([], "l", lazy.layout.grow_up()),
                Key([], "k", lazy.layout.grow_down()),
                Key([], "m", lazy.layout.maximize()),
                Key([], "q", lazy.ungrab_chord()),
            ],
            mode=True,
            name="resizing",
            swallow=True,
        ),
        Key([mod], "v", lazy.layout.swap_main(), desc="Swap main"),
        Key([mod], "comma", lazy.next_screen(), desc="Keyboard focus to next monitor"),
        Key([mod], "o", focus_previous_window()),
        Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
        # Key(
        #     ],
        #     "d",
        #     lazy.layout.toggle_split(),
        #     desc="Toggle between split and unsplit sides of stack",
        # ),
        Key([mod], "d", lazy.spawn("dunstctl action", shell=True), desc="dunstctl"),
        # App launching
        Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
        Key([mod], "s", lazy.spawn("dmenu_run -l 4", shell=True), desc="Tofi launcher"),
        Key(
            [mod, "shift"],
            "s",
            lazy.spawn("flameshot gui", shell=True),
            desc="Screenshot",
        ),
        # Key([mod, "shift"], "Return", lazy.spawn("emacsclient -c", shell=True), desc="emacs"),
        # Toggle between different layouts as defined below
        Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
        Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
        Key([mod], "m", toggle_max_monadtall),
        Key(
            [mod],
            "i",
            lazy.window.toggle_floating().when(),
            desc="Toggle floating on the focused window",
        ),
        Key([mod], "b", lazy.hide_show_bar(), desc="Hides the bar"),
        Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
        Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
        Key(
            [mod, "shift"],
            "v",
            lazy.run_extension(extension.WindowList()),
        ),
    ]
)

# keys.extend(
#     [
#         Key(
#             [mod],
#             "w",
#             lazy.group["w11"].toscreen(),
#             desc="Switch to group w11 for windows wm",
#         ),
#     ]
# )


groups = [
    # Screen affinity here is used to make
    # sure the groups startup on the right screens
    Group(
        "1",
        label="browser",
        # exclusive=True,
        screen_affinity=0,
        persist=True,
        # matches=[Match(wm_class=["floorp"])],
        spawn="floorp",
    ),
    Group(
        "w",
        label="windows",
        exclusive=True,
        screen_affinity=0,
        persist=True,
        matches=[Match(wm_class=["looking-glass-client"])],
        layout="max",
        spawn="looking-glass-client",
    ),
    Group(
        "2",
        label="editor",
        screen_affinity=0,
        persist=True,
        # matches=[Match(wm_class=["emacs"])],
        spawn="emacsclient -c",
    ),
    Group(
        "3",
        label="anki",
        screen_affinity=0,
        persist=True,
        spawn="anki",
    ),
    Group(
        "4",
        screen_affinity=0,
        persist=True,
        spawn="ticktick",
    ),
    Group(
        "5",
        screen_affinity=1,
        persist=True,
    ),
    Group(
        "6",
        screen_affinity=1,
        persist=True,
    ),
    Group(
        "7",
        screen_affinity=1,
        persist=True,
        # matches=[Match(wm_class=["ticktick"])],
    ),
    Group(
        "8",
        label="music",
        screen_affinity=2,
        persist=True,
        spawn="spotify-launcher",
        matches=[Match(wm_class=["spotify"])],
        layout="verticaltile",
        layout_opts={"border_width": 0, "border_focus": "#000000"},
    ),
    Group("9", label="todo", screen_affinity=2, persist=True),
]


def go_to_group(name: str):
    def _inner(qtile):
        if len(qtile.screens) == 1:
            qtile.groups_map[name].toscreen()
            return

        if name in "1234w":  # remember to update for correct groups switching
            qtile.focus_screen(0)
            qtile.groups_map[name].toscreen()

        elif name in "567":  # remember to update for correct groups switching
            qtile.focus_screen(1)
            qtile.groups_map[name].toscreen()

        else:
            qtile.focus_screen(2)
            qtile.groups_map[name].toscreen()

    return _inner


def go_to_group_and_move_window(name: str):
    def _inner(qtile):
        if len(qtile.screens) == 1:
            qtile.current_window.togroup(name, switch_group=True)
            return

        if name in "1234w":
            qtile.current_window.togroup(name, switch_group=False)
            qtile.focus_screen(0)
            qtile.groups_map[name].toscreen()

        elif name in "567":
            qtile.current_window.togroup(name, switch_group=False)
            qtile.focus_screen(1)
            qtile.groups_map[name].toscreen()

        else:
            qtile.current_window.togroup(name, switch_group=False)
            qtile.focus_screen(2)
            qtile.groups_map[name].toscreen()

    return _inner


for i in groups:
    keys.extend(
        [
            Key(
                [mod],
                i.name,
                lazy.function(go_to_group(i.name)),
                desc="focus group {}".format(i.name),
            ),
            Key(
                [mod, "control"],
                i.name,
                lazy.function(go_to_group_and_move_window(i.name)),
                desc="move windows to group {}".format(i.name),
            ),
            Key(
                ["mod1"],
                i.name,
                lazy.window.togroup(i.name),
                desc="move focused window to group {}".format(i.name),
            ),
        ]
    )

groups.append(
    ScratchPad(
        "filemanager",
        [
            DropDown(
                "yazi",
                'zsh -c "kitty -e yazi"',
                y=0.05,
                height=0.8,
                opacity=1,
                warp_pointer=True,
            ),
            DropDown("nemo", 'zsh -c "nemo"', y=0.05, height=0.8, opacity=1),
            DropDown(
                "pulse",
                'zsh -c "kitty -e pulsemixer"',
                y=0.25,
                x=0.25,
                height=0.5,
                width=0.5,
                opacity=1,
            ),
            DropDown(
                "qalc",
                'zsh -c "kitty -e qalc"',
                y=0.75,
                x=0.4,
                height=0.23,
                width=0.2,
                opacity=1,
                on_focus_lost_hide=False,
            ),
        ],
    )
)

keys.extend(
    [
        Key([mod], "a", lazy.group["filemanager"].dropdown_toggle("yazi")),
        Key([mod], "n", lazy.group["filemanager"].dropdown_toggle("nemo")),
        Key([mod], "h", lazy.group["filemanager"].dropdown_toggle("pulse")),
        # Key([mod],'g',lazy.group['filemanager'].dropdown_toggle('qute')),
        Key([mod], "c", lazy.group["filemanager"].dropdown_toggle("qalc")),
    ]
)

layouts = [
    # layout.Stack(num_stacks=2, border_focus=ColorB, border_normal=ColorZ, border_width=2,),
    # focuslayout.DistractionFree(),
    # layout.Tile(),
    # layout.Bsp(),
    layout.MonadThreeCol(
        align=1,
        # margin=[15,8,15,8],
        margin=0,
        border_focus=ColorI,
        border_width=4,
        border_normal=ColorBG,
        main_centered=True,
        single_margin=[20, 300, 20, 300],
        new_client_position="bottom",
        single_border_width=0,
        ratio=0.6,
        max_ratio=0.55,
        min_ratio=0.35,
    ),
    # layout.MonadTall(),
    # layout.MonadWide(border_focus=ColorB, border_width=3, border_normal=ColorFG),
    layout.Columns(
        # margin=[15,10,15,10],
        border_focus=ColorI,
        border_normal=ColorZ,
        border_focus_stack=ColorI,
        border_normal_stack=ColorBG,
        margin_on_single=[20, 300, 20, 300],
        border_width=4,
        num_columns=3,
        split=False,
        wrap_focus_columns=True,
        wrap_focus_rows=True,
        wrap_focus_stacks=True,
    ),
    layout.Max(),
    # layout.RatioTile(),
    # layout.TreeTab(),
    layout.VerticalTile(
        border_width=0,
    ),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="JuliaMono",
    fontsize=10,
    padding=5,
)
extension_defaults = widget_defaults.copy()

screen_wallpaper = (
    "~/.local/share/Wallpapers/Misc/wp9302457-3440x1440-summer-wallpapers.jpg"
)

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.Spacer(length=5),
                widget.CurrentLayout(
                    foreground=ColorFG,
                ),
                widget.Spacer(length=5),
                widget.GroupBox(
                    highlight_method="block",
                    center_aligned=True,
                    background=ColorBG,
                    foreground="#FFFFFF",
                    inactive=ColorG,
                    other_current_screen_border=ColorD,
                    this_current_screen_border=ColorA,
                    other_screen_border=ColorH,
                    urgent_border=ColorFG,
                    urgent_text=ColorBG,
                    hide_unused=True,
                ),
                widget.Spacer(length=bar.STRETCH),
                # widget.Prompt(),
                # widget.WindowName(),
                widget.TaskList(
                    highlight_method="block",
                    foreground=ColorFG,
                    border=ColorA,
                    title_width_method="uniform",
                    margin=0,
                ),
                # widget.Chord(
                #     chords_colors={
                #         "launch": ("#ff0000", "#ffffff"),
                #     },
                #     name_transform=lambda name: name.upper(),
                # ),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.Systray(),
                widget.Spacer(length=bar.STRETCH),
                # widget.Visualizer(
                #     bar_colour=ColorB,
                #     width=200,
                #     bars=16,
                #     channels="stereo",
                #     bar_height=30,
                # ),
                # widget.Spacer(length=10),
                widget.Clock(
                    format="%a %d-%m-%Y  %I:%M:%S",
                    foreground=ColorFG,
                    foreground_alert=ColorB,
                ),
                widget.Spacer(length=10),
                widget.Volume(
                    fmt="Vol:{}",
                    foreground=ColorFG,
                    check_mute_string="[off]",
                    volume_app="pavu",
                ),
                widget.Spacer(length=10),
                widget.Wlan(
                    format="{essid}:{percent:2.0%}",
                    foreground=ColorFG,
                ),
                widget.Spacer(length=5),
                widget.ThermalSensor(
                    format="CPU:{temp:.0f}{unit}",
                    foreground=ColorFG,
                ),
                widget.Spacer(length=10),
                widget.Systray(width=200),
                widget.Spacer(length=10),
                # widget.QuickExit(),
            ],
            20,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
            background=ColorBG,
            lenght=bar.STRETCH,
        ),
        wallpaper=screen_wallpaper,
        wallpaper_mode="fill",
    ),
    Screen(
        wallpaper=screen_wallpaper,
        wallpaper_mode="fill",
    ),
    Screen(
        wallpaper=screen_wallpaper,
        wallpaper_mode="fill",
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = True
floating_layout = layout.Floating(
    border_focus=ColorA,
    border_normal=ColorC,
    border_width=2,
    margin=2,
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ],
)
auto_fullscreen = True
focus_on_window_activation = "focus"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

wmname = "LG3D"
