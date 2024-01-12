from libqtile import bar, layout, widget, hook, extension, qtile
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen, ScratchPad, DropDown
# from libqtile.layout.xmonad import MonadThreeCol
# from libqtile.extension import WindowList
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

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
ColorFG=(colordict['special']['foreground'])
ColorBG=(colordict['special']['background'])

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtileDesktop/autostart.sh')
    subprocess.Popen([home])

mod = "mod4"
terminal = "kitty"

# Focus previous focused window
previous_focused = []

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


excluded_keychords_keys = {"j", "n", "l", "k", "m"}

keys = [
    # Switch between windows
    Key([mod], "j", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "Slash", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "k", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "l", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "j", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "Slash", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    KeyChord([mod], "BackSpace", [
         
        Key([], "j", lazy.layout.grow(),
            lazy.layout.grow_left()),

        Key([], "Slash", lazy.layout.shrink(),
            lazy.layout.grow_right()),

        Key([], "n", lazy.layout.normalize(),
                lazy.layout.reset().when(layout="monadthreecol")),

        Key([], "l", lazy.layout.grow_up()),

        Key([], "k", lazy.layout.grow_down()),

        Key([], "m", lazy.layout.maximize()),
        ]
        +
        [Key([], i, lazy.ungrab_chord()) for i in "abcdefghijklmnopqrstuvwxyz" if i not in excluded_keychords_keys],
             mode=True,
             name="resizing",
             swallow= True
             ),

    Key([mod], "v", lazy.layout.swap_main(), desc="Swap main"),
    Key([mod], "comma",
        lazy.next_screen(),
        desc='Keyboard focus to next monitor'
        ),
    Key([mod], "e", focus_previous_window()),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),

    # App launching
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "s", lazy.spawn("dmenu_run -l 4", shell=True), desc="Tofi launcher"),
    Key([mod, "shift"], "s", lazy.spawn('flameshot gui', shell=True), desc="Screenshot"),
    Key([mod], "f", lazy.spawn("floorp"), desc="Firefox Browser"),
    Key([mod, "shift"], "Return", lazy.spawn("emacsclient -c", shell=True), desc="emacs"),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),

    Key([mod], "m", toggle_max_monadtall),

    Key([mod], "t", lazy.window.toggle_floating().when(), desc="Toggle floating on the focused window"),

    Key([mod], "b", lazy.hide_show_bar(), desc="Hides the bar"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
]

keys.extend(
    [
        Key(
            [mod],
            "w",
            lazy.group["w11"].toscreen(),
            desc="Switch to group w11 for windows wm",
        ),
    ]
)


groups = [
    # Screen affinity here is used to make
    # sure the groups startup on the right screens
    Group("1",
          label="browser",
          # exclusive=True,
          screen_affinity=0,
          persist=True,
          matches=[Match(wm_class=["floorp"])],
          spawn="floorp"),
    Group("w",
          label="windows",
          screen_affinity=0,
          persist=True,
          matches=[Match(wm_class=["looking-glass-client"])]),
    Group("2",
          label="editor",
          screen_affinity=0,
          persist=True,
          matches=[Match(wm_class=["emacs"])]),
    Group("3",
          screen_affinity=0,
          persist=True,),
    Group("4",
          screen_affinity=0,
          persist=True,),
    Group("5",
          screen_affinity=1,
          persist=True,),
    Group("6",
          screen_affinity=1,
          persist=True,),
    Group("7",
          screen_affinity=1,
          persist=True,
          matches=[Match(wm_class=["ticktick"])],
          ),
    Group("8",
          label="music",
          screen_affinity=2,
          persist=True,
          spawn="spotify-launcher",
          matches=[Match(wm_class=["spotify"])],
          layout="verticaltile",
          layout_opts= {
                        "border_width":0,
                        "border_focus": "#000000"
                        },
          ),
    Group("9",
          label="todo",
          screen_affinity=2,
          persist=True)]

def go_to_group(name: str):
    def _inner(qtile):
        if len(qtile.screens) == 1:
            qtile.groups_map[name].toscreen()
            return

        if name in '1234w': # remember to update for correct groups switching
            qtile.focus_screen(0)
            qtile.groups_map[name].toscreen()

        elif name in '567': # remember to update for correct groups switching
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
    keys.extend([
        Key([mod],
            i.name, lazy.function(go_to_group(i.name)),
                desc="focus group {}".format(i.name)),
        Key([mod, "control"],
            i.name, lazy.function(go_to_group_and_move_window(i.name)),
                desc="move windows to group {}".format(i.name)),
        Key(["mod1"],
            i.name, lazy.window.togroup(i.name),
                desc="move focused window to group {}".format(i.name))
                ])


groups.append(
    ScratchPad('filemanager',
               [
               DropDown(
                   'ranger','zsh -c "kitty -e ranger"', y=0.05, height=0.8, opacity=1, warp_pointer=True
                       ),
               DropDown(
                   'nemo', 'zsh -c "nemo"', y=0.05, height=0.8, opacity=1
                       ),
               DropDown(
                   'pulse', 'zsh -c "kitty -e pulsemixer"', y=0.25, x=0.25, height=0.5, width=0.5, opacity=1
                       ),
               DropDown(
                   'qute', 'qutebrowser', x=0.70, height=1, width=0.3, opacity=1, on_focus_lost_hide=False
               )
               ]
              )
            )

keys.extend([
    Key([mod],'a',lazy.group['filemanager'].dropdown_toggle('ranger')),
    Key([mod],'n',lazy.group['filemanager'].dropdown_toggle('nemo')),
    Key([mod],'p',lazy.group['filemanager'].dropdown_toggle('pulse')),
    Key([mod],'g',lazy.group['filemanager'].dropdown_toggle('qute')),
])

layouts = [
    # layout.Stack(num_stacks=2, border_focus=ColorB, border_normal=ColorZ, border_width=2,),
    # focuslayout.DistractionFree(),
    # layout.Tile(),
    # layout.Bsp(),
    layout.MonadThreeCol(
        align=1,
        # margin=[15,8,15,8],
        margin= 0,
        border_focus=ColorI,
        border_width=4,
        border_normal=ColorBG,
        main_centered=True,
        single_margin=[20,300,20,300],
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
        margin_on_single=[20,300,20,300],
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

screen_wallpaper = "~/.local/share/Wallpapers/Misc/wp9302457-3440x1440-summer-wallpapers.jpg"

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
                highlight_method='block',
                center_aligned= True,
                background=ColorBG,
                foreground="#FFFFFF",
                inactive=ColorG,
                other_current_screen_border=ColorD,
                this_current_screen_border=ColorA,
                other_screen_border=ColorH,
                urgent_border=ColorFG,
                urgent_text=ColorBG,
                hide_unused= True,

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
                foreground_alert=ColorB
            ),
            widget.Spacer(length=10),
            widget.Volume(
                fmt='Vol:{}',
                foreground=ColorFG,
                check_mute_string="[off]",
                volume_app="pavu",
            ),
            widget.Spacer(length=10),
            widget.Wlan(
                format='{essid}:{percent:2.0%}',
                foreground=ColorFG,
            ),
            widget.Spacer(length=5),
            widget.ThermalSensor(
                format='CPU:{temp:.0f}{unit}',
                foreground=ColorFG,
            ),
            widget.Spacer(length=10),
            widget.Systray(
                width=200
            ),
            widget.Spacer(length=10),
            # widget.QuickExit(),
        ],
        20,
        # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
        # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        background=ColorBG,
        lenght=bar.STRETCH,
        ),

        wallpaper= screen_wallpaper,
        wallpaper_mode="fill",
    ),
    Screen(
        wallpaper= screen_wallpaper,
        wallpaper_mode="fill",
    ),
    Screen(
        wallpaper= screen_wallpaper,
        wallpaper_mode="fill",
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
follow_mouse_focus = False
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
auto_fullscreen = True
focus_on_window_activation = "focus"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

wmname = "LG3D"
