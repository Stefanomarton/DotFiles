import subprocess
import json


def json_to_list(command):
    output = subprocess.check_output(command.split())
    list_output = json.loads(output.decode("utf-8"))
    return list_output


def get_current_workspace_id():
    output = json_to_list("hyprctl -j activeworkspace")
    return output["id"]


def get_visible_workspace_id():
    monitors = json_to_list("hyprctl -j monitors")
    current_workspace_id = get_current_workspace_id()
    return [
        monitor["activeWorkspace"]["id"]
        for monitor in monitors
        if monitor["activeWorkspace"]["id"] != current_workspace_id
    ]


def get_windows():
    windows = json_to_list("hyprctl -j clients")
    return windows


def get_windows_current_ws():
    windows = get_windows()
    current_workspace_id = get_current_workspace_id()
    return [
        window
        for window in windows
        if window["workspace"]["id"] == current_workspace_id
    ]


def get_windows_visible_ws():
    windows = get_windows()
    visible_workspace_id = get_visible_workspace_id()
    return [
        window
        for window in windows
        if window["workspace"]["id"] in visible_workspace_id
    ]


def get_windows_not_visible_ws():
    windows = get_windows()
    visible_workspace_id = get_visible_workspace_id()
    current_workspace_id = get_current_workspace_id()
    return [
        window
        for window in windows
        if window["workspace"]["id"]
        not in (current_workspace_id, *visible_workspace_id)
    ]


def get_windows_by_class(window_class, window_list):
    return [window for window in window_list if window.get("class") == window_class]


def remove_focused_window(window_list):
    return [window for window in window_list if window.get("focusHistoryID") != 0]


def focus_window(address):
    subprocess.run(["hyprctl", "dispatch", "focuswindow", f"address:{address}"])


def ordered_windows(window_class):
    current_ws_windows = get_windows_by_class(window_class, get_windows_current_ws())
    visible_ws_windows = get_windows_by_class(window_class, get_windows_visible_ws())
    not_visible_ws_windows = get_windows_by_class(
        window_class, get_windows_not_visible_ws()
    )
    return [current_ws_windows + visible_ws_windows + not_visible_ws_windows]


# windows = ordered_windows("emacs")
# for x in range(len(windows)):
#     print(windows[x])
