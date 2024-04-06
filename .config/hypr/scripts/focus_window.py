#!/bin/python

import subprocess
import json
import sys


# Get the class from the first argument
if len(sys.argv) < 2:
    print("Usage: python script.py <window_class>")
    sys.exit(1)

window_class = sys.argv[1]


# Focus window with address
def focus_window(address):
    subprocess.run(["hyprctl", "dispatch", "focuswindow", f"address:{address}"])


# Clear index file
def clear_index(window_class):
    with open(f"last_focused_index_{window_class}.txt", "w") as file:
        file.write("-1")


# Get the current active workspace
activeworkspace_output = subprocess.check_output(["hyprctl", "-j", "activeworkspace"])
activeworkspace_json = json.loads(activeworkspace_output.decode("utf-8"))
current_workspace_id = activeworkspace_json["id"]

# Get the current active workspace on other monitors
monitors_output = subprocess.check_output(["hyprctl", "-j", "monitors"])
monitors_json = json.loads(monitors_output.decode("utf-8"))

# Extract the IDs of active workspaces on
# other monitors excluding the current workspace
other_monitor_workspace_ids = [
    monitor["activeWorkspace"]["id"]
    for monitor in monitors_json
    if monitor["activeWorkspace"]["id"] != current_workspace_id
]

# Get the class of the current active window
current_window_output = subprocess.check_output(["hyprctl", "-j", "activewindow"])
current_window_json = json.loads(current_window_output.decode("utf-8"))
current_window_class = current_window_json["class"]

# Check if the current window class is different from the desired class
if current_window_class != window_class:
    # If different, clear the index and create a new list of windows
    clear_index(window_class)

# Get information about all windows
windows_output = subprocess.check_output(["hyprctl", "-j", "clients"])
windows_json = json.loads(windows_output.decode("utf-8"))

# Filter windows by class equal to "emacs"
class_windows = [
    window for window in windows_json if window.get("class") == window_class
]

# Initialize a list to hold all windows
all_windows = []

# Add windows from the current workspace
current_workspace_windows = [
    window
    for window in class_windows
    if window["workspace"]["id"] == current_workspace_id
]
all_windows.extend(current_workspace_windows)

# Add windows from other workspaces
for window in class_windows:
    if window["workspace"]["id"] in other_monitor_workspace_ids:
        all_windows.append(window)

# Add remaining windows
for window in class_windows:
    if window["workspace"]["id"] not in (
        current_workspace_id,
        *other_monitor_workspace_ids,
    ):
        all_windows.append(window)

# Remove current focused window# Sort the windows based on their position in the workspace
all_windows = [window for window in all_windows if window.get("focusHistoryID") != 0]

# Focus on each window in the list one by one
# Read the index of the last focused window from a file
try:
    with open(f"last_focused_index_{window_class}.txt", "r") as file:
        last_focused_index = int(file.read().strip())
except FileNotFoundError:
    last_focused_index = -1

# Calculate the index of the next window to focus
next_index = (last_focused_index + 1) % len(all_windows)

# Focus on the next window in the list
focus_window(all_windows[next_index]["address"])

# Update the index of the last focused window in the file
with open("last_focused_index.txt", "w") as file:
    file.write(str(next_index))
