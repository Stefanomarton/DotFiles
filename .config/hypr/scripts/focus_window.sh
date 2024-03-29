#!/bin/bash

class=$1

# File to store the index of the last focused address
index_file="/tmp/last_focused_address_index"

# File to store the address of the last focused window
last_focused_window_file="/tmp/last_focused_window_address"

# Create tmpfile
touch "$index_file"
touch "$last_focused_window_file"

# Function to delete the index file
delete_index_file() {
    rm -f "$index_file"
}

# Function to get the name of the active workspace
get_active_workspace_name() {
    hyprctl activeworkspace -j | jq -r '.name'
}

# Function to fetch addresses matching the current workspace
fetch_addresses_current_workspace() {
    local active_workspace_name="$1"
    hyprctl -j clients | jq -r --arg active_workspace_name "$active_workspace_name" --arg class "$class" '.[] | select(.class == $class and .workspace.name == $active_workspace_name) | .address'
}


# Function to fetch active workspace on monitors
fetch_visible_workspace_names() {
    local active_workspace_name="$1"
    hyprctl -j monitors | jq -r --arg active_workspace_name "$active_workspace_name" '.[] | select(.activeWorkspace.name and .activeWorkspace.name != $active_workspace_name) | .activeWorkspace.name'
}

#Function to fetch addresses matching the visible workspaces
fetch_addresses_visible_workspaces() {
    local active_workspace_name="$1"
    local addresses=()
    # Fetch visible workspace names
    mapfile -t visible_workspace_names < <(fetch_visible_workspace_names "$active_workspace_name")
    # Loop over each visible workspace name
    for workspace_name in "${visible_workspace_names[@]}"; do
        # Fetch addresses for the current visible workspace
        mapfile -t workspace_addresses < <(hyprctl -j clients | jq -r --arg workspace_name "$workspace_name" --arg class "$class" '.[] | select(.class == $class and .workspace.name == $workspace_name) | .address')
        # Add the fetched addresses to the addresses array
        addresses+=("${workspace_addresses[@]}")
    done
    # Return the addresses array
    echo "${addresses[@]}"
}


# Function to fetch all addresses except the current workspace
fetch_addresses_other_workspaces() {
    local active_workspace_name="$1"
    hyprctl -j clients | jq -r --arg active_workspace_name "$active_workspace_name" --arg class "$class" '.[] | select(.class == $class and .workspace.name != $active_workspace_name) | .address'
}

fetch_address_focus_history_id() {
    hyprctl clients -j | jq -r ".[] | [.address, .focusHistoryID] | @tsv"
}

# Read the last focused address index
last_focused_index=$(read_last_focused_address_index)

# If the last focused index is zero, delete the file
if [ "$last_focused_index" -eq 0 ]; then
    rm -f "$index_file"
fi

# Get the name of the active workspace
active_workspace_name=$(get_active_workspace_name)

# Fetch addresses matching the current workspace
mapfile -t matching_addresses_current_workspace < <(fetch_addresses_current_workspace "$active_workspace_name")

# Fetch addresses matching the visible workspaces
mapfile -t matching_addresses_visible_workspaces < <(fetch_addresses_visible_workspaces "$active_workspace_name")

# Fetch addresses from other workspaces if no addresses found in the current workspace
mapfile -t matching_addresses_other_workspaces < <(fetch_addresses_other_workspaces "$active_workspace_name")

# Combine addresses from the current workspace, visible workspaces, and other workspaces
# addresses=("${matching_addresses_current_workspace[@]}" "${matching_addresses_visible_workspaces=[@]}" "${matching_addresses_other_workspaces[@]}")
addresses=("${matching_addresses_current_workspace[@]}" "${matching_addresses_other_workspaces[@]}")


# Fetch the addresses and focusHistoryID of each window
mapfile -t window_info < <(fetch_address_focus_history_id)

# Get the address of the currently focused window
focused_window_address=$(printf '%s\n' "${window_info[@]}" | awk '$2 == 0 { print $1; exit }')

# Function to read the last focused address index
read_last_focused_address_index() {
    if [ -f "$index_file" ]; then
        index=$(<"$index_file")
        echo "$index"
    else
        echo "0"
    fi
}

# Function to write the last focused address index
write_last_focused_address_index() {
    echo "$1" > "$index_file"
}

# Read the last focused address index
last_focused_index=$(read_last_focused_address_index)

# Calculate the index of the next address to focus on
next_index=$(( (last_focused_index + 1) % ${#addresses[@]} ))

# Get the next address to focus on
next_address=${addresses[$next_index]}

# Check if the focusHistoryID of the next window is 0, if so, skip it
next_window_focus_history_id=$(printf '%s\n' "${window_info[@]}" | awk -v idx="$next_index" 'NR == (idx+1) { print $2 }')


# Check if the next address is the same as the focused window address, if so, skip to the next
if [ "$next_address" == "$focused_window_address" ]; then
    # Move to the next index
    next_index=$(( (next_index + 1) % ${#addresses[@]} ))
    next_address=${addresses[$next_index]}
fi

# Write the next focused address index
write_last_focused_address_index "$next_index"


# Execute hyprctl focuswindow with the next address
hyprctl dispatch focuswindow address:"$next_address"
