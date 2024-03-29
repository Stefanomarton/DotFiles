# #!/bin/bash

# # Function to get the name of the active workspace
get_active_workspace_name() {
    hyprctl activeworkspace -j | jq -r '.name'
}

CLASS_NAME=$1

WORKSPACE_ID=$(get_active_workspace_name)


for address in `hyprctl -j clients | jq -r ".[] | select(.class == \"$CLASS_NAME\") | .address"`
do
hyprctl dispatch movetoworkspacesilent $WORKSPACE_ID,address:$address 
done
