ewww close-popup ;
hyprctl --batch dispatch workspace $1 &&
eww open ws-popup &&
sleep $2 &&
eww close ws-popup
