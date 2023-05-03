battery="BAT0";

has_battery() {
    if [ -d /sys/class/power_supply/$battery ]; then
        return 0;
    fi
    return 1;
}
get_battery_status() {
    charge="$(get_charge)"
    echo """ "$charge"%"
}

get_charging_status() {
    cat "/sys/class/power_supply/$battery/status"
}

get_charge() {
    cat "/sys/class/power_supply/$battery/capacity"
}

get_datetime() {
    date +"%a %d %b %Y | %I:%M"
}

get_status() {
    battery_status="";
    if $(has_battery); then
        battery_status=" $(get_battery_status) |";
    fi

    echo "${battery_status} $(get_datetime)";
}

while true
do
    xsetroot -name "$(get_status)";
    sleep 1m;
done
