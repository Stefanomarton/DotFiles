#!/usr/bin/env bash
function is_emacs_server_running() {
    emacsclient -e "(message \"\")" >/dev/null 2>&1
}

if is_emacs_server_running; then
        emacsclient -c
  else
      emacs &
      dunstify -a qtile "Emacs server is not running" -t 3000
fi
