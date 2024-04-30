#!/usr/bin/env sh

BROWSER="floorp"
DATABASE_PATH="$HOME/GoogleDrive/Projects/Linux/buku/buku.db"

url=$(buku --db $DATABASE_PATH -p -f4 | fzf -m --reverse --preview "buku --nostdin -p {1}" --preview-window=wrap | cut -f2)

if [ -n "$url" ]; then
    echo "$url" | xargs $BROWSER
fi
