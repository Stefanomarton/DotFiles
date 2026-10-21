#!/usr/bin/env bash
# Opener di yazi per gli archivi: estrae in una cartella col nome scelto
# e sposta l'archivio nel cestino (~/.local/share/Trash, spec freedesktop).
set -uo pipefail
shopt -s dotglob nullglob nocasematch

# Cestino secondo la spec freedesktop, ripristinabile da qualsiasi file manager.
# ponytail: sempre il cestino della home, anche per file su altri dischi (mv copia).
trash() {
  local f t n i=1
  f=$(realpath -- "$1")
  t=${XDG_DATA_HOME:-$HOME/.local/share}/Trash
  mkdir -p "$t/files" "$t/info"
  n=$(basename -- "$f")
  until (set -C; : >"$t/info/$n.trashinfo") 2>/dev/null; do
    n="$(basename -- "$f").$((i++))"
  done
  printf '[Trash Info]\nPath=%s\nDeletionDate=%s\n' \
    "$(python3 -c 'import sys, urllib.parse; print(urllib.parse.quote(sys.argv[1]))' "$f")" \
    "$(date +%FT%T)" >"$t/info/$n.trashinfo"
  mv -- "$f" "$t/files/$n" || { rm -f -- "$t/info/$n.trashinfo"; return 1; }
}

fail() { echo "✗ $*"; read -rp "Invio per continuare… " _; }

for a in "$@"; do
  a=$(realpath -- "$a")
  dir=$(dirname -- "$a")
  base=$(basename -- "$a")
  def=${base%.*}
  def=${def%.tar}

  read -rep "Cartella per «$base»: " -i "$def" name || exit 1
  [[ -n $name ]] || continue
  dest="$dir/$name"
  [[ -e $dest ]] && { fail "esiste già: $dest"; continue; }

  tmp="$dir/.extract-$$"
  mkdir -- "$tmp" || { fail "impossibile creare $tmp"; continue; }
  if [[ $base == *.tar || $base == *.tar.* || $base == *.t[gbx]z || $base == *.tbz2 ]]; then
    tar -xf "$a" -C "$tmp"
  else
    "$(command -v 7zz || command -v 7z)" x -y -o"$tmp" -- "$a" >/dev/null
  fi || { rm -rf -- "$tmp"; fail "estrazione fallita, archivio lasciato dov'è"; continue; }

  # Una sola cartella in cima all'archivio: si usa quella, senza doppio livello.
  entries=("$tmp"/*)
  if ((${#entries[@]} == 1)) && [[ -d ${entries[0]} && ! -L ${entries[0]} ]]; then
    mv -- "${entries[0]}" "$dest" && rmdir -- "$tmp"
  else
    mv -- "$tmp" "$dest"
  fi || { fail "rinomina fallita, i file sono in $tmp"; continue; }

  trash "$a" || fail "estratto in $dest, ma l'archivio non è andato nel cestino"
  ya emit reveal "$dest" 2>/dev/null
done
