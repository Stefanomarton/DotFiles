# Hyprland + widget Quickshell

Config Hyprland in Lua (`hyprland.lua` → `modules/*.lua`) e widget Quickshell in `widgets/`,
uno per cartella, ognuno avviato da `modules/autostart.lua` con `qs -p <cartella>`.
Gira su desktop (G9 + Xiaomi) e sui portatili (`laptop`, `tm-laptop`): profili in `modules/vars.lua`.

## Scorciatoie e menu

- `modules/menu.lua`: **una tabella** per le scorciatoie di app/azioni e per il menu which-key.
  Nel menu il tasto è la scorciatoia senza Super (Super+N veloce = Super tenuto, poi N).
  Le sezioni sono solo titoli; l'unico accordo è Layout (Shift+L, poi m/f/s/d).
  Al caricamento scrive `$XDG_RUNTIME_DIR/whichkey.json`, che il widget legge; il widget esegue
  la voce scelta con `hyprctl dispatch 'function() whichkey_run("3.9.1") end'`.
- `modules/keybindings.lua`: solo movimento, workspace, volume, mouse (`navigation_binds`),
  ripetuti anche nel submap `whichkey` perché restino attivi col menu aperto.
  Quindi i tasti del menu non possono essere cifre, j/k/l, slash, frecce, Esc, Backspace.
- Tenere Super: `widgets/whichkey` apre il menu dopo `holdDelay` (Config.qml) e mette Hyprland
  nel submap `whichkey`. Non si apre se il puntatore si è mosso (Super+trascina), se è aperto un
  pannello con la tastiera (`blockers`) o se nel frattempo è scattata una scorciatoia
  (`whichkey-cancel`). Si chiude da solo `autoClose` ms dopo il rilascio di Super.
- Super+Esc = `clean_screen` in menu.lua: nasconde scratchpad (`pypr hide '*'`), popup,
  picker, menu, tofi.

## Widget

| Cartella | Cosa | Note |
|---|---|---|
| `shared/Theme.qml` | colori, font, `textScale` | tutti i `Theme.qml` sono symlink a questo |
| `whichkey` | menu tenendo Super | |
| `notifications` | server notifiche (al posto di dunst) | Super+N/Shift+N/Ctrl+N/Alt+N, Super+D non disturbare |
| `recorder` | badge e popup di `~/.local/bin/meet-rec` | la registrazione è il servizio utente `meet-rec` |
| `volume` | OSD del volume | |
| `workspaces` | picker, exposé, `pickers-close` | |
| `corner` | nome del workspace in ogni monitor | |
| `pomodoro` | non avviato | tema suo, non toccato |

- Schermo dei popup: lista `monitors` in ogni Config.qml, primo collegato vince:
  `["Odyssey G91F", "eDP-1"]` = G9 (desktop o portatile in dock), altrimenti pannello; mai Xiaomi.
- Tema: nero, testo quasi bianco, bordi grigi 2 px, accenti ciano → verde **uguali al bordo
  della finestra attiva** (`modules/appearance.lua`: se cambi uno, cambia l'altro).
  Cornice ciano solo sui pannelli che prendono la tastiera (menu, picker, cronologia).
  Font unico Geist Mono (non ha ⌫: usare la parola). Testo ×1.3 se c'è il G9 (`textScale`).

## Trappole (scoperte a caro prezzo)

- **Funzione-bind + `global`**: se una funzione Lua legata a un tasto fa
  `hl.dispatch(hl.dsp.global(...))`, Hyprland la riesegue al rilascio del tasto
  (Super+A apriva e richiudeva yazi). Dentro le funzioni usare `qs_send(name)`
  (keybindings.lua), che passa da `hyprctl`. `qs_dsp()` in menu.lua solo per bind non-funzione.
- **Global e rilascio**: una bind `global` manda al widget sia `pressed` (tasto giù) sia
  `released` (tasto su); una bind con `{ release = true }` non serve. Il long press di
  Hyprland segue `repeat_delay` (100 ms qui): inutilizzabile come "tieni premuto".
- **Process condiviso in QML**: un `Process` già in esecuzione ignora un nuovo `running = true`;
  per comandi rapidi in sequenza usare `Quickshell.execDetached`.
- **Live reload di Quickshell**: `sed -i` sostituisce il file e il watcher lo perde; modificare
  in place (Edit/python). Le istanze si chiamano `quickshell`, non `qs`: `pkill -f "qs -p …"`
  non le trova e `pgrep -f` col percorso trova anche la propria shell.
- **Test dei tasti**: `wtype` non fa scattare le bind di Hyprland (tastiera virtuale);
  serve un dispositivo uinput (python-evdev). Non farlo mentre Stefano usa il PC.
- **pypr 3.4.6** può morire a un reload della config (perde il socket di hyprctl e fallisce
  anche la notifica d'errore): gira in un ciclo in autostart, log in `~/.cache/pypr.log`.
- **Sessione Hyprland riavviata**: `HYPRLAND_INSTANCE_SIGNATURE` della shell resta vecchio
  ("Couldn't connect"); prendere il più recente in `/run/user/1000/hypr/`.
- **Autopush**: il repo `.dotfiles` ha un hook post-commit che pubblica subito ogni commit.
  Controllare lo staging prima di committare (una cancellazione già in stage finisce dentro).

## Aperto

- Picker dei workspace mostra "No workspaces" anche se `hyprctl workspaces -j` li elenca;
  non viene dal tema. Ultima modifica alla logica della lista: commit 7ba1f815.
- dunst tolto da `~/.nixos/modules/theme.nix`: serve `sudo nixos-rebuild switch` su ogni host.
- `.zshrc` ed `emacs` hanno modifiche non committate di un'altra sessione.
