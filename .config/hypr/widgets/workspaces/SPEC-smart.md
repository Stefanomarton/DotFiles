# Smart picker unico — specifica (2026-10-23)

Un solo picker (Super+S) al posto di Workspace (Super+P), Finestre (Super+W),
Smart (Super+R), Sposta finestra (Super+M) ed Esegui comando (tofi-run, Super+S).

## Layout

```
┌ Smart ───────────────────────────┬────────────────────────┐
│ > ricerca_                       │  anteprima live del    │
├──────────────────────────────────┤  workspace della riga  │
│ ● Firefox — GitHub        win    │  evidenziata, con la   │
│ ○ Firefox Developer       app    │  finestra evidenziata  │
│ ■ web                     ws     │  contornata            │
├──────────────────────────────────┴────────────────────────┤
│ Enter apri · Ctrl+Enter qui · Alt+Enter altro monitor · … │
└───────────────────────────────────────────────────────────┘
```

- Lista unica (niente colonne, niente ←→). Riga: icona, titolo, sottotitolo, tag del tipo
  (`ws` `win` `app` `cmd`) col colore del tipo.
- Anteprima a destra; se lo schermo del picker è più stretto di lista + anteprima, va sotto.
  Riga di workspace → mappa di quel workspace; riga di finestra → mappa del suo workspace,
  finestra contornata; app/comando → anteprima vuota (lo spazio resta, niente salti).
- Una riga di suggerimenti, che cambia con la modalità (normale, `m `, `> `, nome workspace).
- Sotto la ricerca, i selettori `all · w · p · a · m · >`: il prefisso scritto (`w ` …) sparisce dal
  testo e accende il suo selettore (anche col clic); Backspace a campo vuoto torna a `all`.

## Risultati e ordine

Punteggio unico (esatto > prefisso > contiene > fuzzy), poi a parità: workspace > finestra > app.
Enter attiva sempre la riga evidenziata, che all'inizio è la prima.

| Ricerca | Contenuto |
|---|---|
| vuota | finestre per ultimo uso (`focusHistoryID`), la corrente esclusa; poi workspace di progetto, il corrente esclusi |
| testo | workspace, finestre, app senza finestra aperta (DesktopEntries), mescolati per punteggio |
| `w ` / `p ` / `a ` | solo workspace / finestre / app |
| `m ` | solo workspace: sposta la finestra che era attiva all'apertura |
| `> ` | una riga `cmd`: Enter esegue il testo come comando di shell |

Esclusioni invariate: workspace `1`–`10` e `special:*` (`Config.pickerExcludedWorkspaces`),
finestre nei workspace di `Config.windowPickerExcludedWorkspaces`.
Se il testo non corrisponde a nessun workspace (e non c'è prefisso, o c'è `w `), l'ultima riga è
"Crea workspace «testo»".

## Tasti

| Tasto | Finestra | Workspace | App | `m ` |
|---|---|---|---|---|
| Enter | focus | vai | avvia | sposta, resta qui |
| Ctrl+Enter | porta nel workspace corrente | porta sul monitor corrente | — | — |
| Alt+Enter | — | sposta sull'altro monitor | — | — |
| Shift+Enter | — | — | — | sposta e segui |
| Tab | seleziona (multi) | — | — | — |
| Alt+Backspace | chiude (o le selezionate) | chiude tutte le sue finestre | — | — |
| ↑↓ / Esc | naviga / chiude, tornando al workspace di partenza | | | |

Con finestre selezionate (Tab): Enter su un workspace le sposta lì, Ctrl+Enter le porta qui.

## Dati e prestazioni

- Niente processi all'apertura: `Hyprland.toplevels`, `Hyprland.workspaces`, `Hyprland.monitors`
  (Quickshell.Hyprland, aggiornati via eventi) + `refreshToplevels()` all'apertura per
  `lastIpcObject` (classe, geometria, `focusHistoryID`). App da `DesktopEntries`.
- Anteprima: `toplevel.wayland` diretto per `ScreencopyView` (niente ricerca per appId/titolo), un
  fotogramma quando la tessera compare (`live: false`): la cattura continua a 144 Hz costava ~7% CPU
  qui, ~15% in Hyprland e ~10% GPU finché il picker restava aperto.
- Nessuna animazione d'apertura: layer rule `no_anim` (senza, `layersIn` eredita 800 ms da `global`).
- Azioni: un solo `hyprctl dispatch`/`eval` per azione (come oggi); comandi `> ` e app con
  `Quickshell.execDetached` / `DesktopEntry.execute()`.
- Ordinamento in `rank.js` (`.pragma library`, funzioni pure), con `rank_test.js` eseguibile
  con `node` (assert).

## File

- Riscritti: `SmartPicker.qml`, `SmartPickerManager.qml`. Nuovi: `rank.js`, `rank_test.js`.
- Cancellati: `WorkspacePicker.qml`, `WindowPicker.qml`, `WindowManager.qml`,
  `MoveWindowPicker.qml`, `MoveWindowManager.qml`.
- `WorkspaceManager.qml` resta (lo usa l'Exposé), senza toccarlo.
- `shell.qml`: via gli shortcut `workspaces`, `windows`, `movewindow`; `pickers-close` aggiornato.
- `modules/menu.lua`: via Super+P/W/R/M e tofi-run; Super+S = `qs("smart")`.
- `modules/rules.lua`, `widgets/whichkey/Config.qml`: via i namespace dei picker tolti.
- `AGENTS.md`, `PROGRESS.md`, `CLAUDE.md`: aggiornati.

## Verifica

- `node rank_test.js` passa.
- `qs log -p widgets/workspaces` senza errori dopo il reload; `hyprctl configerrors` vuoto.
- Tasti provati da Stefano (simularli con uinput solo a PC libero).

## Fuori scope

Exposé e il suo `WorkspaceManager`; MRU dei workspace; titoli nelle tessere dell'anteprima.
