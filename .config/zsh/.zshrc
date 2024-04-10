#> RC - XDG Spec
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state
export XDG_DESKTOP_DIR="$HOME/"

##>>> XDG - ENV
source "$XDG_CONFIG_HOME"/zsh/xdg-env
export ANDROID_HOME="$XDG_DATA_HOME"/android
export HISTFILE="$XDG_STATE_HOME"/bash/history
export HISTFILE="$XDG_STATE_HOME"/zsh/history
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java export
export NODE_REPL_HISTORY="$XDG_DATA_HOME"/node_repl_history
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export ZDOTDIR="$HOME"/.config/zsh 
export STARSHIP_CONFIG="$XDG_CONFIG_HOME"/starship.toml
export NUGET_PACKAGES="$XDG_CACHE_HOME"/NuGetPackages
export GOPATH="$XDG_DATA_HOME"/go
export BW_SESSION="B6o+4VlAgK/+ne9t+IItf5viLHkYhlsCoS/dAtNu35wUwgcillYuCMkr075RtNQ9vaQRj2dFhU0iHhwrA/McUg=="

export FZF_MARKS_FILE="$XDG_CONFIG_HOME"/fzf-marks/bookmarks
export FZF_MARKS_JUMP=""
export MOZ_ENABLE_WAYLAND=1

export VI_MODE_SET_CURSOR=true
autoload -U colors && colors

# Vim mode
bindkey -v 

bindkey -a '^[[3~' delete-char
bindkey "^[[3~" delete-char
bindkey "^H" backward-delete-char
bindkey "^?" backward-delete-char

# fzm yazi

function fzm-ya() {
    local output
    fzm_output=$(fzm)
    if [[ -n "$fzm_output" ]]; then
        echo "$fzm_output" | xargs -r -d'\n' yazi
    fi
}

bindkey -s "^f" "fzm-ya^M"

# # Keybindings
bindkey "^a" beginning-of-line
bindkey "^e" end-of-line
bindkey '^I' expand-or-complete-prefix

# Fix home and end binding
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line

# #############
# #  Aliases  #
# #############

# # Aliases for zoxide
# \builtin alias cd=__zoxide_z
# \builtin alias cdi=__zoxide_zi
alias a="cd"
alias v="nvim"
alias e="emacsclient -nw"
alias e="emacsclient --alternate-editor=\"\" $*"
alias music="ncmpcpp"
alias lsa="ls -a"
alias gp="git push"
alias ga="git add"
alias gc="git commit"
alias -g ls="ls --color"
alias pmi="~/.local/bin/scripts/pmi"
alias -g pmr="~/.local/bin/scripts/pmr"
alias -g c="clear"
alias -g df="dotfiles"
alias -g V="sudoedit"
alias -s {yml,yaml,lua,c,tex}=nvim #Auto open file with nvim based on extension
alias nvidia-settings="nvidia-settings --config="$XDG_CONFIG_HOME"/nvidia/settings"
alias -g lg="lazygit"
alias -g ppttopdf="libreoffice --headless --invisible --convert-to pdf"

# # Variables
export EDITOR='emacsclient -c'
# export EDITOR='nvim'
export BROWSER='/usr/bin/floorp'
export PAGER='moar -colors "auto" -no-statusbar'
HISTFILE=~/.zsh_history
HISTSIZE=100000000
SAVEHIST=100000000


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

path+=('~/bin')
path+=('/home/stefanom/go/bin')
path+=('/opt/')
path+=('/home/stefanom/.local/share/cargo/bin')
path+=('/home/stefanom/.local/bin')
path+=('/home/stefanom/.local/bin/scripts/')
path+=('/home/stefanom/.local/share/gem/ruby/3.0.0/bin')
export PATH

setopt autocd autopushd 

function ya() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}


bindkey -s '^a' 'ya\n'

source /usr/share/fzf/completion.zsh

##FZF config
export FZF_DEFAULT_OPTS='
--height 40% --layout=reverse --select-1 --border
--color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
--color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
'
export FZF_DEFAULT_COMMAND='rg --files --hidden --glob '"'"'!.git/'"'"

# Edit in the command in vim 
autoload edit-command-line; zle -N edit-command-line
bindkey '^v' edit-command-line

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

source ~/.local/lib/zpico/zpico.zsh
zpico add zsh-users/zsh-syntax-highlighting 
zpico add qoomon/zsh-lazyload
zpico add zsh-users/zsh-autosuggestions
zpico add zsh-users/zsh-completions
zpico add hlissner/zsh-autopair
zpico add urbainvaes/fzf-marks
zpico add IngoMeyer441/zsh-easy-motion
zpico add Aloxaf/fzf-tab
EASY_MOTION_TARGET_KEYS="asdghklqwertyuiopzxcvbnmfj;"
EASY_MOTION_DIM="fg=242"
EASY_MOTION_HIGHLIGHT="fg=196,bold"
EASY_MOTION_HIGHLIGHT_2_FIRST="fg=11,bold"
EASY_MOTION_HIGHLIGHT_2_SECOND="fg=3,bold"
bindkey -M vicmd ' ' vi-easy-motion

# options
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt APPEND_HISTORY            # append to history file

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

## ripgrep-all integration with fzf

rga-fzf() {
	RG_PREFIX="rga --files-with-matches"
	local file
	file="$(
		FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
			fzf --height 100% --sort --preview="[[ ! -z {} ]] && rga -j 12 --pretty --context 5 {q} {}" \
				--phony -q "$1" \
				--bind "change:reload:$RG_PREFIX {q}" \
				--preview-window="70%:wrap"
	)" &&
	echo "opening $file" &&
	xdg-open "$file"
}

# zle -N rga-fzf rga-fzf
# bindkey "^b" rga-fzf
alias jf="rga-fzf"

PROMPT=' %(?.%F{blue}%B𝝍%b.%F{red}?%?)%f%F{white} • %F%f%'
RPROMPT='%B%F{blue}%~%f%b'

source ~/.cache/wal/colors.sh
source /usr/share/fzf/key-bindings.zsh 

eval "$(zoxide init zsh)"

