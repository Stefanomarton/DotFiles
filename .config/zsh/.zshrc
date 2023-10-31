#> RC - XDG Spec
 export XDG_CONFIG_HOME=$HOME/.config
 export XDG_CACHE_HOME=$HOME/.cache
 export XDG_DATA_HOME=$HOME/.local/share
 export XDG_STATE_HOME=$HOME/.local/state

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
 PATH="/home/stefanomarton/perl5/bin${PATH:+:${PATH}}"; export PATH;
 PERL5LIB="/home/stefanomarton/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
 PERL_LOCAL_LIB_ROOT="/home/stefanomarton/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
 PERL_MB_OPT="--install_base \"/home/stefanomarton/perl5\""; export PERL_MB_OPT;
 PERL_MM_OPT="INSTALL_BASE=/home/stefanomarton/perl5"; export PERL_MM_OPT;
 export BW_SESSION="B6o+4VlAgK/+ne9t+IItf5viLHkYhlsCoS/dAtNu35wUwgcillYuCMkr075RtNQ9vaQRj2dFhU0iHhwrA/McUg=="
 export FZF_MARKS_FILE="$XDG_CONFIG_HOME"/fzf-marks/bookmarks
 export FZF_DEFAULT_COMMAND='rg --files --hidden --glob '"'"'!.git/'"'"
export MOZ_ENABLE_WAYLAND=1

 export VI_MODE_SET_CURSOR=true
 autoload -U colors && colors

 # Vim mode
 bindkey -v

 bindkey -a '^[[3~' delete-char
 bindkey "^[[3~" delete-char
 bindkey "^H" backward-delete-char
 bindkey "^?" backward-delete-char

 # # Keybindings
 bindkey "^a" beginning-of-line
 bindkey "^e" end-of-line
 bindkey '^I' expand-or-complete-prefix

 # Fix home and end binding
 bindkey "^[[H" beginning-of-line
 bindkey "^[[F" end-of-line

#############
#  Aliases  #
#############
alias cp= "cp -r"

# Aliases for zoxide
\builtin alias cd=__zoxide_z
\builtin alias cdi=__zoxide_zi
alias a="cd"
alias v="nvim"
alias e="emacs -nw"
alias music="ncmpcpp"
alias lsa="ls -a"
alias L="ptls -a"
alias gp="git push"
alias ga="git add"
alias gc="git commit"
alias -g ls="ptls"
alias -g ptcp="cp"
alias pmi="~/.local/bin/scripts/pmi"
alias -g pmr="~/.local/bin/scripts/pmr"
alias -g c="clear"
alias -g df="dotfiles"
alias -g V="sudoedit"
alias -s {yml,yaml,lua,c,tex}=nvim #Auto open file with nvim based on extension
alias nvidia-settings="nvidia-settings --config="$XDG_CONFIG_HOME"/nvidia/settings"
alias -g lg="lazygit"

# Better use alias for convience and completion
alias -g rp="lazygit --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@""
alias -g rpg="git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@""

# Variables
export EDITOR='nvim'
export PAGER='moar -colors "auto" -no-statusbar'
HISTFILE=~/.zsh_history
HISTSIZE=100000000
SAVEHIST=100000000


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

path+=('~/bin')
path+=('/home/stefanom/go/bin')
path+=('/opt/')
path+=('/home/stefanom/.cargo/bin')
path+=('/home/stefanom/.local/bin')
path+=('/home/stefanom/.local/bin/scripts/')
path+=('/home/stefanom/.local/share/gem/ruby/3.0.0/bin')
export PATH

setopt autocd autopushd 

# Use ranger to switch diurectories and bind it to ctrl-o
rng () {
    tmp="$(mktemp)"
    ranger --choosedir="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" 
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}

bindkey -s '^u' 'rng\n'
bindkey -s '^f' 'rng --cmd=fzm \n'

### FZF Shortcuts
# The code at the top and the bottom of this file is the same as in completion.zsh.
if 'zmodload' 'zsh/parameter' 2>'/dev/null' && (( ${+options} )); then
    __fzf_key_bindings_options="options=(${(j: :)${(kv)options[@]}})"
else
    () {
        __fzf_key_bindings_options="setopt"
        'local' '__fzf_opt'
        for __fzf_opt in "${(@)${(@f)$(set -o)}%% *}"; do
            if [[ -o "$__fzf_opt" ]]; then
                __fzf_key_bindings_options+=" -o $__fzf_opt"
            else
                __fzf_key_bindings_options+=" +o $__fzf_opt"
            fi
        done
    }
fi

'emulate' 'zsh' '-o' 'no_aliases'

{

    [[ -o interactive ]] || return 0

    # CTRL-T - Paste the selected file path(s) into the command line
    __fsel() {
        local cmd="${FZF_CTRL_T_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
              -o -type f -print \
              -o -type d -print \
              -o -type l -print 2> /dev/null | cut -b3-"}"
        setopt localoptions pipefail no_aliases 2> /dev/null
        local item
        eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read item; do
            echo -n "${(q)item} "
        done
        local ret=$?
        echo
        return $ret
    }

    __fzfcmd() {
        [ -n "$TMUX_PANE" ] && { [ "${FZF_TMUX:-0}" != 0 ] || [ -n "$FZF_TMUX_OPTS" ]; } &&
            echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
    }

    fzf-file-widget() {
        LBUFFER="${LBUFFER}$(__fsel)"
        local ret=$?
        zle reset-prompt
        return $ret
    }
    zle     -N            fzf-file-widget
    bindkey -M emacs '^T' fzf-file-widget
    bindkey -M vicmd '^T' fzf-file-widget
    bindkey -M viins '^T' fzf-file-widget

    # ALT-C - cd into the selected directory
    fzf-cd-widget() {
        local cmd="${FZF_ALT_C_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
              -o -type d -print 2> /dev/null | cut -b3-"}"
        setopt localoptions pipefail no_aliases 2> /dev/null
        local dir="$(eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS" $(__fzfcmd) +m)"
        if [[ -z "$dir" ]]; then
            zle redisplay
            return 0
        fi
        zle push-line # Clear buffer. Auto-restored on next prompt.
        BUFFER="builtin cd -- ${(q)dir}"
        zle accept-line
        local ret=$?
        unset dir # ensure this doesn't end up appearing in prompt expansion
        zle reset-prompt
        return $ret
    }
    zle     -N             fzf-cd-widget
    bindkey -M emacs '^o' fzf-cd-widget
    bindkey -M vicmd '^o' fzf-cd-widget
    bindkey -M viins '^o' fzf-cd-widget

    # CTRL-R - Paste the selected command from history into the command line
    fzf-history-widget() {
        local selected num
        setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
        selected=( $(fc -rl 1 | awk '{ cmd=$0; sub(/^[ \t]*[0-9]+\**[ \t]+/, "", cmd); if (!seen[cmd]++) print $0 }' |
                         FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort,ctrl-z:ignore $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" $(__fzfcmd)) )
        local ret=$?
        if [ -n "$selected" ]; then
            num=$selected[1]
            if [ -n "$num" ]; then
                zle vi-fetch-history -n $num
            fi
        fi
        zle reset-prompt
        return $ret
    }
    zle     -N            fzf-history-widget
    bindkey -M emacs '^R' fzf-history-widget
    bindkey -M vicmd '^R' fzf-history-widget
    bindkey -M viins '^R' fzf-history-widget

} always {
    eval $__fzf_key_bindings_options
    'unset' '__fzf_key_bindings_options'
}

source /usr/share/fzf/completion.zsh

##FZF config
export FZF_DEFAULT_OPTS='
--height 40% --layout=reverse --select-1 --border
--color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
--color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
'

## Edit in the command in vim 
autoload edit-command-line; zle -N edit-command-line
bindkey '^v' edit-command-line

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

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

eval "$(zoxide init zsh)"

source ~/.cache/wal/colors.sh
