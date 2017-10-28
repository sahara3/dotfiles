## ~/.zshrc: executed by zsh for each shell.

# key bindings
bindkey -e

# options
setopt auto_pushd chase_dots
setopt auto_list auto_remove_slash no_list_beep
setopt no_extended_glob
setopt append_history hist_ignore_all_dups hist_ignore_dups hist_verify
setopt correct correct_all no_flow_control ignore_eof print_eight_bit
setopt check_jobs no_notify
setopt no_prompt_cr prompt_subst
setopt pushd_ignore_dups
setopt auto_remove_slash
setopt no_flow_control
setopt monitor
setopt zle

HISTFILE=${HOME}/.zsh_history
HISTSIZE=1024
SAVEHIST=4096

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# completion
autoload -Uz compinit ; compinit -u

zstyle ':completion:*' use-cache true
zstyle ':completion:*:default' menu select=1

compdef -d cygstart

# prompts
autoload -U colors ; colors

PROMPT='%{$reset_color%}
%{$fg[green]%}%n@`hostname`${WINDOW:+"[$WINDOW]"} %{$fg[yellow]%}%~%{$reset_color%}
%(!.#.$) '

PROMPT2='%_> '

RPROMPT='%{$fg[cyan]%}[\$?=$?]%{$reset_color%}'

# aliases
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias .=source

# source the users' aliases if it exists
if [ -f "${HOME}/.alias" ]; then
    source "${HOME}/.alias"
fi
