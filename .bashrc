## ~/.bashrc: executed by bash for non-login shells.

# if not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# prompt
#PS1='[\u@\h:\w]$ ' # no color
PS1='\[\e]0;\w\a\]\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n$ '

# shell options
HISTCONTROL=ignoredups
HISTSIZE=10000
HISTFILESIZE=100000

set -o ignoreeof
set -o monitor
shopt -s histappend
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# completion
complete -d cd
complete -c man
if [ -e /etc/bash_completion ] ; then
    . /etc/bash_completion
fi

# source the machine-dependent environments if it exists
if [ -e "${HOME}/.env" ]; then
    source "${HOME}/.env"
fi

# dircolors
if [ -e "${HOME}/.dircolors" ] ; then
    eval `dircolors -b "${HOME}/.dircolors"`
fi

# source the users' aliases if it exists
if [ -e "${HOME}/.aliases" ] ; then
    source "${HOME}/.aliases"
fi
