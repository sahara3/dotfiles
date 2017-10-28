## ~/.bashrc: executed by bash for each shell.

# shell options
set -o ignoreeof
set -o monitor
export HISTCONTROL=ignoredups

PS1='\[\e]0;\w\a\]\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n$ '

# completion
complete -d cd
complete -c man
if [ -e /etc/bash_completion ] ; then
    . /etc/bash_completion
fi

# dircolors
if [ -e "${HOME}/.dircolors" ] ; then
    eval `dircolors -b "${HOME}/.dircolors"`
fi

# source the users' aliases if it exists
if [ -e "${HOME}/.alias" ] ; then
    source "${HOME}/.alias"
fi
