## ~/.bash_profile: executed by bash for login shells.

# source the system wide bashrc if it exists
if [ -e /etc/bash.bashrc ] ; then
    source /etc/bash.bashrc
fi

# source the common users' settings if it exists
if [ -e "${HOME}" ]; then
    source "${HOME}/.profile"
fi

# source the users bashrc if it exists
if [ -e "${HOME}/.bashrc" ] ; then
    source "${HOME}/.bashrc"
fi