## ~/.profile: common settings for login shells.

umask 022

# basic profiles
export USERNAME="Soichiro SAHARA"
export LANG=ja_JP.UTF-8

export PAGER=less
export LESS=fMr

export EDITOR=vim

# set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ]; then
    echo $PATH | grep -q "${HOME}/bin" || PATH=${HOME}/bin:${PATH}
fi

# set MANPATH so it includes users' private man if it exists
if [ -d "${HOME}/man" ]; then
    echo $MANPATH | grep -q "${HOME}/man" || MANPATH=${HOME}/man:${MANPATH}
fi

# set INFOPATH so it includes users' private info if it exists
if [ -d "${HOME}/info" ]; then
    echo $INFOPATH | grep -q "${HOME}/info" || INFOPATH=${HOME}/info:${INFOPATH}
fi

# special pathes for cygwin or msys
function windows_path {
    echo $1 | grep '\\' >/dev/null 2>&1 && return 0
    return 1
}

if [ $(windows_path $JAVA_HOME) ]; then
    export JAVA_HOME=`cygpath $JAVA_HOME`
    export PATH=$JAVA_HOME/bin:$PATH
fi

if [ $(windows_path $GRADLE_HOME) ]; then
    export GRADLE_HOME=`cygpath $GRADLE_HOME`
fi
