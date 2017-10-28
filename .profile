## ~/.profile: common settings for login shells.

umask 022

function is_windows {
    if [ $(uname -o) = Cygwin -o $(uname -o) = Msys ]; then
	return 0
    fi
    return 1
}

# basic profiles
#export USER=`id -un`
#export USERNAME="Soichiro SAHARA"
#export HOST=`hostname`
export LANG=ja_JP.UTF-8
#export TZ=JST-09
#export DISPLAY=localhost:0.0

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

# source the machine-dependent environments if it exists
if [ -e "${HOME}/.env" ]; then
    source "${HOME}/.env"
fi

# special pathes for cygwin or msys
if [ $(is_windows) ]; then
    if [ ! -z $JAVA_HOME ]; then
	JAVA_HOME=$(cygpath $JAVA_HOME)
	PATH=$JAVA_HOME/bin:$PATH
    fi
    if [ ! -z $GRADLE_HOME ]; then
	GRADLE_HOME=$(cygpath $GRADLE_HOME)
    fi
    export PATH
fi

