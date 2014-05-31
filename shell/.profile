# universal zsh/bash environment & initialization script
# es@ethanschoonover.com

DEBUG=false


# ---------------------------------------------------------------------
# PLATFORM SPECIFIC ENVIRONMENT CONFIG
# ---------------------------------------------------------------------
case $(uname) in
        Linux)
            # following requires a chrome script. using this as the chrome
            # script can start chromium differently on different systems,
            # for intance, forcing webgl to start
            # export BROWSER=uzbl
            export BROWSER=chromium
            export PLATFORM=linux
        ;;
        Darwin)
            export HOME=$HOME/home
            cd $HOME
            export PATH="$PATH:/opt/local/bin:/opt/local/sbin"
            export PATH="$HOME/Library/Haskell/bin:$PATH"
            # following because brew install zsh misses its functions
            fpath=( /usr/local/share/zsh/functions $fpath )
            export BROWSER="open /Applications/Google\ Chrome.app"
            export PLATFORM=osx
        ;;
        *)
            logger "WARNING: Unknown platform detected (~/etc/bin/shell/profile)" 
        ;;
esac

# ---------------------------------------------------------------------
# ENVIRONMENT VARIABLES
# ---------------------------------------------------------------------
# The following conf files use the bin or conf directories in hardcoded 
# declarations due to config file limitations:
# ../mail/offlineimaprc
# ../web/newsbeuter-config  -- may support path variable, need to test
# ../crontab/crontab-current*

export BIN_DIR=bin
export CONF_DIR=etc
export SEC_DIR=sec
export DATA_DIR=var

export BIN_PATH=$HOME/$BIN_DIR
export CONF_PATH=$HOME/$CONF_DIR
export SEC_PATH=$HOME/$SEC_DIR
export DATA_PATH=$HOME/$DATA_DIR

export MAILCONF=$CONF_PATH/mail # used in muttrc, offlineimaprc, msmtprc
export MAILDATA=$DATA_PATH/mail # used in muttrc, offlineimaprc, msmtprc

export ARCH_HASKELL='Ethan Schoonover <es@ethanschoonover.com>'

export XENVIRONMENT="$HOME/.Xresources"

# see http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html#variables
#export XDG_DATA_HOME="$HOME/var/data" # default is $HOME/.local/share
#export XDG_CONFIG_HOME="$HOME/etc/config" # default is $HOME/.config
#export XDG_CACHE_HOME="$HOME/var/cache # default is $HOME/.cache


# ---------------------------------------------------------------------
# PATH
# ---------------------------------------------------------------------
# initial bin relative to current directory
export PATH="$HOME/.gem/ruby/2.1.0/bin:bin:/usr/local/bin:/usr/local/sbin:$PATH:$BIN_PATH:$CONF_PATH/bin:$SEC_PATH/bin"
export PATH="$PATH:$HOME/.cabal/bin"

# ---------------------------------------------------------------------
# APPS
# ---------------------------------------------------------------------
export EDITOR=vim
#export BROWSER=chromium #see above in platform detection case block
export MUTTJUMP_INDEXER=notmuch

# ---------------------------------------------------------------------
# ALIASES
# ---------------------------------------------------------------------
alias uzbl='uzbl-browser &>/dev/null &'
alias netcfg='sudo netcfg'
alias shutdown='sudo pm-powersave false && sudo poweroff'
alias reboot='sudo pm-powersave false && sudo reboot'
alias weechat='weechat-curses'

# ---------------------------------------------------------------------
# SHELL TYPE SPECIFIC FUNCTIONS
# ---------------------------------------------------------------------
function omz()
{
# for interactive zsh shells

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
export ZSH_THEME="agnoster"

# export DISABLE_AUTO_UPDATE="true"

# oh-my-zsh plugins
plugins=(vi-mode git mercurial autojump command-not-found python pip github gnu-utils history-substring-search)

# source oh-my-zsh
source $ZSH/oh-my-zsh.sh
bindkey -v

# the following addresses problems with the oh-my-zsh vim plugin eating the
# line above the prompt with resetting it. I've had no negative impact from
# nullifying the function of zle-line-init
#function zle-line-init {
#}
}

# ---------------------------------------------------------------------
# DETECT SHELL TYPE (ZSH/BASH), MODE (NON)INTERACTIVE, STATE (NON)LOGIN
# ---------------------------------------------------------------------
if [[ -n $PS1 ]]; then SHELLSTATE=interactive; else SHELLSTATE=noninteractive; fi

if ( `shopt &>/dev/null` ); then
    SHELLTYPE=bash
    if shopt -q login_shell; then SHELLMODE=login; else SHELLMODE=nonlogin; fi
else
    SHELLTYPE=zsh
    if [[ -o login ]]; then SHELLMODE=login; else SHELLMODE=nonlogin; fi
fi

# ---------------------------------------------------------------------
# HELPER FUNCTIONS
# ---------------------------------------------------------------------

Keychain_Prompt ()
{
    eval `keychain --quiet --attempts 5 --eval id_rsa`
    return 0
}

# ---------------------------------------------------------------------
# EXECUTE BASED ON SHELL MODE
# ---------------------------------------------------------------------
case $SHELLMODE in
    login)
        # this is typically going to be either straight from a virtual terminal 
        # (tty1-n) or through ssh. Possible to include some SSH specific logic 
        # here.  

        # start x if we're not root and on tty1 (e.g. default boot condition)
        # who needs a display manager?
        # Keychain_Prompt
        if [[ $USER != "root" ]] && [[ `tty` = /dev/tty1 ]] && [[ -z "$DISPLAY" ]]; then
            # .Xauthority causes X startup failure every other login. I know 
            # how that sounds. .Xauthority, I do not like you.
            #rm .Xauthority* &>/dev/null || true
            startx
            logout
        fi
        $DEBUG && logger ">>>>>>>>>> SHELL MODE: login ($SHELLTYPE)" || true
    ;;
    nonlogin)
        # normal shell & shell script state
        # Keychain_Prompt
        $DEBUG && logger ">>>>>>>>>> SHELL MODE: nonlogin ($SHELLTYPE)" || true
    ;;
    *)  $DEBUG && logger ">>>>>>>>>> SHELL MODE: NOT DETECTED ($SHELLTYPE)" || true ;;
esac

# ---------------------------------------------------------------------
# EXECUTE BASED ON SHELL STATE
# ---------------------------------------------------------------------
case $SHELLSTATE in
    interactive)
        $DEBUG && logger ">>>>>>>>>> SHELL STATE: interactive ($SHELLTYPE)" || true
        if [[ $SHELLTYPE = "zsh" ]]; then omz; fi
    ;;
    noninteractive)
        $DEBUG && logger ">>>>>>>>>> SHELL STATE: noninteractive ($SHELLTYPE)" || true
    ;;
    *)  $DEBUG && logger ">>>>>>>>>> SHELL STATE: NOT DETECTED" || true ;;
esac
autoload -U compinit promptinit
compinit
promptinit
prompt suse
. /usr/share/zsh/site-contrib/powerline.zsh
