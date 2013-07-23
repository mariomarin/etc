# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
export ZSH_THEME="agnoster"

plugins=(vi-mode git mercurial autojump command-not-found python pip github gnu-utils history-substring-search archlinux composer)

source $ZSH/oh-my-zsh.sh
bindkey -v
eval `dircolors ~/projects/dircolors-solarized/dircolors.ansi-dark`

# the following addresses problems with the oh-my-zsh vim plugin eating the
# line above the prompt with resetting it. I've had no negative impact from
# nullifying the function of zle-line-init
function zle-line-init {
}

# start x session if not already in one
# modified from http://pbrisbin.com/pages/display-manager.html
if [[ $USER != "root" ]] && [[ `tty` = /dev/tty1 ]] && [[ -z "$DISPLAY" ]]; then
  startx
  logout
fi

