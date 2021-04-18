export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="spaceship"

plugins=(
  themes
  zsh-syntax-highlighting
)

HISTFILE="$XDG_CACHE_HOME/zsh/history"
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

source $ZSH/oh-my-zsh.sh
source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/util.zsh

spaceship_customs

if [ "$(tty)" = "/dev/tty1" ]; then
	exec sway
fi
