
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="bureau"

plugins=(
  git
  osx
  zsh-syntax-highlighting
  colorize
  themes
  zsh-interactive-cd
)

source $ZSH/oh-my-zsh.sh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

# aliases
alias vim="nvim" 
alias cp="cp -iv"
alias rm="rm -i"
alias mv="mv -i"

alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'


# utility functions

# extract archives
function ex () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2) tar xjf $1 ;;
      *.tar.gz) tar xzf $1 ;;
      *.bz2) bunzip2 $1 ;;
      *.rar) unrar x $1 ;;
      *.gz) gunzip $1 ;;
      *.tar) tar xf $1 ;;
      *.zip) unzip $1 ;;
      *) echo "'$1' cannot be extracted with ex" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# man with colors
function man() {
    LESS_TERMCAP_md=$'\e[01;34m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}
