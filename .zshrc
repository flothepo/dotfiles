
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
