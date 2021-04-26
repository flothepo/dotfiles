set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin('~/.config/nvim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" Always show statusline
set laststatus=2
set clipboard+=unnamedplus

" Use 256 colours (Use this setting only if your terminal supports 256 colours)
set t_Co=256
syntax on
set number
set showmatch

" Farben
highlight Normal ctermfg=lightgrey
highlight Comment ctermfg=darkgreen
highlight Constant ctermfg=lightred
highlight LineNr ctermfg=darkgrey
highlight Statement cterm=bold ctermfg=003
highlight Type ctermfg=magenta
let g:airline_theme='deus'

