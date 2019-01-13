" /-------------------------------------------------------------------------------
" | Plugins
" |-------------------------------------------------------------------------------
" |
" | Todo
" | 
" | 
" /

filetype plugin on

filetype off

"Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

"Let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

"Visuals
Plugin 'hzchirs/vim-material'
Plugin 'tam5/vim-color-util'
Plugin 'airblade/vim-gitgutter'

"Editor utils
Plugin 'tpope/vim-vinegar'
Plugin 'scrooloose/nerdcommenter'
Plugin 'jiangmiao/auto-pairs'
Plugin 'alvan/vim-closetag'
Plugin 'tpope/vim-surround'
Plugin 'ctrlpvim/ctrlp.vim'

"Syntax Highlighting
Plugin 'tam5/typescript-vim'
Plugin 'wavded/vim-stylus'
Plugin 'tam5/php.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'othree/html5.vim'

" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on
