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
" Plugin 'hzchirs/vim-material'
Plugin 'kaicataldo/material.vim'
Plugin 'itchyny/lightline.vim'
Plugin 'airblade/vim-gitgutter'
" Plugin 'tam5/vim-color-util'

"Editor utils
Plugin 'tpope/vim-vinegar'
Plugin 'tpope/vim-commentary'
Plugin 'jiangmiao/auto-pairs'
Plugin 'alvan/vim-closetag'
Plugin 'tpope/vim-surround'
Plugin 'ctrlpvim/ctrlp.vim'

"Syntax Highlighting
" Plugin 'leafgarland/typescript-vim'
" Plugin 'peitalin/vim-jsx-typescript'
" Plugin 'wavded/vim-stylus'
" Plugin 'tam5/php.vim'
" Plugin 'pangloss/vim-javascript'
" Plugin 'othree/html5.vim'

" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on
