"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
"-----------------------------------------------------Vim-Plugins-----------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
set nocompatible                                                                                "be iMproved, required.
filetype off                                                                                                 "required.

set rtp+=~/.vim/bundle/Vundle.vim                                                                "Set the runtime path.
call vundle#begin()                                                                                        "Initialize.


Plugin 'VundleVim/Vundle.vim'                                                                          "Plugin manager.

"--------------------------------------------------Visuals-&-Layout---------------------------------------------------"
Plugin 'tpope/vim-vinegar'                                                               "Help with directory browsing.
Plugin 'scrooloose/nerdtree'                                                                           "Adds a sidebar.
Plugin 'vim-airline/vim-airline'                                                                 "Powerline status bar.
Plugin 'vim-airline/vim-airline-themes'                                                             "Status bar themes.
Plugin 'airblade/vim-gitgutter'                                                                            "Git Gutter.
Plugin 'Yggdroot/indentLine'                                                                            "Indent guides.
Plugin 'tam5/vim-devicons'                                                                             "Add file icons.
Plugin 'tam5/vim-nerdtree-syntax-highlight'                                                      "Add file icon colors.
Plugin 'ap/vim-css-color'                                                                              "Preview colors.
Plugin 'tam5/vim-quantum'                                                                                "Color scheme.

"-------------------------------------------------Misc-Essentials-----------------------------------------------------"
Plugin 'ctrlpvim/ctrlp.vim'                                                                      "Ctrl+p file browsing.
Plugin 'easymotion/vim-easymotion'                                                                        "Easy motion.
Plugin 'tpope/vim-unimpaired'                                                               "Some extra nifty mappings.

Plugin 'tpope/vim-fugitive'                                                                                "Git plugin.
Plugin 'diepm/vim-rest-console'                                                           "Make CURL requests from VIM.

Plugin 'w0rp/ale'                                                                                      "Syntax checker.
Plugin 'mileszs/ack.vim'                                                                           "Search the project.

Plugin 'editorconfig/editorconfig-vim'                                                "Support for .editorconfig files.

"-----------------------------------------------Syntax-Highlighters---------------------------------------------------"
Plugin 'tam5/typescript-vim'                                                                    "Add Typescript syntax.
Plugin 'wavded/vim-stylus'                                                                          "Add Stylus synatx.
Plugin 'tam5/php.vim'                                                                                  "Add PHP syntax.
Plugin 'pangloss/vim-javascript'                                                                "Add Javascript syntax.
Plugin 'posva/vim-vue'                                                                                 "Add vue syntax.

"-----------------------------------------------Editor-Enhancements---------------------------------------------------"
Plugin 'scrooloose/nerdcommenter'                                                            "Easily comment out lines.
Plugin 'jiangmiao/auto-pairs'                                                            "Auto close pairs {}, '', etc.
Plugin 'alvan/vim-closetag'                                                                      "Auto close HTML tags.
Plugin 'tpope/vim-surround'                                                                  "Change surrounding pairs.
Plugin 'terryma/vim-multiple-cursors'                                                                "Multiple cursors.
Plugin 'maxbrunsfeld/vim-yankstack'                                                                      "Yank history.

"--------------------------------------------Autocomplete-&-Snippets-------------------------------------------------"
Plugin 'ervandew/supertab'                                                           "Better tab completion management.
Plugin 'Valloric/YouCompleteMe'                                                                  "Auto complete & more.
Plugin 'SirVer/ultisnips'                                                                                    "Snippets.
Plugin 'mattn/emmet-vim'                                                                                        "Emmet.


call vundle#end()                                                                                            "required.
filetype plugin indent on                                                                                    "required.
