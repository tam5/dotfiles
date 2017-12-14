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
Plugin 'tpope/vim-vinegar'                                                               "Help with directory browsing.
Plugin 'scrooloose/nerdtree'                                                                           "Adds a sidebar.
Plugin 'vim-airline/vim-airline'                                                                 "Powerline status bar.
Plugin 'vim-airline/vim-airline-themes'                                                             "Status bar themes.
Plugin 'tpope/vim-fugitive'                                                                                "Git plugin.
Plugin 'airblade/vim-gitgutter'                                                                            "Git Gutter.
Plugin 'ctrlpvim/ctrlp.vim'                                                                  "Add ctrl+p file browsing.
Plugin 'scrooloose/nerdcommenter'                                                            "Easily comment out lines.
Plugin 'Yggdroot/indentLine'                                                                            "Indent guides.
Plugin 'tam5/vim-quantum'                                                                                "Color scheme.
Plugin 'tam5/typescript-vim'                                                                    "Add Typescript syntax.
Plugin 'wavded/vim-stylus'                                                                          "Add Stylus synatx.
Plugin 'tam5/php.vim'                                                                                  "Add PHP syntax.
Plugin 'pangloss/vim-javascript'                                                                "Add Javascript syntax.
Plugin 'posva/vim-vue'                                                                                 "Add vue syntax.
Plugin 'jiangmiao/auto-pairs'                                                                  "Auto pairs {}, '', etc.
Plugin 'alvan/vim-closetag'                                                                      "Auto close HTML tags.
Plugin 'vim-syntastic/syntastic'                                                                       "Syntax checker.
Plugin 'mileszs/ack.vim'                                                                           "Search the project.
Plugin 'Valloric/YouCompleteMe'                                                                         "Auto complete.
Plugin 'ryanoasis/vim-devicons'                                                                        "Add file icons.
Plugin 'ap/vim-css-color'                                                                              "Preview colors.
Plugin 'maxbrunsfeld/vim-yankstack'                                                                      "Yank history.
Plugin 'easymotion/vim-easymotion'                                                                        "Easy motion.


call vundle#end()                                                                                            "required.
filetype plugin indent on                                                                                    "required.
