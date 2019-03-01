" /-------------------------------------------------------------------------------
" | Vim Config
" |-------------------------------------------------------------------------------
" |
" | Todo
" |
" |
" /

let $MYPLUGINS = '~/.vim/plugins.vim'
so $MYPLUGINS

" /-------------------------------------------------------------------------------
" | Visuals
" |-------------------------------------------------------------------------------
" /

syntax enable
set background=dark

if has#colorscheme('vim-material')
    colorscheme vim-material
else
    colorscheme desert
endif

"Show line numbers
set number

set hlsearch incsearch

"Set whitespace characters
set list
set listchars=tab:->,trail:~,extends:>,precedes:<,space:· 

if !has("termguicolors")
    set nolist
endif

"Hide vertical split line
set fillchars+=vert:\ 

set signcolumn=yes

"Some dynamic color tweaks
if has#plugin('vim-color-util')
    let s:marks_color = color#Lighten(color#GetHighlight('Normal', 'guibg'), 50)
    silent! call color#Highlight('CurrentWord', '', s:marks_color, '')
    silent! call color#Highlight('EndOfBuffer', color#GetHighlight('Normal', 'guibg'), '', '')
    silent! call color#Highlight('SpecialKey', s:marks_color, '', '')
    silent! call color#Highlight('Whitespace', s:marks_color, '', '')
    silent! call color#Highlight('VertSplit', color#GetHighlight('Normal', 'guibg'), color#GetHighlight('Normal', 'guibg'), '')
    silent! call color#Highlight('Visual', 'NONE', s:marks_color, '')
endif

function! HighlightCurrentWord()
    "Highlight all instances of the current word.
    silent! exec 'match CurrentWord "\<' . escape(expand('<cword>'), '\') . '\>"'
endfunction

"Milliseconds until CursorHold event is fired
setl updatetime=300
autocmd CursorHold * call HighlightCurrentWord()

"Use true colors
let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
set termguicolors

if $TERM_PROGRAM =~ "iTerm"
    "Cursor is a block in normal mode
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
    "Cursor is a vertical bar in insert mode
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
endif

" /-------------------------------------------------------------------------------
" | Behavior
" |-------------------------------------------------------------------------------
" /

set encoding=utf8
set noswapfile

set splitbelow
set splitright

set expandtab
set nojoinspaces
set smarttab

set shiftwidth=4
set softtabstop=4
set tabstop=4

set ignorecase smartcase
set backspace=indent,eol,start

set clipboard=unnamed

"Prevent auto adding line breaks
set textwidth=0 wrapmargin=0
set nowrap

"Get rid of bells
set noerrorbells visualbell t_vb=

"Don't preserve backwards compatibility to older versions of vim
set nocompatible

"Add a single space after comment delim
let g:NERDSpaceDelims=1

"Re-source the vimrc file when it is saved.
augroup reload_vimrc
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC

    "Reset git gutter
    if exists("g:gitgutter_override_sign_column_highlight")
        call gitgutter#highlight#define_sign_column_highlight()
        call gitgutter#highlight#define_highlights()
    endif
augroup END

" /-------------------------------------------------------------------------------
" | Key Bindings
" |-------------------------------------------------------------------------------
" /

"Set the leader key
let mapleader = ','

"Write current buffer
nmap <Leader>w :w<cr>
"Close current window
nmap <Leader>q :q<cr>
"Quit
nmap <Leader>Q :qa!<cr>
nnoremap Q <Nop>
"Delete current buffer
nmap <Leader>d :Kwbd<cr>

nmap <Leader>v :vs<cr>
nmap <Leader>h :sp<cr>

"Search the current file
nmap <space> /
nmap <Leader><space> :nohlsearch<cr>

"Make tab and shift-tab indent as expected.
nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
inoremap <S-Tab> <C-D>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

"Comment out lines (with NERDComment)
noremap <Leader>x :call NERDComment(1, 'Toggle')<cr>

"Show most recently used files
nnoremap <Leader>mru :call SwitchToWriteableBufferAndExec('CtrlPMRUFiles')<CR>

"Edit the vimrc file
nmap <Leader>ev :e $MYVIMRC<CR>
nmap <Leader>ep :e $MYPLUGINS<CR>
nmap <Leader>ez :e ~/.zshrc<CR>

" /-------------------------------------------------------------------------------
" | Helpers
" |-------------------------------------------------------------------------------
" /

function! SwitchToWriteableBufferAndExec(command)
    "Prevent CtrlP opening files inside non-writeable buffers.
    let c = 0
    let wincount = winnr('$')
    " Don't open it here if current buffer is not writable (e.g. NERDTree)
    while !empty(getbufvar(+expand("<abuf>"), "&buftype")) && c < wincount
        exec 'wincmd w'
        let c = c + 1
    endwhile
    exec a:command
endfunction


