" /-------------------------------------------------------------------------------
" | Vim Config
" |-------------------------------------------------------------------------------
" | 
" | Vi is like a Ferrari, if you're a beginner, it handles like a bitch, but
" | once you get the hang of it, it's small, powerful and FAST!
" /

set nocompatible

let $MYPLUGINS = '~/.vim/plugins.vim'

if !empty(glob($MYPLUGINS))
   source $MYPLUGINS
endif

" /-------------------------------------------------------------------------------
" | Visuals
" |-------------------------------------------------------------------------------
" |
" | The section below configures all the visuals. Making the editor visually
" | appealing is just as important as making it work properly, isn't it?
" /

"Set the colorscheme
syntax enable
silent! colorscheme material

let g:lightline = { 'colorscheme': 'material_vim' }

"Add line numbers
set number

"Specify which list chars to show when on
set listchars=tab:->,trail:~,extends:>,precedes:<,space:· 

"Highlight search results
set hlsearch

"Always show lightline
set laststatus=2

"Don't show the mode
set noshowmode

"Always show sign column
set signcolumn=yes

"Hide vertical split line
set fillchars+=vert:\ 

"Some dynamic color tweaks
" if has#plugin('vim-color-util')
"     let s:marks_color = color#Lighten(color#GetHighlight('Normal', 'guibg'), 50)
"     silent! call color#Highlight('CurrentWord', '', s:marks_color, '')
"     silent! call color#Highlight('EndOfBuffer', color#GetHighlight('Normal', 'guibg'), '', '')
" endif

function! HighlightCurrentWord()
    "Highlight all instances of the current word.
    silent! exec 'match CurrentWord "\<' . escape(expand('<cword>'), '\') . '\>"'
endfunction

"Milliseconds until CursorHold event is fired
setl updatetime=300
autocmd CursorHold * call HighlightCurrentWord()

"True color support
if (has('termguicolors'))
    let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif

if !has('gui_running')
  set t_Co=256
endif

" /-------------------------------------------------------------------------------
" | Key Bindings
" |-------------------------------------------------------------------------------
" |
" | Vim provides it's own keybindings obviously, but we'll add just a pinch
" | of customization to make things even more awesomely awesome.
" /

"Set the leader key
let mapleader = ','

"Write current buffer
nmap <Leader>w :w<cr>

"Close current window
nmap <Leader>q :q<cr>

"Delete current buffer
nmap <Leader>d :Kwbd<cr>

"Search the current file
nmap <Leader><space> :nohlsearch<cr>

" Splits
nmap <Leader>v :vs<cr>
nmap <Leader>h :sp<cr>

"Make tab and shift-tab indent as expected.
nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
inoremap <S-Tab> <C-D>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

"Show most recently used files
nnoremap <space>fr :call SwitchToWriteableBufferAndExec('CtrlPMRUFiles')<CR>

"Edit the vimrc file
nmap <Leader>ev :e $MYVIMRC<CR>
nmap <Leader>ep :e $MYPLUGINS<CR>

" /-------------------------------------------------------------------------------
" | Behavior
" |-------------------------------------------------------------------------------
" |
" | This sections includes some miscellaneous tweaks of the editor's behavior
" | since some things are admittedly pretty wonky by default.
" /
set incsearch

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

"Prevent auto adding line breaks
set textwidth=0 wrapmargin=0
set nowrap

"Get rid of bells
set noerrorbells visualbell t_vb=

"Persistent undo
set undodir=~/.vim/undodir

"Ingore vendor files from CTRLP
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'

" /-------------------------------------------------------------------------------
" | Auto Commmands
" |-------------------------------------------------------------------------------
" /
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
