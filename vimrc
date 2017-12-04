"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
"------------------------------------------------Vim-Configuration----------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"


"-----------------------------------------------------Variables-------------------------------------------------------"
let $MYPLUGINS = '~/.vim/plugins.vim'                                            "Set the location of the plugins file.
so $MYPLUGINS                                                                        "Source the external plugins file.

let $MYCOLORSCHEME = '~/.vim/bundle/vim-quantum/colors/quantum.vim'

filetype plugin on


"-----------------------------------------------------Visuals---------------------------------------------------------"
set background=dark
set t_Co=256                                                                                           "Use 256 colors.
set t_8f=[38;2;%lu;%lu;%lum                                                                         "For true colors.
set t_8b=[48;2;%lu;%lu;%lum                                                                         "For true colors.
set termguicolors                                                                                     "Use true colors.

syntax enable                                                                              "Enable syntax highlighting.
set number                                                                                       "Display line numbers.

let g:quantum_black=1                                                                      "Use the dark quantum theme.
colorscheme quantum                                                                              "Set the color scheme.

"Hide vertical split line
set fillchars+=vert:\ 

set nolist                                                                                 "Don't show list characters.
set listchars=tab:->,trail:~,extends:>,precedes:<,space:·             "Defines how list characters should be displayed.
:match SpecialKey '\s'                                                      "Override the cursor line for special keys.

let g:indentLine_char = '│'                                                           "Sets the indent guide character.
let g:indentLine_color_gui = '#292B2D'                                                    "Sets the indent guide color.

set hlsearch                                                                                 "Highlight search results.
set incsearch                                                                     "Highlight search results on keydown.

if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"                                              "Vertical bar in insert mode.
    let &t_EI = "\<Esc>]50;CursorShape=2\x7"                                                 "Underline in normal mode.
endif


"---------------------------------------------------Status-Bars-------------------------------------------------------"
let g:airline_theme = 'quantum'                                                              "Set the status bar theme.
let g:airline_powerline_fonts = 1                                                      "Auto populate powerline glyphs.
let g:airline#extensions#tabline#enabled = 1                                   "Show buffers when only one tab is open.
set laststatus=2                                                                          "Always show the status bars.
let g:airline#extensions#tabline#left_sep = ' '                                                "Set the tabs seperator.
let g:airline#extensions#tabline#left_alt_sep = '|'                                            "Set the tabs seperator.

set noshowmode                                                        "Don't display the mode (it's in the status bar).


"----------------------------------------------------Behavior---------------------------------------------------------"
set mouse=a                                                                                          "Enable the mouse.

set clipboard=unnamed                                                                            "Use system clipboard.

set ignorecase smartcase

set backspace=indent,eol,start                                                         "Make backspace behave normally.

set textwidth=0 wrapmargin=0                                                          "Prevent auto adding line breaks.
set nowrap                                                                                    "Don't wrap to next line.

set splitbelow                                                               "Open new splits below the current buffer.
set splitright                                                     "Open new splits to the right of the current buffer.

set expandtab                                                                                  "Convert tabs to spaces.
set nojoinspaces                                                                                    "Don't join spaces.
set smarttab                                                                       "Tab according to indentation level.
set shiftwidth=4                                                                          "Set indentation to 4 spaces.
set softtabstop=4                                                                 "Insert 4 spaces when tab is pressed.
set tabstop=4                                                                                       "A tab is 4 spaces.

set wildignore+=*/tmp/*,*.so,*.swp,*.zip                                                           "Ignore these files.


"-------------------------------------------------File-Browsing-------------------------------------------------------"
let NERDTreeHijackNetrw = 0                                                      "Make NERDTree work better with Netrw.

"Set files ctrl+p should ignore
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)|(node_modules|vendor|dist)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ }


"--------------------------------------------------Key-Bindings-------------------------------------------------------"
"Set the leader key to comma
let mapleader = ','

"Exit insert mode with jk
imap jk <Esc>


"/
"/ Files, buffers, etc.
"/

"Save the file
nmap <Leader>w :w<cr>

"Close current window
nmap <Leader>q :q<cr>

"Go to previous buffer
nmap <Leader>2 :bp<cr>

"Go to next buffer
nmap <Leader>3 :bn<cr>

"Delete current buffer
nmap <Leader>bd :bd<cr>


"/
"/ Git bindings
"/
nmap <Leader>gb :Gblame<cr>
nmap <Leader>gs :Gstatus<cr>


"/
"/ Workflow bindings
"/

"Comment out lines (with NERDComment)
noremap <Leader>x :call NERDComment(1, 'Toggle')<cr>

"Make tab and shift-tab indent as expected.
nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
inoremap <S-Tab> <C-D>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

"Duplicate line
nmap <Leader>d yyp

"Search the current file
nmap <space> /

"Go to definition (with ctags)
nmap gd <C-]>

"Jump back from definition
nmap g- <C-T>

nmap gp <C-6>

"Browse symbols (with CtrlP)
nmap <C-R> :CtrlPBufTag<cr>

"Open recent files (with CtrlP)
nmap <Leader>mru :CtrlPMRUFiles<cr>

"Search the current file
nmap <space> /

"Remove highlighting from search results
nmap <Leader><space> :nohlsearch<cr>

"Toggle sidebar (with NERDTree)
nmap <Leader>1 :NERDTreeToggle<cr>


"/
"/ Commonly used files
"/
"Edit the Vimrc file
nmap <Leader>ev :edit $MYVIMRC<cr>

"Edit the Vimrc file
nmap <Leader>ep :edit $MYPLUGINS<cr>

"Edit the colorscheme file
nmap <Leader>es :edit $MYCOLORSCHEME<cr>


"--------------------------------------------------Auto-Commands------------------------------------------------------"
augroup autosourcing                                                      "Automatically source the Vimrc file on save.
    autocmd!
    autocmd BufWritePost .vimrc source %
augroup END


"-----------------------------------------------------Functions-------------------------------------------------------"

"Smooth scrolling
function! SmoothScroll(up)
    if a:up
        let scrollaction=""
    else
        let scrollaction=""
    endif
    exec "normal " . scrollaction
    redraw
    let counter=1
    while counter<&scroll
        let counter+=1
        sleep 10m
        redraw
        exec "normal " . scrollaction
    endwhile
endfunction

nnoremap <C-U> :call SmoothScroll(1)<Enter>
nnoremap <C-D> :call SmoothScroll(0)<Enter>
inoremap <C-U> <Esc>:call SmoothScroll(1)<Enter>i
inoremap <C-D> <Esc>:call SmoothScroll(0)<Enter>i

map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>

