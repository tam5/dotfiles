"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
"------------------------------------------------Vim-Configuration----------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"

filetype plugin on


"-----------------------------------------------------Variables-------------------------------------------------------"
let $MYPLUGINS = '~/.vim/plugins.vim'                                            "Set the location of the plugins file.
so $MYPLUGINS                                                                        "Source the external plugins file.

let $MYCOLORSCHEME = '~/.vim/bundle/vim-quantum/colors/quantum.vim'


"-----------------------------------------------------Visuals---------------------------------------------------------"
set background=dark
set t_Co=256                                                                                           "Use 256 colors.
set t_8f=[38;2;%lu;%lu;%lum                                                                         "For true colors.
set t_8b=[48;2;%lu;%lu;%lum                                                                         "For true colors.
set termguicolors                                                                                     "Use true colors.

syntax enable                                                                              "Enable syntax highlighting.
set number                                                                                       "Display line numbers.

set nospell                                                                                     "Don't use spell check.

set signcolumn=yes                                                                        "Always show the sign column.

let g:quantum_black=1                                                                      "Use the dark quantum theme.
colorscheme quantum                                                                              "Set the color scheme.

"Hide vertical split line
set fillchars+=vert:\ 

set list                                                                                          "Sow list characters.
set listchars=tab:->,trail:~,extends:>,precedes:<,space:·             "Defines how list characters should be displayed.

let g:indentLine_char = '│'                                                           "Sets the indent guide character.
let g:indentLine_color_gui = '#292B2D'                                                    "Sets the indent guide color.

set hlsearch                                                                                 "Highlight search results.
set incsearch                                                                     "Highlight search results on keydown.

if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"                                              "Vertical bar in insert mode.
    let &t_EI = "\<Esc>]50;CursorShape=2\x7"                                                 "Underline in normal mode.
endif

set noerrorbells visualbell t_vb=                                                                    "Get rid of bells.

let g:webdevicons_enable_airline_tabline = 0                                 "Don't show file icons in the top tab bar.
let g:WebDevIconsOS = 'Darwin'                                              "Might help performance, assumes OS is Mac.

"/
"/ Sidebar
"/
let g:NERDTreeWinSize=64                                                           "Set the sidebar width (in columns).
let g:WebDevIconsUnicodeDecorateFolderNodes = 1                                                     "Show folder icons.
let g:DevIconsEnableFoldersOpenClose = 1                                                "Show folders open/close icons.

let NERDTreeShowHidden = 1                                                            "Include hidden files in sidebar.

let g:NERDTreeDirArrowExpandable = 'ƛ'                                            "Hide the sidebar arrows with a hack.
let g:NERDTreeDirArrowCollapsible = 'ƛ'                                           "Hide the sidebar arrows with a hack.

let g:WebDevIconsUnicodeDecorateFolderNodesDefaultSymbol = '  '                           "Set the folder closed icon.
let g:DevIconsDefaultFolderOpenSymbol = '  '                                                "Set the folder open icon.

let g:WebDevIconsUnicodeDecorateFolderNodes = 1                                                     "Show folder icons.
let g:DevIconsEnableFoldersOpenClose = 1                                                "Show folders open/close icons.
let g:DevIconsEnableFolderPatternMatching = 0                                            "Don't show icons for folders.

autocmd FileType nerdtree setlocal nolist                                       "Don't show list characters in sidebar.

"Open the sidebar on startup if no files were opened.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

"Hack for hiding some unwanted clutter in the NERDTree sidebar.
autocmd filetype nerdtree syntax match hideInNerdTree '\v\/$|ƛ|\".*' contained conceal cchar=_ containedin=ALL

"After a re-source, fix syntax matching issues (concealing brackets):
if exists('g:loaded_webdevicons')
    call webdevicons#refresh()
endif


"---------------------------------------------------Status-Bars-------------------------------------------------------"
let g:airline_theme = 'minimalist'                                                           "Set the status bar theme.
let g:airline_powerline_fonts = 1                                                      "Auto populate powerline glyphs.
let g:airline#extensions#tabline#enabled = 1                                   "Show buffers when only one tab is open.
set laststatus=2                                                                          "Always show the status bars.
let g:airline#extensions#tabline#left_sep = ' '                                                "Set the tabs separator.
let g:airline#extensions#tabline#left_alt_sep = '|'                                            "Set the tabs separator.

set noshowmode                                                        "Don't display the mode (it's in the status bar).


"----------------------------------------------------Behavior---------------------------------------------------------"
set encoding=utf8

set noswapfile                                                                                   "Don't use a swapfile.
set autowriteall                                                           "Automatically write when switching buffers.

set ignorecase smartcase                                                                         "Set case sensitivity.

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

let g:NERDSpaceDelims = 1                                                         "Add a space after comments, ex: // .

set wildignore+=*/tmp/*,*.so,*.swp,*.zip                                                           "Ignore these files.


"-------------------------------------------------File-Browsing-------------------------------------------------------"
let NERDTreeHijackNetrw = 0                                                      "Make NERDTree work better with Netrw.

"Set files ctrl+p should ignore
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)|(node_modules|vendor|dist)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ }

"Searching with Ag
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif


"---------------------------------------------------Syntastic----------------------------------------------------------"
"Use the recommended settings for now.
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_typescript_checkers = ['tslint']
let g:syntastic_html_checkers = []

let g:syntastic_check_on_open = 0
let g:syntastic_error_symbol = '●'
let g:syntastic_warning_symbol = '●'


"-------------------------------------------------REST-Requests-------------------------------------------------------"
let g:vrc_elasticsearch_support = 1
let g:vrc_allow_get_request_body = 1


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
nmap <Leader>bb :bp<cr>

"Go to next buffer
nmap <Leader>bf :bn<cr>

"Delete current buffer
nmap <Leader>d :Kwbd<cr>

"Create new file relative to current file
nmap <Leader>fc :e <C-R>=expand('%:h') . '/'<cr>

"/
"/ Git bindings
"/
nmap <Leader>gb :Gblame<cr>
nmap <Leader>gs :Gstatus<cr>

"/
"/ Workflow bindings
"/

"Redo previous change
nmap U :redo<cr>

"Comment out lines (with NERDComment)
noremap <Leader>x :call NERDComment(1, 'Toggle')<cr>

"Make tab and shift-tab indent as expected.
nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
inoremap <S-Tab> <C-D>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

"Cycle through the yank stack
nmap <leader>p <Plug>yankstack_substitute_older_paste

nmap ;w <Plug>(easymotion-bd-w)

"Search the current file
nmap <space> /

"Search the entire project
nmap <leader>s :Ack!<space>

"Close the quick fix window
nmap <leader>cc :cclose<cr>

"Go to definition (with ctags)
nmap gd :YcmCompleter GoToDefinition<cr>

"Jump back from definition
nmap g- <C-O>

"Find all references
nmap gr :YcmCompleter GoToReferences<cr>

" Browse files (with CtrlP)
let g:ctrlp_map = ''
nnoremap <C-p> :call SwitchToWriteableBufferAndExec('CtrlPMixed')<CR>

"Browse symbols (with CtrlP)
nmap <C-R> :CtrlPBufTag<cr>

"Open recent files (with CtrlP)
nnoremap <Leader>mru :call SwitchToWriteableBufferAndExec('CtrlPMRUFiles')<CR>

"Search the current file
nmap <space> /

"Remove highlighting from search results
nmap <Leader><space> :nohlsearch<cr>

"Toggle sidebar (with NERDTree)
nmap <Leader>1 :NERDTreeToggle<cr>

"Reveal the current file in the sidebar
nmap<Leader>2 :NERDTreeFind<cr>

"/
"/ REST Requests
"/
"Make a requst.
let g:vrc_trigger = '<Leader>r'

"/
"/ Commonly used files
"/
"Edit the Vimrc file
nmap <Leader>ev :call SwitchToWriteableBufferAndExec('edit $MYVIMRC')<CR>

"Edit the Vimrc file
nmap <Leader>ep :call SwitchToWriteableBufferAndExec('edit $MYPLUGINS')<CR>

"Edit the colorscheme file
nmap <Leader>es :call SwitchToWriteableBufferAndExec('edit $MYCOLORSCHEME')<CR>

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

let g:UltiSnipsSnippetsDir="~/.vim/ultisnips"
let g:UltiSnipsSnippetDirectories=["ultisnips"]


"--------------------------------------------------Auto-Commands------------------------------------------------------"
augroup autosourcing                                                      "Automatically source the Vimrc file on save.
    autocmd!
    autocmd BufWritePost .vimrc source %
augroup END


"-----------------------------------------------------Functions-------------------------------------------------------"

" Prevent CtrlP opening files inside non-writeable buffers
function! SwitchToWriteableBufferAndExec(command)
    let c = 0
    let wincount = winnr('$')
    " Don't open it here if current buffer is not writable (e.g. NERDTree)
    while !empty(getbufvar(+expand("<abuf>"), "&buftype")) && c < wincount
        exec 'wincmd w'
        let c = c + 1
    endwhile
    exec a:command
endfunction
