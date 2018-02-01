"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
"------------------------------------------------Vim-Configuration----------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
filetype plugin on

let $MYPLUGINS = '~/.vim/plugins.vim'                                            "Set the location of the plugins file.
so $MYPLUGINS                                                                        "Source the external plugins file.

let $MYCOLORSCHEME = '~/.vim/bundle/vim-quantum/colors/quantum.vim'

let g:miniBufExplVSplit = 40   " column width in chars
let g:miniBufExplorerAutoStart = 0
let g:miniBufExplSortBy = 'mru'

"---------------------------------------------------------------------------------------------------------------------"
"---------------------------------------------------Basic-Visuals-----------------------------------------------------"
"----------------------This section contains basic visual settings that may change from time to time.-----------------"
"---------------------------------------------------------------------------------------------------------------------"
syntax enable                                                                              "Enable syntax highlighting.
colorscheme quantum                                                                               "Set the colorscheme.
let g:airline_theme = 'magic'                                                                "Set the status bar theme.

set number                                                                                       "Display line numbers.
set list                                                                                          "Sow list characters.

set nospell                                                                                     "Don't use spell check.

set hlsearch incsearch                                                            "Highlight search results on keydown.

if $TERM_PROGRAM =~ "iTerm"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"                                         "Cursor is a block in normal mode.
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"                                  "Cursor is a vertical bar in insert mode.
endif

"---------------------------------------------------------------------------------------------------------------------"
"----------------------------------------------------Advanced-Visuals-------------------------------------------------"
"----------------------------This section contains advanced visual settings, tweaks, and hacks.-----------------------"
"---------------These settings generally do not affect a normal workflow and should be changed less often.------------"
"---------------------------------------------------------------------------------------------------------------------"

"/
"/ General
"/
set noshowmode                                                        "Don't display the mode (it's in the status bar).

set listchars=tab:->,trail:~,extends:>,precedes:<,space:·             "Defines how list characters should be displayed.

"Hide the vertical split line.
set fillchars+=vert:\ 

set noerrorbells visualbell t_vb=                                                                    "Get rid of bells.

"/
"/ True colors
"/
set t_Co=256
set t_8f=[38;2;%lu;%lu;%lum
set t_8b=[48;2;%lu;%lu;%lum
set termguicolors

"/
"/ Additional Highlighting
"/
setl updatetime=300                                                      "Milliseconds until CursorHold event is fired.
autocmd CursorHold * call HighlightCurrentWord()                             "Highlight all occurences of current word.

"Here are some additional custom highlighting rules that are calculated
"based on "other parts of the current color scheme.
let s:marks_color = color#Lighten(color#GetHighlight('Normal', 'guibg'), 50)
silent! call color#Highlight('CurrentWord', '', s:marks_color, '')
silent! call color#Highlight('EndOfBuffer', color#GetHighlight('Normal', 'guibg'), '', '')
silent! call color#Highlight('SpecialKey', s:marks_color, '', '')
silent! call color#Highlight('Whitespace', s:marks_color, '', '')
silent! call color#Highlight('VertSplit', color#GetHighlight('Normal', 'guibg'), color#GetHighlight('Normal', 'guibg'), '')
silent! call color#Highlight('Visual', 'NONE', s:marks_color, '')

"Here are some highlighting overrides we want to apply no matter what
"colorscheme we are using.
silent! call color#Highlight('NERDTreeDefaultIcon', '#78909C', '', '')
silent! call color#Highlight('NERDTreeFile', '#6a6c6c', '', '')
silent! call color#Highlight('Directory', '#6a6c6c', '', '')

"/
"/ Buffer Explorer
"/
" Don't show list characters when the filetype is undetected.
augroup NoFileType
  autocmd!
  autocmd BufNewFile,BufRead * if &filetype ==# '' | setlocal filetype=noft | endif
  autocmd FileType noft setlocal nolist
augroup END

let g:bufExplorerDefaultHelp = 0                                                             "Do not show default help.

" Change the highlighting of the buffer explorer
hi def link bufExplorerActBuf Identifier
hi def link bufExplorerAltBuf Normal
hi def link bufExplorerCurBuf Identifier
hi def link bufExplorerHidBuf Normal
hi def link bufExplorerLockedBuf Normal
hi def link bufExplorerModBuf Normal
hi def link bufExplorerUnlBuf Normal
hi def link bufExplorerInactBuf Normal

"/
"/ Indent Guides
"/
let g:indentLine_char = '│'                                                           "Sets the indent guide character.
let g:indentLine_color_gui = s:marks_color                                                "Sets the indent guide color.

"/
"/ Tab bar
"/
let g:airline#extensions#tabline#enabled = 0                             "Don't Show buffers when only one tab is open.

let g:webdevicons_enable_airline_tabline = 0                                 "Don't show file icons in the top tab bar.

let g:airline#extensions#tabline#left_sep = ' '                                                "Set the tabs separator.
let g:airline#extensions#tabline#left_alt_sep = '|'                                            "Set the tabs separator.

"/
"/ Sidebar
"/
let g:NERDTreeWinSize=64                                                           "Set the sidebar width (in columns).
let NERDTreeMinimalUI=1                                                                                "Use minimal UI.
let NERDTreeCascadeSingleChildDir = 0                                                      "Don't collapse directories.
let NERDTreeHighlightCursorline = 0                                            "Don't highlight cursor line in sidebar.

autocmd FileType nerdtree setlocal nolist                                       "Don't show list characters in sidebar.

let g:WebDevIconsUnicodeDecorateFolderNodes = 1                                                     "Show folder icons.
let g:DevIconsEnableFolderPatternMatching = 0                                     "Don't show spcial icons for folders.
let g:DevIconsEnableFoldersOpenClose = 1                                                "Show folders open/close icons.

"Hack for hiding some unwanted clutter in the NERDTree sidebar.
let g:NERDTreeDirArrowExpandable = 'ƛ'                                                 "Set the arrow to a flag symbol.
let g:NERDTreeDirArrowCollapsible = 'ƛ'                                                "Set the arrow to a flag symbol.
autocmd filetype nerdtree syntax match hideInNerdTree '\v\/$|ƛ|\*|\".*' contained conceal cchar=_ containedin=ALL

let NERDTreeShowHidden = 1                                                            "Include hidden files in sidebar.
let NERDTreeRespectWildIgnore = 1                                                            "Respect the `wildignore`.
let NERDTreeIgnore = ['.git[[dir]]']                                                          "Ignore from the sidebar.

"Open the sidebar on startup if no files were opened.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

"/
"/ Buffer Explorer
"/
let g:bufExplorerDefaultHelp=0                                                               "Do not show default help.

"/
"/ File Icons
"/
let g:WebDevIconsOS = 'Darwin'                                              "Might help performance, assumes OS is Mac.

let g:WebDevIconsUnicodeDecorateFolderNodesDefaultSymbol = '  '                           "Set the folder closed icon.
let g:DevIconsDefaultFolderOpenSymbol = '  '                                                "Set the folder open icon.

let g:WebDevIconsUnicodeDecorateFileNodesDefaultSymbol = ''                                     "Set the default icon.
autocmd FileType nerdtree syntax match NERDTreeDefaultIcon '' contained containedin=ALL   "Add color for default icon.

"Add syntax highlighting for filetypes not yet supported by the current plugin.
let s:red = "D14748"
let s:orange = "D28445"

let g:NERDTreeExtensionHighlightColor = {}
let g:NERDTreeExtensionHighlightColor['rest'] = s:orange
let g:NERDTreeExtensionHighlightColor['xml'] = s:red

"/
"/ Gutter symbols
"/
set signcolumn=yes                                                                        "Always show the sign column.

let g:ale_sign_error = '●'                                                                       "Set the error symbol.
let g:ale_sign_warning = '●'                                                                   "Set the warning symbol.

let g:ycm_error_symbol = '●'                                                                     "Set the error symbol.
let g:ycm_warning_symbol = '●'                                                                 "Set the warning symbol.

"/
"/ Status bar
"/
let g:airline_powerline_fonts = 1                                                      "Auto populate powerline glyphs.
set laststatus=2                                                                          "Always show the status bars.

let g:airline#extensions#ale#enabled = 1                                        "Show errors & warnings in status line.

"---------------------------------------------------------------------------------------------------------------------"
"----------------------------------------------------Behavior---------------------------------------------------------"
"-----------------------------------This section contains behavioral settings.----------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"

"/
"/ General
"/
set encoding=utf8
set nocompatible                                                               "Don't preserve backwards compatibility.

set noswapfile                                                                                   "Don't use a swapfile.
set autowriteall                                                           "Automatically write when switching buffers.

set ignorecase smartcase                                                                         "Set case sensitivity.

set backspace=indent,eol,start                                                         "Make backspace behave normally.

set textwidth=0 wrapmargin=0                                                          "Prevent auto adding line breaks.
set nowrap                                                                                    "Don't wrap to next line.

"/
"/ Splits
"/
set splitbelow                                                               "Open new splits below the current buffer.
set splitright                                                     "Open new splits to the right of the current buffer.

"/
"/ Tabs & Spaces
"/
set expandtab                                                                                  "Convert tabs to spaces.
set nojoinspaces                                                                                    "Don't join spaces.
set smarttab                                                                       "Tab according to indentation level.
set shiftwidth=4                                                                          "Set indentation to 4 spaces.
set softtabstop=4                                                                 "Insert 4 spaces when tab is pressed.
set tabstop=4                                                                                       "A tab is 4 spaces.

"Two spaces for .yml files.
autocmd Filetype yaml setlocal ts=2 sw=2 sts=0 expandtab

"/
"/ Commenting Stuff Out
"/
let g:NERDSpaceDelims = 1                                                         "Add a space after comments, ex: // .

"/
"/ Files & Ignores
"/
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,.DS_Store                                                 "Ignore these files.
let g:EditorConfig_exclude_patterns = ['fugitive://.*']                                     "Exclude from editorconfig.

"Set files ctrl+p should ignore
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)|(node_modules|vendor|dist)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ }

"/
"/ Syntax checkers (with Ale)
"/
let g:ale_linters = {
\   'html': [],
\}

"/
"/ File Browsing
"/
let NERDTreeHijackNetrw = 0                                                      "Make NERDTree work better with Netrw.

"/
"/ Searching
"/
"Searching with Ag
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

"/
"/ Rest Requests
"/
let g:vrc_elasticsearch_support = 1
let g:vrc_allow_get_request_body = 1

"---------------------------------------------------------------------------------------------------------------------"
"--------------------------------------------------Key-Bindings-------------------------------------------------------"
"--------------This section contains all our custom key bindings and is the heart and soul of everything.-------------"
"---------------------------------------------------------------------------------------------------------------------"

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

"Close Vim
nmap <Leader>Q :qa!<cr>
nnoremap Q <Nop>

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

" Require double <esc> to exit multiple cursors.
let g:multi_cursor_exit_from_visual_mode = 0

"Cycle through the yank stack
nmap <leader>p <Plug>yankstack_substitute_older_paste

"Easy motion
nmap 4 <Plug>(easymotion-bd-w)

"Search the current file
nmap <space> /

"Find all occurences of current word in current buffer (with CtrlSF).
nmap * :CtrlSF <C-R>=expand('<cword>')<cr><space>% -B 2 -A 2<cr>
vmap * y:CtrlSF <C-R>"<CR>

"Search the entire project.
nmap <Leader>s :CtrlSF<space>

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
nnoremap <C-p> :call SwitchToWriteableBufferAndExec('CtrlP')<CR>

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

map <Leader>e :MBEToggle!<cr>:MBEFocus<cr>j

"/
"/ Terminal Emulator
"/
nmap <Leader>t :NERDTreeClose<CR>:vs<CR>:terminal<CR>a

"/
"/ REST Requests
"/
"Make a requst.
let g:vrc_trigger = '<Leader>r'

"/
"/ Commonly used files
"/
"Edit the Vimrc file
nmap ;ev :call SwitchToWriteableBufferAndExec('edit ~/.vimrc')<CR>
nmap ;en :call SwitchToWriteableBufferAndExec('edit $MYVIMRC')<CR>

tnoremap <Esc> <C-\><C-n>

"Edit the Vimrc file
nmap ;ep :call SwitchToWriteableBufferAndExec('edit $MYPLUGINS')<CR>

"Edit the colorscheme file
nmap ;es :call SwitchToWriteableBufferAndExec('edit $MYCOLORSCHEME')<CR>

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

"---------------------------------------------------------------------------------------------------------------------"
"-------------------------------------------Fixes-for-Resourcing-This-File--------------------------------------------"
"---------------------------------------------------------------------------------------------------------------------"
augroup reload_vimrc
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
    autocmd BufWritePost $MYVIMRC :call RefreshUI()
augroup END

"---------------------------------------------------------------------------------------------------------------------"
"-----------------------------------------------------Functions-------------------------------------------------------"
"--------------------This section contains a few helper functions that are used above in the vimrc.-------------------"
"---------------------------------------------------------------------------------------------------------------------"

"/
"/ Prevent CtrlP opening files inside non-writeable buffers.
"/
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

"/
"/ Highlight all instances of the word currently under the cursor.
"/
function! HighlightCurrentWord()
    "exclude from nerdtree
    if &ft =~ 'nerdtree'
        return
    endif
    silent! exec 'match CurrentWord "\<' . escape(expand('<cword>'), '\') . '\>"'
endfunction

"/
"/ Some portions of the UI need to be refreshed when resourcing the .vimrc.
"/
function! RefreshUI()
    "For some reason, AirlineRefresh has to be called twice to work properly.
    "See here: https://github.com/vim-airline/vim-airline/issues/539
    if exists(':AirlineRefresh')
        AirlineRefresh | AirlineRefresh
    endif

    "Fix syntax matching issues with icons (concealing brackets).
    if exists('g:loaded_webdevicons')
        call webdevicons#refresh()
    endif
endfunction
