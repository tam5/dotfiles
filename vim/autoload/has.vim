function! has#colorscheme(scheme)
    "Check if a color scheme exists
    return !empty(globpath(&rtp, 'colors/'.a:scheme.'.vim'))
endfunction

function! has#plugin(name)
    "Check if a plugin exists
    return !empty(glob('~/.vim/bundle/'.a:name))
endfunction
