" this is the bare minimum
let g:dotvim_settings = {}
let g:dotvim_settings.version = 1

" here are some basic customizations, please refer to the top of the vimrc file for all possible options
let g:dotvim_settings.default_indent = 2
let g:dotvim_settings.max_column = 120

" by default, language specific plugins are not loaded.  this can be changed with the following:
let g:dotvim_settings.plugin_groups_exclude = ['ruby','python','javascript']

" alternatively, you can set this variable to load exactly what you want
let g:dotvim_settings.plugin_groups = ['core','web']

" finally, load the distribution
source ~/.vim/vimrc

" anything defined here are simply overrides
set wildignore+=\*/node_modules/\*
set guifont=Wingdings:h10
