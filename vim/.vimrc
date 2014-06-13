" this is the bare minimum
let g:dotvim_settings = {}
let g:dotvim_settings.version = 1

" here are some basic customizations, please refer to the top of the vimrc file 
" for all possible options
let g:dotvim_settings.max_column = 120

" by default, language specific plugins are not loaded.  this can be changed with the following:
let g:dotvim_settings.plugin_groups_exclude = ['ruby','python','javascript']

" alternatively, you can set this variable to load exactly what you want
let g:dotvim_settings.plugin_groups = ['core','web','editing','navigation','unite','autocomplete','scm']

" finally, load the distribution
source ~/.vim/vimrc

" anything defined here are simply overrides
set wildignore+=\*/node_modules/\*
set guifont=Wingdings:h10
let g:unite_prompt=''
" Text Formatting/Layout    {{{
" " ----------------------------------------------------------------------

		" PHP {{{2
	let php_sql_query=1         " highlight all sql queries
	let php_htmlInStrings=1     " highlight html syntax within strings
	let php_noShortTags=1       " disable short tags
	let php_folding=1           " enable folding for classes and functions
	au FileType php setlocal noexpandtab shiftwidth=4 softtabstop=4 tabstop=4
	au BufRead,BufNewFile *.ps1 set ft=ps1

	" Python {{{2
	au FileType python set omnifunc=pythoncomplete#Complete
	au FileType python setlocal et sw=4 sts=4 ts=4 ai foldmethod=indent foldlevel=99

	" Markdown {{{2
	" Don't insert linebreaks in documents, it screws up conversions
	au FileType markdown setlocal tw=0 wrap linebreak nolist wrapmargin=0 ai formatoptions=tcroqn2 comments=n:>

	" Text {{{2
	"au BufRead,BufNewFile *.txt set filetype=txt
	au FileType txt set tw=100 autoindent expandtab formatoptions=taqn

	" VimWiki {{{2
	au FileType vimwiki set foldlevel=2 foldlevelstart=2
	au FileType vimwiki map <F8> :!ctags -R .<CR>
	au FileType vimwiki let tlist_vimwiki_settings = 'wiki;h:Headers'

	" XML {{{2
	au FileType xml setlocal et sw=2 sts=2 ts=2 ai
	" html {{{2
	let g:html_indent_inctags = "html,body,head,tbody"
	let g:html_indent_script1 = "inc"
	let g:html_indent_style1 = "inc"
	au FileType html setlocal et sw=2 sts=2 ts=2 ai
	" html {{{2
	au FileType css setlocal et sw=2 sts=2 ts=2 ai

	" tex {{{
	au FileType tex set setlocal tabstop=2 softtabstop=2 expandtab smarttab shiftwidth=2 autoindent smartindent shiftround
	" Compilation Rules
	au FileType tex set setlocal let g:Tex_CompileRule_pdf='xelatex -shell-escape -interaction=nonstopmode $*'
	au FileType tex set setlocal let g:Tex_CompileRule_xelatex='xelatex -interaction=nonstopmode $*'
	au FileType tex set setlocal let g:Tex_CompileRule_xgnuplot='xelatex -shell-escape -interaction=nonstopmode $*'
	" View rules
	au FileType tex set setlocal let g:Tex_ViewRule_ps = 'evince'
	au FileType tex set setlocal let g:Tex_ViewRule_pdf = 'evince'
	" Target formats
	au FileType tex set setlocal let g:Tex_DefaultTargetFormat = 'pdf'
	" Ignore some warnings
	au FileType tex set setlocal let g:Tex_IgnoredWarnings="Font""\n"
" }}}

" Directories           {{{
" ------------------------------------------------------------------
    " backup and swap directories rely on variable set in zshenv
    set backup                  " enable backups
    set backupdir=$DATA_PATH/vim/backup// " backups
    set directory=$DATA_PATH/vim/swap//   " swap files
    v:vimrc>702 ? set undofile  " use persistent undofile
    v:vimrc>702 ? set autochdir " always switch to the current file's dir
" }}}
" ------------------------------------------------------------------
" Mappings: hard to type {{{
" ------------------------------------------------------------------
	imap jj <Esc>
	imap uu _
	imap hh =>
	imap aa @
	imap ç `
	imap ¡¡ ^^
	" Map ++ to save current file
	"noremap ++ :w <cr>
	noremap <Left> :bp<CR>
	noremap <Right> :bn<CR>
" }}}
