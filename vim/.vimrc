" ----------------------------------------------------------------------
"   Ethan Schoonover
"   es@ethanschoonover.com
"   vimrc
"   last changed:
"   Modified: 2011 May 31
" ----------------------------------------------------------------------
" Environment               {{{
" ----------------------------------------------------------------------
    " Setup Bundle Support  {{{
    " ------------------------------------------------------------------
        " This allows all vim plugins to be stored in individual subdirectories 
        " of the vim runtime 'bundles' subdirectory. See pathogen docs for 
        " details.
        " filetype off
        "runtime! autoload/pathogen.vim
        call pathogen#runtime_append_all_bundles()
        call pathogen#helptags()
        filetype plugin indent on
        " comment out the following to activate the haskell conceal plugin
        let g:no_haskell_conceal = 1
    " }}}
    " Filetype Settings     {{{
    " ------------------------------------------------------------------
    " }}}
    " Colors                {{{
    " ------------------------------------------------------------------
        if has('gui_running')
            set background=light
        else
            set background=dark
        endif
        let g:solarized_visibility="low"
        syntax enable               " syntax highlighting on
        colorscheme solarized       " detects background value (light/dark)
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
    " Environment           {{{
    " ------------------------------------------------------------------
        set nocompatible            " explicitly leave vi-compatibility mode
        set encoding=utf-8          " unicode encoding by default
        set title                   " show title in terminal
        set clipboard+=unnamed      " unnamed buffer set to gui register *
                                    " so that dd == "*dd
        set hidden                  " buffers hidden, not closed, when abandoned
        v:vimrc>702 ? set undoreload=10000
                                    " save the whole buffer for undo when
                                    " reloading it.  This applies to the ":e!"
                                    " command and reloading for when the buffer
                                    " buffer changed outside of Vim.
        set noexrc                  " don't use local version of .(g)vimrc, .exrc
    " }}}
    " Compatible Options    {{{
    " ------------------------------------------------------------------
        set cpoptions=aABceFsmq
        "             |||||||||
        "             ||||||||`---- When joining lines, leave the cursor 
        "             ||||||||      between joined lines
        "             ||||||||
        "             |||||||`----- When a new match is created (showmatch) 
        "             |||||||       pause for .5
        "             |||||||
        "             ||||||`------ Set buffer options when entering the buffer
        "             ||||||
        "             |||||`------- :write command updates current file name
        "             |||||
        "             ||||`-------- Automatically add <CR> to the last line 
        "             ||||          when using :@r
        "             ||||
        "             |||`--------- Searching continues at the end of the match 
        "             |||           at the cursor position
        "             |||
        "             ||`---------- A backslash has no special meaning in
        "             ||            mappings
        "             ||
        "             |`----------- :write updates alternative file name
        "             |
        "             `------------ :read updates alternative file name
    " }}}
    " }}}
" Vim UI                    {{{
" ----------------------------------------------------------------------
    " Insert Mode           {{{
    " ------------------------------------------------------------------
        set backspace=indent,eol,start " make backspace more flexible
    " }}}
    " Search                {{{
    " ------------------------------------------------------------------
        set hlsearch                " highlight searched for phrases
        set incsearch               " BUT do highlight as you type
        set ignorecase              " case insensitive search for lowercase...
        set smartcase               " ...but if mixed case, go case-sensitive
    " }}}
    " Location Indicators   {{{
    " ------------------------------------------------------------------
        set nocursorcolumn          " highlight the current column, off
        set cursorline              " highlight current line
        v:vimrc>702 ? set colorcolumn=81
        set number                  " turn on line numbers
        set numberwidth=5           " we are good up to 99999 lines
        v:vimrc>702 ? set relativenumber " use relative line numbers
    " }}}
    " Screen Drawing        {{{
    " ------------------------------------------------------------------
        set linespace=0             " don't insert extra pixel lines betweens rows
        set lazyredraw              " do not redraw while running macros
        set ttyfast                 " fast redraw screen
    " }}}
    " Character Display     {{{
    " ------------------------------------------------------------------
        set list                " show nonprinting chars (set with listchars)
        set listchars=eol:¬,extends:»,tab:▸\ ,trail:›
        set noshowmatch         " don't match brackets (using matchparen instd)
                                " see 'set matchtime' if using showmatch
    " }}}
    " Navigation, Movement  {{{
    " ------------------------------------------------------------------
        set nostartofline       " cursor can stay on blank characters
        set scrolloff=5         " keep 10 lines (top/bottom) for scope 
        " virtualedit (below) severely conflicts with autoformating of wrapped, 
        " indented lines (non comment indented lines)
        " set virtualedit=all     " cursor past end of line... danger }}}
    " Completion            {{{
    " ------------------------------------------------------------------
        " set completeopt=      " don't use a pop up menu for completions
        set wildchar=<Tab>      " type to start wildcard expansion in the
                                " command-line (tab is default)
        set wildmenu            " command-line completion enhanced mode
        "set wildmode=list:longest  " When more than one match, list all and
                                " complete till longest common string.
        set wildmode=longest:full   " complete till longest common string
                                " if this doesn't result in a longer string,
                                " use the next part (also start 'wildmenu')
    " }}}
    " Status Indicators     {{{
    " ------------------------------------------------------------------
        set showcmd             " show the command being typed
        set showmode            " show insert, replace, visual mode indicator
        set laststatus=2        " always show the status line
        set visualbell          " blink
        set report=0            " report this or greater number of changed lines
        set ruler               " always show current positions along the bottom
    " }}}
    " Short Message Format  {{{
    " ------------------------------------------------------------------
        " shortens messages to avoid 'press a key' prompt
        set shortmess=aOtsT  
        "             |||||
        "             ||||`--T- truncate other messages in the middle if they
        "             ||||      are too long to fit on the command line.
        "             ||||      "..." will appear in the middle.
        "             ||||
        "             |||`---s- no "search hit BOTTOM, continuing at TOP" or
        "             |||       "search hit TOP, continuing at BOTTOM" messages
        "             |||
        "             ||`----t- truncate file message at start if it is too long
        "             ||        to fit on command-line, "<" will appear in the
        "             ||        left most column.  Ignored in Ex mode.
        "             ||
        "             |`-----O- message for reading a file overwrites previous
        "             |         message. Also for quickfix message (eg, ":cn").
        "             |
        "             `------a- all of the following abbreviations
        "                       f = use "(3 of 5)" not "(file 3 of 5)"
        "                       i = use "[noeol]" not "[Incomplete last line]"
        "                       l = use "999L, 888C" not "999 lines, " etc.
        "                       m = use "[+]" not "[Modified]"
        "                       n = use "[New]" not "[New File]"
        "                       r = use "[RO]" not "[readonly]"
        "                       w = use "[w]" not "written" for file write mssg
        "                       a = nd "[a]" not "appended" for ':w >> file' cmd
        "                       x = use "[dos]" not "[dos format]", "[unix]" not
        "                           "[unix format]" & "[mac]" not "[mac format]"
    " }}}
    " Statusline Format     {{{
    " ------------------------------------------------------------------
        "set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L,\ col\ %c)
        "set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%]\ [\ %05l\ %03v\ ]
        set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%]\ line:%l\/%L\ col:%v
        "              | | | | |  |   |      |  |     |    |
        "              | | | | |  |   |      |  |     |    `- current column
        "              | | | | |  |   |      |  |     |
        "              | | | | |  |   |      |  |     `------ current line
        "              | | | | |  |   |      |  |
        "              | | | | |  |   |      |  `------------ % into file
        "              | | | | |  |   |      |
        "              | | | | |  |   |      `--------------- current syntax in 
        "              | | | | |  |   |                       square brackets
        "              | | | | |  |   |
        "              | | | | |  |   `---------------------- current fileformat
        "              | | | | |  |
        "              | | | | |  `-------------------------- number of lines
        "              | | | | |
        "              | | | | `----------------------------- preview flag in
        "              | | | |                                square brackets
        "              | | | |
        "              | | | `------------------------------- help flag in
        "              | | |                                  square brackets
        "              | | |
        "              | | `--------------------------------- readonly flag in
        "              | |                                    square brackets
        "              | |
        "              | `----------------------------------- modified flag in
        "              |                                      square brackets
        "              |
        "              `------------------------------------- full path to file
        "                                                     in the buffer
    " }}}
    " Folding               {{{
    " ------------------------------------------------------------------
        set foldenable          " Turn on folding
        set foldmethod=marker   " Fold on the marker
        function! MyFoldText()
            let line = getline(v:foldstart)
            let nucolwidth = &fdc + &number * &numberwidth
            let windowwidth = winwidth(0) - nucolwidth - 3
            let foldlinecount = v:foldend - v:foldstart
            " expand tabs into spaces
            let onetab = strpart('          ', 0, &tabstop)
            let line = substitute(line, '\t', onetab, 'g')
            let line = strpart(line, 0, windowwidth - 2 -len(foldlinecount))
            let fillcharcount = windowwidth - len(line) - len(foldlinecount) - 4
            let fillchars = repeat(" ",fillcharcount)
            return line . '…' . fillchars . '…' . foldlinecount
        endfunction
        set foldtext=MyFoldText()
        set foldlevel=100
    " }}}
    " Gui Options           {{{
    " ------------------------------------------------------------------
        if has('gui_running')
            " Basics        {{{
            " ----------------------------------------------------------
            " set transparency=4
            set columns=88              " inc. width of number/folding columns
            set lines=55                " perfect size for me
            set mousehide               " hide the mouse cursor when typing
            set guioptions=ace
            "              ||
            "              |`---------- use simple dialogs rather than pop-ups
            "              |
            "              `----------- use GUI tabs, not console style tabs
            set guioptions-=mTlLrR
            "               ||||||
            "               |||||`----- remove righthand scrollbar
            "               |||||
            "               ||||`------ ?
            "               ||||
            "               |||`------- ?
            "               |||
            "               ||`-------- ?
            "               ||
            "               |`--------- remove toolbar
            "               |
            "               `---------- remove gui menu
            "
            " }}}
            " Fonts         {{{
            " ----------------------------------------------------------
                if has("gui_gtk2")
                    "set guifont=Terminus\ 10 " default for gvim in arch/xmonad
                    "set guifont=DejaVu\ Sans\ Mono\ 10 " default for gvim in arch/xmonad
                    set guifont=LetterGothicMono\ 12 " default for gvim in arch/xmonad
                    set guifont=Terminus\ 10 " default for gvim in arch/xmonad
                    set guifont=DejaVu\ Sans\ Mono\ 10 " default for gvim in arch/xmonad
                    set guifont=LetterGothicMono\ 12 " default for gvim in arch/xmonad
                elseif has("gui_photon")
                    set guifont=Terminus:s10
                elseif has("gui_kde")
                    set guifont=Terminus/10/-1/5/50/0/0/0/1/0
                elseif has("x11")
                    " set guifont=-*-terminus-medium-*-*-*-10-*-*-*-*-*-*-*
                    set guifont=xft:LetterGothicMono-Light:pixelsize=16
                elseif has("gui_macvim")
                    "set guifont=Terminus:h20
                    set guifont=LetterGothicMono\ Light:h18
                else
                    set guifont=Terminus:h10:cDEFAULT
                endif
                map <F8> <ESC>:set guifont=Consolas:h8<CR>
                map <F9> <ESC>:set guifont=Consolas:h10<CR>
                map <F10> <ESC>:set guifont=Consolas:h12<CR>
                map <F11> <ESC>:set guifont=Consolas:h16<CR>
                map <F12> <ESC>:set guifont=Consolas:h20<CR>
                "set guifont=DejaVuSansMono:h40
                "set guifont=SkyHookMono:h50
                " 20, 24, 43
            " }}}
            " Mac Specific  {{{
            " ----------------------------------------------------------
            if has("gui_macvim")
                set transparency=4
                macmenu &File.New\ Tab key=<nop>
                map <leader>t <Plug>PeepOpen
            end
            " }}}
        else
            " Nongui opts   {{{
            " ----------------------------------------------------------
            " set nocursorline              " cursorline in console mode...
            " }}}
        endif
    " }}}
    " }}}
" Text Formatting/Layout    {{{
" ----------------------------------------------------------------------
    " Wrap and Indent       {{{
    " ------------------------------------------------------------------
        set wrap                " soft wrap long lines
        set textwidth=79        " maximum width of text line during insert 
        set autoindent          " copy indent from current when starting a new line
    " }}}
    " Tabs                  {{{
    " ------------------------------------------------------------------
        set expandtab           " convert tabs into spaces
        set tabstop=8           " real tabs should be 8 (unix default)
        set softtabstop=4       " tab conversion to number of spaces
        set shiftwidth=4        " auto-indent amount when using cindent, >>, <<
        set shiftround          " when at 3 spaces, and I hit > ... go to 4, not 5
        " C/C++ {{{2
        au FileType cc,cpp map <F8> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

        " js {{{2
        au FileType js set foldmethod=indent
        au FileType js set foldmarker={,} 
        au FileType js set foldtext=substitute(getline(v:foldstart+1),'{.*','{...}',)
        au FileType js set foldlevelstart=4
        au FileType js setlocal noexpandtab noautoindent nocindent nosmartindent shiftwidth=4 softtabstop=4 tabstop=4

        " C# {{{2
        au FileType cs set foldmethod=indent
        au FileType cs set foldmarker={,} 
        au FileType cs set foldtext=substitute(getline(v:foldstart+1),'{.*','{...}',)
        au FileType cs set foldlevelstart=4
        au FileType cs map <F8> :!ctags --recurse --extra=+fq --fields=+ianmzS --c\#-kinds=cimnp .<CR>

        " Mail {{{2
        autocmd BufNewFile,BufRead /tmp/mutt-* set filetype=mail
        au FileType mail setlocal spell spelllang=en_us
        au FileType mail set tw=66 autoindent expandtab formatoptions=tcqn
        au FileType mail set list listchars=tab:»·,trail:·
        au FileType mail set comments=nb:>
        au FileType mail vmap D dO[...]^[
        " go to a good starting point
        au FileType mail silent normal /--\s*$^MO^[gg/^$^Mj

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


    " }}}
    " Format Options        {{{
    " ------------------------------------------------------------------
        set formatoptions=tcroqwan1b
        "set formatoptions=aw
        " tcq   default options if not set here
        "
        " t +   Auto-wrap text using textwidth
        " c +   Auto-wrap comments using textwidth, inserting the current
        "       comment leader automatically.
        " r +   Automatically insert the current comment leader after hitting
        "       <Enter> in Insert mode.
        " o +   Automatically insert the current comment leader after hitting 
        "       'o' or 'O' in Normal mode.
        " q +   Allow formatting of comments with "gq".
        "       Note that formatting will not change blank lines or lines 
        "       containing only the comment leader.  A new paragraph starts
        "       after such a line, or when the comment leader changes.
        " w +   Trailing white space indicates a paragraph continues in the
        "       next line. A line that ends in non-whitespace ends a paragraph.
        " a +   Automatic formatting of paragraphs.  Every time text is
        "       inserted or deleted the paragraph will be reformatted.
        "       See |auto-format|.
        "       When the 'c' flag is present this only happens for recognized
        "       comments.
        " n +   When formatting text, recognize numbered lists.  This actually
        "       uses the 'formatlistpat' option, thus any kind of list can be
        "       used. The indent of the text after the number is used for the 
        "       next line. The default is to find a number, optionally followed
        "       by '.', ':', ')', " ']' or 'curly bracket'.  Note that 
        "       'autoindent' must be set too.  Doesn't work well with "2".
        "           Example: >
        "               1. the first item
        "                  wraps
        "               2. the second item
        " 2 -   When formatting text, use the indent of the second line of a 
        "       paragraph for the rest of the paragraph, instead of the indent
        "       of the first line. This supports paragraphs in which the first
        "       line has a different indent than the rest.  Note that 
        "       'autoindent' must be set too.  Example: >
        "
        "                   first line of a paragraph
        "           second line of the same paragraph
        "           third line.
        " v -   Vi-compatible auto-wrapping in insert mode: Only break a line
        "       at a blank that you have entered during the current insert 
        "       command.  (Note: this is not 100% Vi compatible.  Vi has some
        "       "unexpected features" or bugs in this area.  It uses the screen
        "       column instead of the line column.)
        " b +   Like 'v', but only auto-wrap if you enter a blank at or before
        "       the wrap margin.  If the line was longer than 'textwidth' when
        "       you started the insert, or you do not enter a blank in the
        "       insert before reaching 'textwidth', Vim does not perform
        "       auto-wrapping.
        " l -   Long lines are not broken in insert mode: When a line was
        "       longer than 'textwidth' when the insert command started, Vim
        "       does not automatically format it.
        " m -   Also break at a multi-byte character above 255.  This is useful
        "       for Asian text where every character is a word on its own.
        " M -   When joining lines, don't insert a space before or after a
        "       multi-byte character.  Overrules the 'B' flag.
        " B -   When joining lines, don't insert a space between two multi-byte
        "       characters.  Overruled by the 'M' flag.
        " 1 +   Don't break a line after a one-letter word. It's broken before
        "       it instead (if possible).
    " }}}
    " }}}
" Maps & Functions          {{{
" ----------------------------------------------------------------------
    " Leader / nops         {{{
    " ------------------------------------------------------------------
        " redefine leader from default backslash
        let mapleader = ","
        " turn off manual key
        nnoremap K <nop>
    " }}}
    " Autocommands          {{{
    " ------------------------------------------------------------------
        " au! BufRead,BufNewFile *.txt set filetype=markdown
        " au! BufRead,BufNewFile *.mkd set filetype=pdc
        " au! BufRead,BufNewFile *.markdown set filetype=pdc
        " au FocusLost * :wa             " save when losing focus
        "au FocusLost * set nocursorline nocursorcolumn
        "au FocusGained * set cursorline cursorcolumn

    " }}}
    " Navigation            {{{
    " ------------------------------------------------------------------
        " Use only hjkl for navigation, not cursor keys
        nnoremap <up> <nop>
        nnoremap <down> <nop>
        nnoremap <left> <nop>
        nnoremap <right> <nop>
        " allow navigation on wrapped lines
        nnoremap j gj
        nnoremap k gk
        " Easy buffer navigation
        map <C-h> <C-w>h
        map <C-j> <C-w>j
        map <C-k> <C-w>k
        map <C-l> <C-w>l
        map <leader>w <C-w>v<C-w>l
    " }}}
    " Search, Copy, Replace {{{
    " ------------------------------------------------------------------
        " kill search highlight with leader-slash
        map <leader>/ :noh<cr>
        " immediately enter very-magic mode when searching
        "nnoremap / /\v
        "vnoremap / /\v
        " make Y conform to C/D conventions and act on cursor-to-eol
        nnoremap Y y$
    " }}}
    " Highlighting          {{{
    " ------------------------------------------------------------------
        " Syntax              {{{
        " Show syntax highlighting groups for word under cursor
        nmap <C-S-P> :call <SID>SynStack()<CR>
        function! <SID>SynStack()
            if !exists("*synstack")
                return
            endif
            echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
        endfunc
        " }}}
        " Hex Colors        {{{
        if exists('*HexHighlight()')
            nmap <leader>h :call HexHighlight()<Return>
        endif
        " }}}
    " }}}
    " Smart Fullscreen      {{{
    " ------------------------------------------------------------------
    " terminus 20/32/??

        "set guifont=DejaVu\ Sans\ Mono:h20
        let s:guifontprefix="DejaVu\ Sans\ Mono:h"
        let g:fullscreen_defaultfontsize=20
        let g:fullscreen_minfontsize=36
        let g:fullscreen_maxfontsize=36

        "set guifont=Terminus\ Medium:h20
        let s:guifontprefix="Terminus\ Medium:h"
        let g:fullscreen_defaultfontsize=20
        let g:fullscreen_minfontsize=32
        let g:fullscreen_maxfontsize=32

        let g:fullscreen_defaultcolumns=88 " 80 cols plus room for number col

        "set guifont=Menlo:h14
        let s:guifontprefix="Menlo:h"
        let g:fullscreen_defaultfontsize=14
        let g:fullscreen_minfontsize=20
        let g:fullscreen_maxfontsize=36

        "set guifont=LetterGothicMono\ Light:h14
        let s:guifontprefix="LetterGothicMono\ Light:h"
        let g:fullscreen_defaultfontsize=14
        let g:fullscreen_minfontsize=36     " 1.8 (for letterg, deja uses this
                                            " for max width
        let g:fullscreen_maxfontsize=44     " 2.2


        function!  SmartFullScreen(fullscreenmode,indicatorsmode)
            " fullscreenmode can be "min", "max", "default"
            " indicatorsmode can be "off", "on"
            if a:fullscreenmode == "default"
                exe "let s:fullguifont = \"" . s:guifontprefix .
                            \ g:fullscreen_defaultfontsize . "\""
                let s:fulloptions = "maxvert,maxhorz,background:#00000000"
                let s:fullcolumns = g:fullscreen_defaultcolumns
                let s:sfs_scrolloff=2
            elseif a:fullscreenmode == "max"
                exe "let s:fullguifont = \"" . s:guifontprefix .
                            \ g:fullscreen_maxfontsize . "\""
                let s:fulloptions = "maxvert,maxhorz,background:#00000000"
                let s:fullcolumns = g:fullscreen_defaultcolumns
                let s:sfs_scrolloff=2
            elseif a:fullscreenmode == "min"
                exe "let s:fullguifont = \"" . s:guifontprefix .
                            \ g:fullscreen_minfontsize . "\""
                let s:fulloptions = "maxvert,background:#00000000"
                let s:fullcolumns = g:fullscreen_defaultcolumns
                let s:sfs_scrolloff=3
            endif
            if !exists("w:lastfullscreenmode")
                let w:lastfullscreenmode = ""
            endif
            if &fullscreen && a:fullscreenmode == w:lastfullscreenmode
                " we are full screen and the function called is the last called 
                " smartfullscreen mode, so we return to normal window mode
                if exists("w:nonFullScreenFont")
                    exe "let &guifont=\"" . w:nonFullScreenFont . "\""
                else
                    let &guifont= "LetterGothicMono\ Light:h22"
                endif
                if exists("w:nonFullScreenTabline")
                    exe "set showtabline=" . w:nonFullScreenTabline
                else
                    set showtabline=1
                endif
                if exists("w:nonFullScreenTransparency")
                    exe "set transparency=" . w:nonFullScreenTransparency
                else
                    set transparency=5
                endif
                if exists("w:nonFullScreenColumns")
                    exe "set columns=" . w:nonFullScreenColumns
                else
                    set columns=88
                endif
                if exists("w:nonFullScreenLines")
                    exe "set lines=" . w:nonFullScreenLines
                else
                    set lines=40
                endif
                if exists("w:nonFullScreenLines")
                    exe "set scrolloff=" . w:nonFullScreenScrollOff
                else
                    set scrolloff=5
                endif
                set nofullscreen
                let w:lastfullscreenmode = ""
            elseif &fullscreen && a:fullscreenmode != w:lastfullscreenmode
                " fullscreen already but switching modes
                set nofullscreen
                exe "set fuoptions=" . s:fulloptions
                let &guifont = s:fullguifont
                exe "set columns=" . s:fullcolumns
                exe "set scrolloff=" . s:sfs_scrolloff
                set showtabline=0
                set transparency=0
                set fullscreen
                let w:lastfullscreenmode = a:fullscreenmode
            else
                " not yet fullscreen, applying a mode
                set nofullscreen
                exe "set fuoptions=" . s:fulloptions
                let w:nonFullScreenFont = &guifont
                let w:nonFullScreenColumns = &columns
                let w:nonFullScreenLines = &lines
                let w:nonFullScreenTransparency = &transparency
                let w:nonFullScreenTabline = &showtabline
                let w:nonFullScreenScrollOff = &scrolloff
                let &guifont = s:fullguifont
                exe "set columns=" . s:fullcolumns
                exe "set scrolloff=" . s:sfs_scrolloff
                set showtabline=0
                set transparency=0
                set fullscreen
                let w:lastfullscreenmode = a:fullscreenmode
            endif
        endfunction
        nnoremap <F1> :call SmartFullScreen("default","on")<CR>
        inoremap <F1> <ESC>:call SmartFullScreen("default","on")<CR>a
        vnoremap <F1> <ESC>:call SmartFullScreen("default","on")<CR>
        nnoremap <F2> :call SmartFullScreen("max","on")<CR>
        inoremap <F2> <ESC>:call SmartFullScreen("max","on")<CR>a
        vnoremap <F2> <ESC>:call SmartFullScreen("max","on")<CR>
    " }}}
    " Toggle Indicators     {{{
    " ------------------------------------------------------------------
    " We'll use &number and &relativenumber (mutually eclusive) as proxies for 
    " the toggle state of all our preferred indicator UI
        function! ToggleIndicators()
            if (&number || &relativenumber) " turn off indicators
                let b:IndicatorNumber = 0
                let b:IndicatorRelativeNumber = 0
                let b:IndicatorCursorColumn = 0
                let b:IndicatorCursorLine = 0
                let b:IndicatorColorColumn = 0
                let b:IndicatorLastStatus = 0
                if &number
                    let b:IndicatorNumber = 1
                    set nonumber
                else " must be relativenumber
                    let b:IndicatorRelativeNumber = 1
                    set norelativenumber
                endif
                if &cursorcolumn==1
                    let b:IndicatorCursorColumn = 1
                    set nocursorcolumn
                endif
                if &cursorline
                    let b:IndicatorCursorLine = 1
                    set nocursorline
                endif
                if &colorcolumn > 0
                    let b:IndicatorColorColumn = &colorcolumn
                    set colorcolumn=0
                endif
                if &laststatus > 0
                    let b:IndicatorLastStatus = &laststatus
                    set laststatus=0
                endif
            else                            " turn on indicators
                if b:IndicatorNumber
                    set number
                else " must be relativenumber
                    set relativenumber
                endif
                if b:IndicatorCursorColumn
                    set cursorcolumn
                endif
                if b:IndicatorCursorLine
                    set cursorline
                endif
                if b:IndicatorColorColumn > 0
                    exe "set colorcolumn=" . b:IndicatorColorColumn
                endif
                if b:IndicatorLastStatus > 0
                    exe "set laststatus=" . b:IndicatorLastStatus
                endif
            endif
        endfunction
        nnoremap <F3> :call ToggleIndicators()<CR>
        inoremap <F3> <ESC>:call ToggleIndicators()<CR>a
        vnoremap <F3> <ESC>:call ToggleIndicators()<CR>
    " }}}
    " Toggle CursorColumn   {{{
        nnoremap <F4> :set invcursorcolumn<CR>
        inoremap <F4> <ESC>:set invcursorcolumn<CR>a
        vnoremap <F4> <ESC>:set invcursorcolumn<CR>
    " }}}
    " Font Scaling          {{{
        let s:pattern = '^\(.*\:h\)\([1-9][0-9]*\)$'
        let s:minfontsize = 6
        let s:maxfontsize = 100
        function! AdjustFontSize(amount)
        if has("gui_running")
            let fontname = substitute(&guifont, s:pattern, '\1', '')
            let cursize = substitute(&guifont, s:pattern, '\2', '')
            let newsize = cursize + a:amount
            if (newsize >= s:minfontsize) && (newsize <= s:maxfontsize)
            let newfont = fontname . newsize
            let &guifont = newfont
            endif
        else
            echoerr "Command requires gui version of vim to run."
        endif
        endfunction
        function! LargerFont()
            call AdjustFontSize(1)
        endfunction
        command! LargerFont call LargerFont()
        function! SmallerFont()
            call AdjustFontSize(-1)
        endfunction
        command! SmallerFont call SmallerFont()
        let i=0
        function! ScaleFont()
            if &columns > 1
            let origcol=&columns
            let thiscol=&columns
            for i in range(1, 3)
                echo "count is" i " and columns =" thiscol
                if thiscol > 88
                    call AdjustFontSize(1)
                    let thiscol=&columns
                    redrawstatus!
                    redraw!
                endif
            endfor
            else
                echo "too big"
            endif
            let w:DebugThisA = thiscol
            let w:DebugThisB = &columns
        endfunction
    " }}}
    " Toggle Background     {{{
    " ------------------------------------------------------------------
    " }}}
    " Plugins               {{{
    " ------------------------------------------------------------------
    "nmap <unique> <F7> <Plug>ToggleBackground
    "imap <unique> <F7> <Plug>ToggleBackground
    "vmap <unique> <F7> <Plug>ToggleBackground
    "if exists("*togglebg#map")
        call togglebg#map("<F7>")
    "endif
        " timestamp.vim
        let g:timestamp_username="Ethan Schoonover"
        let g:timestamp_modelines=15
        let g:timestamp_rep='%Y %b %d'
        let g:timestamp_regexp='\c\%(\<\%(last \)\?\%(changed?\|modified\):\s\+\)\@<=\%(\d\{4}\D.\{-}\d\{2}\|TIMESTAMP\)'
    " }}}
    " }""
