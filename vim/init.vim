source ~/dotfiles/vim/plugin.vim
"--------------------------------------------------------
" コマンドスニペット
" :col :cnew // quickfixを新旧に移動する
" :chi // quickfixのリストを表示
" :vim {pattern} `git ls-files` // git で管理されてる範囲内でgrep
" <Space> E // defxウィンドウをトグる
"
command -nargs=1 Lvimg lvim <f-args> `git ls-files`
let mapleader = "\<Space>"
nnoremap <Space> <Nop>

set showcmd
set number
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter * set norelativenumber
augroup END
syntax enable
set expandtab
set tabstop<
set softtabstop=2
set shiftwidth=2
set hlsearch
set autoindent
set laststatus=2
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif
set shortmess+=I
set ignorecase
set smartcase
set wrapscan
set showmatch
set wildmenu
set formatoptions+=mM
set hidden
set mouse=a
set clipboard+=unnamedplus
autocmd VimResized * wincmd =
set pumblend=20
autocmd FileType * set winblend=20

"-----------------------------------------------------------

" http://cimadai.hateblo.jp/entry/20080325/1206459666
" □とか○の文字があってもカーソル位置がずれないようにする
if exists('&ambiwidth')
  set ambiwidth=double
endif
" 改行コードの自動認識
set fileformats=unix,mac,dos
set encoding=utf-8
set fileencodings=utf-8,euc-jp

"-----------------------------------------------------------

" Makefileでタブスペースをタブ文字にする
let _curfile=expand("%:r")
if _curfile == 'Makefile'
  set noexpandtab
endif

"-----------------------------------------------------------

augroup cpp-path
  autocmd!
  autocmd FileType cpp setlocal path=.,/usr/include,/usr/local/include,/usr/include/c++/
augroup END

"-----------------------------------------------------------

"" markdown 
"  autocmd BufRead,BufNewFile *.mkd  set filetype=markdown
"  autocmd BufRead,BufNewFile *.md  set filetype=markdown
"  " Need: kannokanno/previm
"  nnoremap <silent> <C-p> :PrevimOpen<CR> " Ctrl-pでプレビュー
"  " 自動で折りたたまないようにする
"  let g:vim_markdown_folding_disabled=1
"  let g:vim_markdown_math = 1
"  let g:previm_open_cmd = 'luakit -n'
"  let g:previm_enable_realtime = 1
"" 

"-----------------------------------------------------------
"http://yt-siden-memo.tumblr.com/post/118939556075
"インクルードガードUUID生成
" C/C++ insert UUID based include guard

function! s:insert_include_guard()
  let s:uuid=system('uuidgen')
  let s:uuid=strpart(s:uuid, 0, strlen(s:uuid)-1)
  let s:uuid=substitute(s:uuid, '[a-f]', '\u\0', 'g')
  let s:uuid=substitute(s:uuid, '\-', '_', 'g')
  let s:uuid='UUID_'.s:uuid
  call append(0, '#ifndef '.s:uuid)
  call append(1, '#define '.s:uuid)
  call append('$', '#endif //'.s:uuid)
endfunction
command! -nargs=0 InsertIncludeGuard call s:insert_include_guard()

" key is file extension, value is alternate file extension.
let g:next_alter#pair_extension = { 
            \ 'c'   : [ 'h' ],
            \ 'C'   : [ 'H' ],
            \ 'cc'  : [ 'h' ],
            \ 'CC'  : [ 'H', 'h'],
            \ 'cpp' : [ 'h', 'hpp' ],
            \ 'CPP' : [ 'H', 'HPP' ],
            \ 'cxx' : [ 'h', 'hpp' ],
            \ 'CXX' : [ 'H', 'HPP' ],
            \ 'h'   : [ 'c', 'cpp', 'cxx' ],
            \ 'H'   : [ 'C', 'CPP', 'CXX' ],
            \ 'hpp' : [ 'cpp', 'cxx'],
            \ 'HPP' : [ 'CPP', 'CXX'],
            \ }
" this list shows search directory to find alternate file.
let g:next_alter#search_dir = [ '.' , '..', './include', '../include' ]

" this is used when it opens alternate file buffer.
let g:next_alter#open_option = 'vertical topleft'

"------------------------------------------------------------
" terminal
packadd termdebug
autocmd BufRead,BufNewFile *.rs setfiletype rust
autocmd FileType rust let termdebugger="rust-gdb"
let g:termdebug_wide = 163

"------------------------------------------------------------
" quickfix
"au QuickFixCmdPost *grep* cwindow
"------------------------------------------------------------
" vimgrep
"vim {pattern} `git ls-files`
"------------------------------------------------------------

autocmd FileType cpp map <F5> :w<CR>:!g++ % -o %:r && ./%:r<CR>
autocmd FileType cpp map <C-F5> :w<CR>:!g++ % && zsh ./test%:r.sh<CR>
autocmd FileType cpp map <S-F5> :w<CR>:e ./test%:r.sh<CR>:w<CR>
autocmd FileType c map <F5> :w<CR>:!gcc % -o %:r && ./%:r<CR>
autocmd FileType c map <C-F5> :w<CR>:!gcc % && zsh ./test%:r.sh<CR>
autocmd FileType c map <S-F5> :w<CR>:e ./test%:r.sh<CR>:w<CR>
autocmd FileType rs map <F5> :w<CR>:!cargo run<CR>
autocmd FileType py map <F5> :w<CR>:!python %<CR>

autocmd FileType markdown map <F5> :w<CR>:!pandoc % -o %:r.pdf -V documentclass=ltjarticle --pdf-engine=lualatex -N -H preamble_%:r.tex && apvlv %:r.pdf &<CR>

autocmd FileType cpp set keywordprg=/usr/bin/cppman

" http://daikishimada.github.io/vim_python_crash.html
" Disable omnifunc in Python
if !exists('g:neocomplete#sources#omni#input_patterns')
    let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.python = ''

" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" rusty-tags
autocmd BufRead *.rs :setlocal tags=./rusty-tags.vi;/,$RUST_SRC_PATH/rusty-tags.vi
autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!

nmap <Leader><Leader>w [window]
nnoremap [window]h <C-w>h
nnoremap [window]j <C-w>j
nnoremap [window]k <C-w>k
nnoremap [window]l <C-w>l
nnoremap [window]H <C-w>h:bd<CR>
nnoremap [window]J <C-w>j:bd<CR>
nnoremap [window]K <C-w>k:bd<CR>
nnoremap [window]L <C-w>l:bd<CR>

"highlight IncSearch ctermbg=104
"highlight Search ctermbg=104

" TidalCycles
augroup Tidal
autocmd BufRead,BufNewFile *.tidal  set filetype=tidal
autocmd FileType tidal command! TidalInit call s:TidalInit()
function! s:TidalInit()
  term stack ghci
  file tidal
endfunction

autocmd FileType tidal command! -range=0 TidalRun call s:TidalRun(<count>, <line1>, <line2>)
function! s:TidalRun(range_given, line1, line2)
  if a:range_given
    call term_sendkeys("tidal", join([":{\n", join(getline(a:line1, a:line2), "\n"), "\n", ":}\n"],''))
  else
    call term_sendkeys("tidal", join([getline("."), "\n"]))
  endif
endfunction
autocmd FileType tidal map <F5> :TidalRun<CR>
augroup END

" Rust
augroup Rust
autocmd FileType rust map <F5> :Cargo run<CR>
augroup END

" TypeScript
augroup TypeScript
autocmd BufRead,BufNewFile *.ts set filetype=typescript
autocmd FileType typescript command! TypeScriptRun call s:TypeScriptRun()
function! s:TypeScriptRun()
  !npx ts-node ./src/index.ts
endfunction
autocmd FileType typescript map <F5> :TypeScriptRun<CR>
augroup END

" Vue.js
augroup Vue
autocmd BufRead,BufNewFile *.vue set filetype=vue
augroup END

" Undo
if has('persistent_undo')
  set undodir=~/.vim/undo
  set undofile
endif

" http://koturn.hatenablog.com/entry/2018/02/10/170000
" 入力キーの辞書
let s:compl_key_dict = {
      \ char2nr("\<C-l>"): "\<C-x>\<C-l>",
      \ char2nr("\<C-n>"): "\<C-x>\<C-n>",
      \ char2nr("\<C-p>"): "\<C-x>\<C-p>",
      \ char2nr("\<C-k>"): "\<C-x>\<C-k>",
      \ char2nr("\<C-t>"): "\<C-x>\<C-t>",
      \ char2nr("\<C-i>"): "\<C-x>\<C-i>",
      \ char2nr("\<C-]>"): "\<C-x>\<C-]>",
      \ char2nr("\<C-f>"): "\<C-x>\<C-f>",
      \ char2nr("\<C-d>"): "\<C-x>\<C-d>",
      \ char2nr("\<C-v>"): "\<C-x>\<C-v>",
      \ char2nr("\<C-u>"): "\<C-x>\<C-u>",
      \ char2nr("\<C-o>"): "\<C-x>\<C-o>",
      \ char2nr('s'): "\<C-x>s",
      \ char2nr("\<C-s>"): "\<C-x>s"
      \}
" 表示メッセージ
let s:hint_i_ctrl_x_msg = join([
      \ '<C-l>: While lines',
      \ '<C-n>: keywords in the current file',
      \ "<C-k>: keywords in 'dictionary'",
      \ "<C-t>: keywords in 'thesaurus'",
      \ '<C-i>: keywords in the current and included files',
      \ '<C-]>: tags',
      \ '<C-f>: file names',
      \ '<C-d>: definitions or macros',
      \ '<C-v>: Vim command-line',
      \ "<C-u>: User defined completion ('completefunc')",
      \ "<C-o>: omni completion ('omnifunc')",
      \ "s: Spelling suggestions ('spell')"
      \], "\n")
function! s:hint_i_ctrl_x() abort
  echo s:hint_i_ctrl_x_msg
  let c = getchar()
  return get(s:compl_key_dict, c, nr2char(c))
endfunction
 
inoremap <expr> <C-x>  <SID>hint_i_ctrl_x()

if !has('nvim')
  tnoremap <C-j> <C-\><C-n>
endif

"--------------------------------------------------------
" バッファの大きさを最大化する
" https://qiita.com/grohiro/items/e3dbcc93510bc8c4c812
let s:toggle_window_size = 0
function! ToggleWindowSize()
  if s:toggle_window_size == 1
    exec "normal \<C-w>="
    let s:toggle_window_size = 0
  else
    :resize
    :vertical resize
    let s:toggle_window_size = 1
  endif
endfunction
nnoremap <F11> :call ToggleWindowSize()<CR>

"--------------------------------------------------------
" ColorSchemeの背景を透過させる
" https://qiita.com/s_of_p/items/87a9d787ff5506edab8e
autocmd VimEnter,ColorScheme * highlight Normal ctermbg=NONE
autocmd VimEnter,ColorScheme * highlight NonText ctermbg=NONE
autocmd VimEnter,ColorScheme * highlight TablineSel ctermbg=NONE
autocmd VimEnter,ColorScheme * highlight LineNr ctermbg=NONE
autocmd VimEnter,ColorScheme * highlight CursorLineNr ctermbg=NONE

" https://gist.github.com/ram535/b1b7af6cd7769ec0481eb2eed549ea23
" With this function you can reuse the same terminal in neovim.
" You can toggle the terminal and also send a command to the same terminal.

let s:monkey_terminal_window = -1
let s:monkey_terminal_buffer = -1
let s:monkey_terminal_job_id = -1

function! MonkeyTerminalOpen()
  " Check if buffer exists, if not create a window and a buffer
  if !bufexists(s:monkey_terminal_buffer)
    " Creates a window call monkey_terminal
    new monkey_terminal
    " Moves to the window the right the current one
    wincmd L
    let s:monkey_terminal_job_id = termopen($SHELL, { 'detach': 1 })

     " Change the name of the buffer to "Terminal 1"
     silent file Terminal\ 1
     " Gets the id of the terminal window
     let s:monkey_terminal_window = win_getid()
     let s:monkey_terminal_buffer = bufnr('%')

    " The buffer of the terminal won't appear in the list of the buffers
    " when calling :buffers command
    set nobuflisted
  else
    if !win_gotoid(s:monkey_terminal_window)
    sp
    " Moves to the window below the current one
    wincmd L   
    buffer Terminal\ 1
     " Gets the id of the terminal window
     let s:monkey_terminal_window = win_getid()
    endif
  endif
endfunction

function! MonkeyTerminalToggle()
  if win_gotoid(s:monkey_terminal_window)
    call MonkeyTerminalClose()
  else
    call MonkeyTerminalOpen()
  endif
endfunction

function! MonkeyTerminalClose()
  if win_gotoid(s:monkey_terminal_window)
    " close the current window
    hide
  endif
endfunction

function! MonkeyTerminalExec(cmd)
  if !win_gotoid(s:monkey_terminal_window)
    call MonkeyTerminalOpen()
  endif

  " clear current input
  call jobsend(s:monkey_terminal_job_id, "clear\n")

  " run cmd
  call jobsend(s:monkey_terminal_job_id, a:cmd . "\n")
  normal! G
  wincmd p
endfunction

" With this maps you can now toggle the terminal
nnoremap <silent> <Leader>t :call MonkeyTerminalToggle()<cr>
tnoremap <C-j> <C-\><C-n>

" C#
"autocmd BufRead,BufNewFile *.cs setfiletype csharp
let g:python_host_prog = '/usr/bin/python2'

set thesaurus=~/\.vim/thesaurus/thesaurus\.txt
