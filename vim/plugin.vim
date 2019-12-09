"--------------------------------------------------------
" dein.vim関係
" http://d.hatena.ne.jp/raiden325/20160716/1468641998 参考

" プラグインがインストールされるディレクトリ
let s:dein_dir = expand('~/.cache/dein')
" dein.vim本体
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

" dein.vim がなければ github から落とす
if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh'
    execute '!sh ./installer.sh ' . s:dein_dir 
    execute '!rm ./installer.sh'
  endif
  execute 'set runtimepath+=' . fnamemodify(s:dein_repo_dir, ':p')
endif

" 設定開始
if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  " プラグインリストを収めた TOML ファイル
  " ~/.vim/dein.toml,deinlazy.tomlを用意する
  let g:rc_dir    = expand('~/.vim')
  let s:toml      = g:rc_dir . '/dein.toml'
  let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'

  " TOML を読み込み、キャッシュしておく
  call dein#load_toml(s:toml,      {'lazy': 0})
  call dein#load_toml(s:lazy_toml, {'lazy': 1})

  " 設定終了
  call dein#end()
  call dein#save_state()
endif

" もし、未インストールものものがあったらインストール
if dein#check_install()
  call dein#install()
endif

filetype plugin on
filetype plugin indent on
" :call dein#update() でプラグインのアップデート

"-----------------------------------------------------------
