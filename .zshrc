export EDITOR=nvim

#KVS PATH
export KVS_DIR=~/local/kvs_osmesa
export path=($path $KVS_DIR/bin )
export KVS_GLEW_DIR=$KVS_DIR/local
export path=($path ~/.cargo/bin ~/.local/bin)
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export EDITOR="/usr/bin/vim"
export LANG

#コマンド実行時の時間をプロンプトに表示する
re-prompt() {
  zle .reset-prompt
  zle .accept-line
}

zle -N accept-line re-prompt

[ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null
#PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
#export GEM_HOME=$(ruby -e 'print Gem.user_dir')
eval "$(rbenv init -)"
MAC_Appricot="44:8A:5B:9E:E0:B0"
alias ls="ls -F --color=auto"
alias ipa="ip a"
alias mkdir="mkdir -p"
alias v="nvim"
alias vz="nvim ~/.zshrc"
alias vv="nvim ~/.vimrc"
alias vd="nvim ~/.vim/dein.toml"
alias vdl="nvim ~/.vim/dein_lazy.toml"
alias sz="source ~/.zshrc"
alias em="emacs -nw"
alias fehbg="feh --bg-scale"
alias glc="gcc -lglut -lGL -lGLU -lm -lglfw -lglm"
alias gl+="g++ -lglut -lGL -lGLU -lm -lglfw -lGLEW";
alias gdb="gdb -q"
alias pndjp="pandoc -V documentclass=ltjarticle --latex-engine=lualatex"
alias vis="nvim -S ./Session.vim"
alias open="xdg-open"
alias nocaps="setxkbmap -option ctrl:nocaps"
alias dirs="dirs -v"
alias nvf="nvr --remote-send \"<C-\\><C-n>:vs $1<CR>\""
function nvcd () { realpath $1 | xargs -I{} nvr -c "cd {} "}

#
#
autoload -Uz colors
setopt AUTO_CD
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

PROMPT=">"

# 補完
# 補完機能を有効にする
autoload -Uz compinit
compinit

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                   /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
setopt share_history

# コマンド履歴検索
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt extended_glob
setopt auto_pushd
setopt pushd_ignore_dups
setopt correct
bindkey -e

# C で標準出力をクリップボードにコピーする
# mollifier delta blog : http://mollifier.hatenablog.com/entry/20100317/p1
if which pbcopy >/dev/null 2>&1 ; then
    # Mac
    alias -g C='| pbcopy'
elif which xsel >/dev/null 2>&1 ; then
    # Linux
    alias -g C='| xsel --input --clipboard'
elif which putclip >/dev/null 2>&1 ; then
    # Cygwin
    alias -g C='| putclip'
fi

autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:*' formats '[%F{green}%b%f]'
zstyle ':vcs_info:*' actionformats '[%F{green}%b%f(%F{red}%a%f)]'
precmd() { vcs_info }
PROMPT='[%n@%m]${vcs_info_msg_0_} %{${fg[yellow]}%}%~%{${reset_color}%} [%*]
%(?.%B%F{green}.%B%F{blue})%\>%f%b'
RPROMPT=''

show_buffer_stack() {
  POSTDISPLAY="
stack: $LBUFFER"
  zle push-line-or-edit
}
zle -N show_buffer_stack
setopt noflowcontrol
bindkey '^Q' show_buffer_stack  

alias md2pdf='(){pandoc $1 -o $1{##*/%.*}.pdf -V documentclass=ltjarticle --pdf-engine=lualatex'

# Vivado など Java系の表示を正しくする
export _JAVA_AWT_WM_NONREPARENTING=1
# Vivado のセットアップコマンド
# sudo sh -c "export _JAVA_AWT_WM_NONREPARENTING=1 && ./xsetup"
#
# snippet
#
# エラーを出力しない (find で Permission deniedが表示されない)
# 2>/dev/null
