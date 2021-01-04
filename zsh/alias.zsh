alias ls="ls -F --color=auto"
alias l="exa -F --git"
alias ipa="ip a"
alias mkdir="mkdir -p"
alias v="nvim"
alias vz="nvim ~/dotfiles/zsh/.zshrc"
alias vv="nvim ~/dotfiles/vim/init.vim"
alias vd="nvim ~/dotfiles/vim/dein.toml"
alias vdl="nvim ~/dotfiles/vim/dein_lazy.toml"
alias sz="source ~/dotfiles/zsh/.zshrc"
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

alias gl="git log --graph --oneline --all --decorate=full -20 --date=short --format=\"%C(yellow)%h%C(reset) %C(magenta)[%ad]%C(reset)%C(auto)%d%C(reset) %s %C(cyan)@%an%C(reset)\" "

alias md2pdf='(){pandoc $1 -o $1{##*/%.*}.pdf -V documentclass=ltjarticle --pdf-engine=lualatex'

alias c="code -r"

alias open='powershell.exe /c start'
