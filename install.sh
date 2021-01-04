#!/bin/bash

dotfile_dir=`pwd`
echo $dotfile_dir
cd $HOME
mkdir -p .vim/
mkdir -p .config/nvim

ln -s $dotfile_dir/vim/init.vim $HOME/.vimrc
ln -s $dotfile_dir/vim/init.vim $HOME/.config/nvim/init.vim
ln -s $dotfile_dir/vim/dein.toml $HOME/.vim/dein.toml
ln -s $dotfile_dir/vim/dein_lazy.toml $HOME/.vim/dein_lazy.toml
ln -s $dotfile_dir/vim/settings.json $HOME/.vim/settings.json
ln -s $dotfile_dir/vim/coc-setting.vim $HOME/.vim/coc-setting.vim

ln -s $dotfile_dir/zsh/.zshenv $HOME/.zshenv
ln -s $dotfile_dir/.byobu $HOME/.byobu
ln -s $HOME/.byobu/.tmux.conf $HOME/.tmux.conf

git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

cd $dotfile_dir

# Rust
# curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# rustup default nightly
# rustup component add rustfmt rust-src rust-analysis rls clippy
# cargo install rusty-tags
# cargo install cargo-asm evcxr_repl

# C++
# install llvm ctags 

# neovim
# install neovim pynvim python3

# zsh
# install zsh-syntax-highlighting



