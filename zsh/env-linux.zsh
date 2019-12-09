export EDITOR=nvim

source /etc/profile.d/openfoam-7.sh

#KVS PATH
#export KVS_DIR=~/local/kvs_osmesa_gentoo_debug
#export KVS_DIR=~/local/kvs_glut
export KVS_DIR=~/local/kvs_glut_bef
#export KVS_DIR=~/local/kvs_glut_opencv
export KVS_OSMESA_GALLIUM_DRIVER=llvmpipe
#export KVS_OSMESA_DIR=~/gentoo/usr/include
export path=($path $KVS_DIR/bin )
export path=($path ~/.cargo/bin ~/.local/bin ~/.yarn/bin)
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export LANG

# Vivado など Java系の表示を正しくする
export _JAVA_AWT_WM_NONREPARENTING=1
# Vivado のセットアップコマンド
# sudo sh -c "export _JAVA_AWT_WM_NONREPARENTING=1 && ./xsetup"
#
# snippet
#
# エラーを出力しない (find で Permission deniedが表示されない)
# 2>/dev/null
