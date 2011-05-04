#!/bin/bash

pushd $(dirname $0)
  REPO_DIR=$(pwd)
popd

if [ $(uname) = 'Darwin' ]; then
  for port in emacs color-theme-mode.el
  do
    if [ -z "$(port list installed and $port)" ]; then
      sudo port install $port
    fi
  done
fi

mkdir -p ~/site-lisp
pushd ~/site-lisp
  if [ ! -f color-moccur.el ]; then
    curl -O http://www.bookshelf.jp/elc/color-moccur.el || exit 1
    curl -O http://www.emacswiki.org/emacs/download/moccur-edit.el || exit 1
    emacs -Q --batch --eval '(byte-compile-file "color-moccur.el")
                             (byte-compile-file "moccur-edit.el")' --kill
  fi
popd

for config_file in .emacs .screenrc
do
  ln -s $REPO_DIR/$config_file ~/$config_file
done