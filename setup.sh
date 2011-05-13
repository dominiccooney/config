#!/bin/bash

pushd $(dirname $0)
  REPO_DIR=$(pwd)
popd

if [ $(uname) = 'Darwin' ]; then
  for port in screen emacs color-theme-mode.el sbcl slime
  do
    if [ -z "$(port list installed and $port)" ]; then
      sudo port install $port
    fi
  done

  if [ -z "$(grep EDITOR= ~/.profile)" ]; then
    echo EDITOR=emacsclient >> ~/.profile
  fi
elif [ $(uname) = 'Linux' ]; then
  for package in screen emacs emacs-goodies-el sbcl slime git-core xsel curl
  do
    if [ -z "$(dpkg -s $package | grep 'Status: install ok installed')" ]; then
      sudo apt-get install $package
    fi
  done

  if [ -z "$(grep EDITOR= ~/.bashrc)" ]; then
    echo EDITOR=emacsclient >> ~/.bashrc
  fi

  if [ -z "$(grep TERM=xterm-256color ~/.bashrc)" ]; then
    echo TERM=xterm-256color >> ~/.bashrc
  fi

  # Set up aliases to act like pbcopy and pbpaste on Mac OS X
  if [ -z "$(grep pbcopy ~/.bashrc)" ]; then
    echo alias pbcopy=\'xsel --clipboard --input\' >> ~/.bashrc
    echo alias pbpaste=\'xsel --clipboard --output\' >> ~/.bashrc
  fi
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

for config_file in .emacs .screenrc .xmodmaprc
do
  ln -s $REPO_DIR/$config_file ~/$config_file
done