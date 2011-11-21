#!/bin/bash

pushd $(dirname $0) > /dev/null
  REPO_DIR=$(pwd)
popd > /dev/null

if [ $(uname) = 'Darwin' ]; then
  if [ -z "$(which port)" ]; then
      echo Install MacPorts first
      exit 1
  fi

  sudo port selfupdate
  sudo port upgrade outdated

  for port in git-core screen emacs sbcl slime irssi
  do
    if [ -z "$(port list installed and $port)" ]; then
      sudo port install $port
    fi
  done

  sudo port install git-core +svn

  for editor_spec in EDITOR GIT_EDITOR SVN_LOG_EDITOR VISUAL
  do
    if [ -z "$(grep $editor_spec= ~/.bashrc)" ]; then
      echo $editor_spec=emacsclient >> ~/.profile
    fi
  done
elif [ $(uname) = 'Linux' ]; then
  for package in screen emacs emacs-goodies-el sbcl slime git-core xsel curl irssi
  do
    if [ -z "$(dpkg -s $package | grep 'Status: install ok installed')" ]; then
      sudo apt-get install $package
    fi
  done

  for editor_spec in EDITOR GIT_EDITOR SVN_LOG_EDITOR VISUAL
  do
    if [ -z "$(grep $editor_spec= ~/.bashrc)" ]; then
      echo $editor_spec=emacsclient >> ~/.bashrc
    fi
  done

  if [ -z "$(grep TERM=xterm-256color ~/.bashrc)" ]; then
    echo TERM=xterm-256color >> ~/.bashrc
  fi

  # Set up aliases to act like pbcopy and pbpaste on Mac OS X
  if [ -z "$(grep pbcopy ~/.bashrc)" ]; then
    echo alias pbcopy=\'xsel --clipboard --input\' >> ~/.bashrc
    echo alias pbpaste=\'xsel --clipboard --output\' >> ~/.bashrc
  fi
fi

if [ -z "$(which git)" ]; then
  echo Install git
  exit 1
fi

# Push to the branch being tracked by default, instead of the branch
# with the same name
git config --global push.default tracking

if [ ! -d ~/webkit-tools ]; then
  # Fetch webkit-tools
  pushd ~ > /dev/null
  git clone git@github.com:coonsta/webkit-tools.git
  popd > /dev/null
else
  # Update webkit-tools
  pushd ~/webkit-tools > /dev/null
  git checkout master || exit 1
  git pull --ff-only origin master || exit 1
  popd > /dev/null
fi

mkdir -p ~/site-lisp
pushd ~/site-lisp > /dev/null
  if [ ! -f color-moccur.el ]; then
    curl -O http://www.bookshelf.jp/elc/color-moccur.el || exit 1
    curl -O http://www.emacswiki.org/emacs/download/moccur-edit.el || exit 1
    emacs -Q --batch --eval '(byte-compile-file "color-moccur.el")
                             (byte-compile-file "moccur-edit.el")' --kill
  fi

  if [ ! -d js2-mode-read-only ]; then
    svn checkout http://js2-mode.googlecode.com/svn/trunk/ js2-mode-read-only
  fi

  pushd js2-mode-read-only > /dev/null
    svn update
    emacs -batch -q -l js2-build.el -f js2-build-js2-mode
  popd > /dev/null

  if [ ! -d zenburn-emacs ]; then
    git clone git://github.com/bbatsov/zenburn-emacs.git
  else
    pushd zenburn-emacs > /dev/null
      git pull
    popd
  fi
popd > /dev/null

for config_file in .emacs .gdbinit .screenrc .xmodmaprc
do
  ln -s $REPO_DIR/$config_file ~/$config_file
done
