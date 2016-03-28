#!/bin/bash

set -e
set -u

EMACS=emacs

pushd $(dirname $0) > /dev/null
  REPO_DIR=$(pwd)
popd > /dev/null

if [[ $(uname) = 'Linux' ]]; then
  if [[ $(lsb_release -i) =~ 'Debian' ]]; then
    EMACS_PACKAGE=emacs23-nox
  else
    EMACS_PACKAGE=emacs24-nox
  fi

  for package in subversion screen $EMACS_PACKAGE emacs-goodies-el git-core xsel curl
  do
    if [[ -z $(dpkg -s $package | grep 'Status: install ok installed') ]]; then
      sudo apt-get install $package
    fi
  done
fi

if [[ -z $(which git) ]]; then
  echo Install git
  exit 1
fi

# Push to the branch being tracked by default, instead of the branch
# with the same name
git config --global push.default tracking

git config --global color.ui true
git config --global user.name 'Dominic Cooney'

if [[ ! -d ~/blink-tools/.git ]]; then
  # Fetch blink-tools
  pushd ~ > /dev/null
  git clone git@github.com:coonsta/blink-tools.git
  popd > /dev/null
else
  # Update blink-tools
  pushd ~/blink-tools > /dev/null
  git checkout master
  git pull --ff-only origin master
  popd > /dev/null
fi

if [[ ! -d ~/depot_tools/.git ]]; then
  # Fetch Chromium depot_tools
  pushd ~ > /dev/null
  git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git
  popd > /dev/null
fi

mkdir -p ~/site-lisp
pushd ~/site-lisp > /dev/null
  if [[ ! -f color-moccur.el ]]; then
    curl -O http://www.bookshelf.jp/elc/color-moccur.el
    curl -O http://www.emacswiki.org/emacs/download/moccur-edit.el
    $EMACS -Q --batch --eval '(byte-compile-file "color-moccur.el")
			      (byte-compile-file "moccur-edit.el")' --kill
  fi

  # Clean up old js2-mode
  if [[ -d js2-mode-read-only ]]; then
    rm -rf js2-mode-read-only
  fi

  if [[ ! -d js2-mode ]]; then
    git clone git://github.com/mooz/js2-mode.git
  fi

  pushd js2-mode > /dev/null
    git pull
    $EMACS --batch -f batch-byte-compile js2-mode.el
  popd > /dev/null

  if [[ ! -d zenburn-emacs ]]; then
    git clone git://github.com/bbatsov/zenburn-emacs.git
  else
    pushd zenburn-emacs > /dev/null
      git pull
    popd > /dev/null
  fi

  if [[ ! -d Emacs-D-Mode ]]; then
    git clone https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode.git
  else
    pushd Emacs-D-Mode > /dev/null
      git pull
    popd > /dev/null
  fi
popd > /dev/null


for config_file in .emacs .gdbinit .screenrc .profile
do
  if [[ ! -h ~/$config_file ]]; then
    rm -f ~/$config_file
  fi
  ln -sf $REPO_DIR/$config_file ~/
done

if [[ $(uname) = 'Linux' ]]; then
  if [[ -f ~/.bashrc ]]; then
    rm -f ~/.bash_aliases
    ln -s $REPO_DIR/.bashrc ~/.bash_aliases
  else
    ln -sf $REPO_DIR/.bashrc ~/
  fi
fi

echo '*** done ***'
