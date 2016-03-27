#!/bin/bash

set -e
set -u

EMACS=emacs

pushd $(dirname $0) > /dev/null
  REPO_DIR=$(pwd)
popd > /dev/null

if [[ $(uname) = 'Darwin' ]]; then
  if [[ -z $(which port) ]]; then
      echo Install MacPorts first
      exit 1
  fi

  # Uninstall emacs-snapshot if it is lying around.
  for unused_port in emacs-snapshot irssi
  do
    if [[ ! -z $(port -q outdated $unused_port) ]]; then
      sudo port uninstall -u $unused_port
    fi
  done

  sudo port selfupdate
  if [[ ! -z $(port list outdated) ]]; then
    sudo port upgrade outdated
  fi

  for port in git-core screen emacs R mercurial
  do
    if [[ -z $(port list installed and $port) ]]; then
      sudo port install -u $port
    fi
  done

  sudo port install -u git-core +svn

  if [[ ! -d ~/go/bin ]]; then
    curl -o - 'http://go.googlecode.com/files/go1.0.3.darwin-amd64.tar.gz' | tar xz -C ~
  fi

elif [[ $(uname) = 'Linux' ]]; then
  for package in subversion screen emacs24-nox emacs-goodies-el git-core xsel curl gnome-tweak-tool
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

if [[ ! -d ~/webkit-tools/.git ]]; then
  # Fetch webkit-tools
  pushd ~ > /dev/null
  git clone git@github.com:coonsta/webkit-tools.git
  popd > /dev/null
else
  # Update webkit-tools
  pushd ~/webkit-tools > /dev/null
  git checkout master
  git pull --ff-only origin master
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
  for config_file in .Xmodmap
  do
    if [[ ! -h ~/$config_file ]]; then
      rm -f ~/$config_file
    fi
    ln -sf $REPO_DIR/$config_file ~/
  done

  if [[ -f ~/.bashrc ]]; then
    rm -f ~/.bash_aliases
    ln -s $REPO_DIR/.bashrc ~/.bash_aliases
  else
    ln -sf $REPO_DIR/.bashrc ~/
  fi

  $REPO_DIR/source_code_pro.py
fi

echo '*** done ***'
