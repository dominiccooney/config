#!/bin/bash

set -e

EMACS=emacs

pushd $(dirname $0) > /dev/null
  REPO_DIR=$(pwd)
popd > /dev/null

if [[ $(uname) = 'Darwin' ]]; then
  EMACS=emacs-snapshot

  if [[ -z $(which port) ]]; then
      echo Install MacPorts first
      exit 1
  fi

  sudo port selfupdate
  sudo port upgrade outdated

  for port in git-core screen emacs-snapshot irssi R
  do
    if [[ -z $(port list installed and $port) ]]; then
      sudo port install $port
    fi
  done

  sudo port install git-core +svn
elif [[ $(uname) = 'Linux' ]]; then
  for package in subversion screen emacs emacs-goodies-el git-core xsel curl xmonad
  do
    if [[ -z $(dpkg -s $package | grep 'Status: install ok installed') ]]; then
      sudo apt-get install $package
    fi
  done

  for editor_spec in EDITOR GIT_EDITOR SVN_LOG_EDITOR VISUAL
  do
    if [[ -z $(grep $editor_spec= ~/.bashrc) ]]; then
      echo $editor_spec=emacsclient >> ~/.bashrc
    fi
  done

  if [[ -z $(grep TERM=xterm-256color ~/.bashrc) ]]; then
    echo TERM=xterm-256color >> ~/.bashrc
  fi

  # Set up aliases to act like pbcopy and pbpaste on Mac OS X
  if [[ -z $(grep pbcopy ~/.bashrc) ]]; then
    echo alias pbcopy=\'xsel --clipboard --input\' >> ~/.bashrc
    echo alias pbpaste=\'xsel --clipboard --output\' >> ~/.bashrc
  fi
fi

if [[ -z $(which git) ]]; then
  echo Install git
  exit 1
fi

# Push to the branch being tracked by default, instead of the branch
# with the same name
git config --global push.default tracking

if [[ ! -d ~/webkit-tools/.git ]]; then
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
  if [[ ! -f color-moccur.el ]]; then
    curl -O http://www.bookshelf.jp/elc/color-moccur.el || exit 1
    curl -O http://www.emacswiki.org/emacs/download/moccur-edit.el || exit 1
    $EMACS -Q --batch --eval '(byte-compile-file "color-moccur.el")
                              (byte-compile-file "moccur-edit.el")' --kill
  fi

  # Clean up old js2-mode
  if [[ -d js2-mode-read-only ]]; then
    rm -rf js2-mode-read-only
  fi

  if [[ ! -d js2-mode ]]; then
    git clone https://github.com/dgutov/js2-mode.git
    pushd js2-mode > /dev/null
      git checkout -b emacs24 origin/emacs24
    popd > /dev/null
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
popd > /dev/null


for config_file in .emacs .gdbinit .screenrc .profile
do
  if [[ -h ~/$config_file ]]; then
    rm ~/$config_file
  else
    rm -if $config_file
  fi
  ln -s $REPO_DIR/$config_file ~/$config_file
done

if [[ $(uname) = 'Linux' ]]; then
  for config_file in .Xmodmap .xmonad
  do
    if [[ -h ~/$config_file ]]; then
      rm ~/$config_file
    else
      rm -if $config_file
    fi
    ln -s $REPO_DIR/$config_file ~/
  done

  xmonad --recompile

  sudo cp $REPO_DIR/xmonad.session /usr/share/gnome-session/sessions
  sudo cp $REPO_DIR/xmonad-unity-session.desktop /usr/share/xsessions
fi
