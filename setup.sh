#!/bin/bash

set -e
set -u

EMACS=emacs

pushd $(dirname $0) > /dev/null
  REPO_DIR=$(pwd)
popd > /dev/null

if [[ $(uname) = 'Linux' ]]; then
  # TODO(dpc): Switch to emacs25
  EMACS_PACKAGE=emacs24

  for package in subversion screen $EMACS_PACKAGE emacs-goodies-el git-core xsel curl latex dvipng
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

rm -rf ~/site-lisp

$EMACS -Q --batch --eval "
(progn
  (setq package-archives
    '((\"gnu\" . \"https://elpa.gnu.org/packages/\")
      (\"melpa\" . \"https://melpa.org/packages/\")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package '(material-theme js2-mode))
    (unless (package-installed-p package)
      (package-install package))))"

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
