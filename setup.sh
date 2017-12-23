#!/bin/bash

set -e
set -u

EMACS=emacs

pushd $(dirname $0) > /dev/null
  REPO_DIR=$(pwd)
popd > /dev/null

if [[ $(uname) = 'Linux' ]]; then
  # TODO(dpc): Switch to emacs25
  EMACS_PACKAGE=emacs24-nox

  for package in subversion screen $EMACS_PACKAGE emacs-goodies-el git-core xsel curl python3-pip python3-dev python-virtualenv
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
      (\"melpa\" . \"https://melpa.org/packages/\")
      (\"org\" . \"http://orgmode.org/elpa/\")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package '(material-theme js2-mode org-plus-contrib))
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

# Install Cuda and TensorFlow
#sudo apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub
#curl -O http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/cuda-repo-ubuntu1604_9.1.85-1_amd64.deb
#sudo dbkg -i cuda-repo-ubuntu1604_9.1.85-1_amd64.deb
#sudo apt-get update
#sudo apt-get install cuda-8.0
# https://developer.nvidia.com/rdp/cudnn-download 6.0 for CUDA 8.0 runtime library, then developer library, for Ubuntu 16.04 deb
# sudo apt-get install libcupti-dev
# ... see https://www.tensorflow.org/install/install_linux
echo '*** done ***'
