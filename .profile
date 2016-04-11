export EDITOR=emacsclient
export SVN_LOG_EDITOR=emacsclient
export VISUAL=emacsclient

export PATH=~/depot_tools:"$PATH"

if [ -n "$BASH_VERSION" ]; then
  if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
  fi
fi
