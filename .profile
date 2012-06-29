if [[ $(uname) = 'Darwin' ]]; then
  export PATH=/opt/local/bin:/opt/local/sbin:$PATH
  export JAVA_HOME=$(/usr/libexec/java_home)
  export PATH=~/android-sdk-macosx/tools:~/android-sdk-macosx/platform-tools:$PATH
  alias emacs=emacs-snapshot
fi

export EDITOR=emacsclient
export GIT_EDITOR=emacsclient
export SVN_LOG_EDITOR=emacsclient
export VISUAL=emacsclient

if [ -n "$BASH_VERSION" ]; then
  if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
  fi
fi
