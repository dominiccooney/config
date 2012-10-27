if [[ $(uname) = 'Darwin' ]]; then
  export PATH=/opt/local/bin:/opt/local/sbin:$PATH
  export JAVA_HOME=$(/usr/libexec/java_home)
  export PATH=~/android-sdk-macosx/tools:~/android-sdk-macosx/platform-tools:$PATH
  export EDITOR=emacsclient-snapshot
  export SVN_LOG_EDITOR=emacsclient-snapshot
  export VISUAL=emacsclient-snapshot
  alias emacs=emacs-snapshot
else
  export EDITOR=emacsclient
  export GIT_EDITOR=emacsclient
  export SVN_LOG_EDITOR=emacsclient
  export VISUAL=emacsclient
fi

if [ -n "$BASH_VERSION" ]; then
  if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
  fi
fi
