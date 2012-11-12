export EDITOR=emacsclient
export SVN_LOG_EDITOR=emacsclient
export VISUAL=emacsclient

if [[ $(uname) = 'Darwin' ]]; then
  # Macports
  export PATH=/opt/local/bin:/opt/local/sbin:"$PATH"

  # Android
  export JAVA_HOME=$(/usr/libexec/java_home)
  export PATH=~/android-sdk-macosx/tools:~/android-sdk-macosx/platform-tools:"$PATH"

  # Go
  export GOROOT=~/go
else
  export GOPATH=~/go
fi

export PATH=~/go/bin:"$PATH"

if [ -n "$BASH_VERSION" ]; then
  if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
  fi
fi
