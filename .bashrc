EDITOR=emacsclient
SVN_LOG_EDITOR=emacsclient
VISUAL=emacsclient
TERM=xterm-256color

if [[ $(uname) = 'Linux' ]]; then
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
  alias lock='gnome-screensaver-command --lock'
  alias logout='gnome-session-quit --logout'
fi
