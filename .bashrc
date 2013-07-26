EDITOR=emacsclient
SVN_LOG_EDITOR=emacsclient
VISUAL=emacsclient
TERM=xterm-256color
export CHROME_DEVEL_SANDBOX=/usr/local/sbin/chrome-devel-sandbox

if [[ $(uname) = 'Linux' ]]; then
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
  alias lock='gnome-screensaver-command --lock'
  alias logout='gnome-session-quit --logout'
fi
. ~/webkit-tools/completions
