# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# If not running interactively, don't do anything
[[ $- == *i* ]] || return

if [ -f ~/.dir_colors/dircolors ]
    then eval `dircolors ~/.dir_colors/dircolors`
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
alias rm='rm -I'

export PATH=$PATH:~/misc
export PATH=$PATH:~/git/perf-tools/bin
export PATH=$PATH:~/git/FlameGraph
export PATH=/usr/local/bin:$PATH

alias sep='fortune | cowsay -f dragon-and-cow | lolcat'

# Fun!
fortune | cowsay | lolcat
