shopt -s checkwinsize
shopt -s histappend
HISTCONTROL=ignoredups
HISTSIZE=1000
HISTFILESIZE=2000
PATH=$PATH:$HOME/bin
alias ls="ls -h --color=auto"
eval `dircolors ~/.dircolors`
export PS1='\[\033]0;\u@\h: \w\007\]\[\033[01;36m\]\h\[\033[00m\]:\[\033[01;31m\]\W\[\033[00m\]\$ '
