if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

function git_prompt {
    local GITOUTPUT; GITOUTPUT=$(git rev-parse --show-toplevel --symbolic-full-name --abbrev-ref HEAD 2> /dev/null)
    if [ $? -eq 0 ] # i.e. we are in a git repo
    then
        IFS=$'\n'; DATA=($GITOUTPUT); unset IFS;
        local GITPATH="${DATA[0]}"
        local GITNAME="${GITPATH##*/}"
        local GITBRANCH="${DATA[1]}"
        if [ "$GITBRANCH" == HEAD ]
        then
            local NM=$(git name-rev --name-only HEAD 2> /dev/null)
            if [ "$NM" != undefined ]
            then echo -n "($GITNAME/@$NM):"
            else echo -n "($GITNAME/$(git rev-parse --short HEAD 2> /dev/null)):"
            fi
        else
            echo -n "($GITNAME/$GITBRANCH):"
        fi
    fi
}

# export for subshells
export -f git_prompt
export PS1='\[\033]0;\u@\H: \w\007\]\[\033[01;36m\]\H\[\033[00m\]:\[\033[01;34m\]$(git_prompt)\[\033[01;31m\]\W\[\033[00m\]\$ '
export EDITOR=emacsclient -t
export VISUAL=emacsclient -c
export ALTERNATE_EDITOR=emacs
export HISTCONTROL=ignoredups
export HISTSIZE=1000
export HISTFILESIZE=2000
export PATH=$PATH:$HOME/bin:$HOME/.cargo/bin:$HOME/.local/bin
export GRADLE_OPTS="-Dorg.gradle.daemon=true -Dorg.gradle.parallel=true"
export JAVA_OPTS="-Xms128m -Xmx16384m"
export _JAVA_AWT_WM_NONREPARENTING=1
export BOTO_CONFIG="~/.aws/config"
export VDPAU_DRIVER=radeonsi

# address problems with Gnome apps
# http://debbugs.gnu.org/cgi/bugreport.cgi?bug=15154#11
export NO_AT_BRIDGE=1

shopt -s checkwinsize
shopt -s histappend

if [ -x /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi
