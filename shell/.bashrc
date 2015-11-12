function we_are_in_git_work_tree {
    git rev-parse --is-inside-work-tree &> /dev/null
}

function optional_git_separator {
    if we_are_in_git_work_tree
    then echo -n ":"
    fi
}

function parse_git_branch {
    if we_are_in_git_work_tree
    then
        local BR=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD 2> /dev/null)
        if [ "$BR" == HEAD ]
        then
            local NM=$(git name-rev --name-only HEAD 2> /dev/null)
            if [ "$NM" != undefined ]
            then echo -n "@$NM)"
            else echo -n "$(git rev-parse --short HEAD 2> /dev/null))"
            fi
        else
            echo -n "$BR)"
        fi
    fi
}

function parse_git_status {
    if we_are_in_git_work_tree
    then
        local ST=$(git status --short 2> /dev/null)
        if [ -n "$ST" ]
        then echo -n "(+"
        else echo -n "(-"
        fi
    fi
}

function parse_git_directory {
    if we_are_in_git_work_tree
    then
        local GD=$(git rev-parse --show-toplevel 2> /dev/null)
        local CURRENT=$(echo "$GD" | sed -e "s|.*/\(.*\)/\(.*\)|\2|")
        echo "$CURRENT/"
    fi
}

# export all these for subshells
export -f parse_git_branch parse_git_status parse_git_directory we_are_in_git_work_tree

export TERM="xterm-256color"
export PS1='\[\033]0;\u@\h: \w\007\]\[\033[01;36m\]\h\[\033[00m\]:\[\033[01;34m\]$(parse_git_status)$(parse_git_directory)$(parse_git_branch)\[\033[00m\]$(optional_git_separator)\[\033[01;31m\]\W\[\033[00m\]\$ '
export EDITOR=emacsclient VISUAL=emacsclient ALTERNATE_EDITOR=emacs
export HISTCONTROL=ignoredups
export HISTSIZE=1000
export HISTFILESIZE=2000
export PATH=$PATH:$HOME/bin
export GRADLE_OPTS="-Dorg.gradle.daemon=true -Dorg.gradle.parallel=true"
export JAVA_OPTS="-Xms128m -Xmx16384m"
export BOTO_CONFIG="~/.aws/config"

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