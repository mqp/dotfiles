shopt -s checkwinsize
shopt -s histappend
HISTCONTROL=ignoredups
HISTSIZE=1000
HISTFILESIZE=2000
PATH=$PATH:$HOME/bin
alias ls="ls -h --color=auto"
eval `dircolors ~/.dircolors`

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
export TERM="xterm-color"
export PS1='\[\033]0;\u@\h: \w\007\]\[\033[01;36m\]\h\[\033[00m\]:\[\033[01;34m\]$(parse_git_status)$(parse_git_directory)$(parse_git_branch)\[\033[00m\]$(optional_git_separator)\[\033[01;31m\]\W\[\033[00m\]\$ '
