source "$HOME/.homesick/repos/homeshick/homeshick.sh"
fpath=($HOME/.homesick/repos/homeshick/completions $fpath)

source "$HOME/.zgen/zgen.zsh"

# Disable oh-my-zsh auto update
export DISABLE_AUTO_UPDATE=true

if ! zgen saved; then
    zgen oh-my-zsh

    zgen oh-my-zsh plugins/archlinux
    zgen oh-my-zsh plugins/celery
    zgen oh-my-zsh plugins/cp
    zgen oh-my-zsh plugins/django
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/docker-compose
    zgen oh-my-zsh plugins/gem
    zgen oh-my-zsh plugins/bundler
    zgen oh-my-zsh plugins/ruby
    zgen oh-my-zsh plugins/python
    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/git-flow
    zgen oh-my-zsh plugins/heroku
    zgen oh-my-zsh plugins/kubectl
    zgen oh-my-zsh plugins/node
    zgen oh-my-zsh plugins/npm
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/rbenv
    zgen oh-my-zsh plugins/sudo
    zgen oh-my-zsh plugins/systemd
    zgen oh-my-zsh plugins/tmux
    zgen oh-my-zsh plugins/virtualenv
    zgen oh-my-zsh plugins/virtualenvwrapper

    zgen load tonyseek/oh-my-zsh-virtualenv-prompt
    zgen load tonyseek/oh-my-zsh-seeker-theme seeker
    zgen load unixorn/autoupdate-zgen

    zgen save
fi

# PageUp and PageDown search
bindkey "\e[5~" history-beginning-search-backward
bindkey "\e[6~" history-beginning-search-forward

# Home and End keys
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line


# aliases
alias ms="mux start"

# force EDITOR
export EDITOR="/usr/bin/nvim"
alias vim="nvim"

# History setup

HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=1000000
setopt appendhistory extendedglob

# Vi mode
#bindkey -v
export KEYTIMEOUT=1

## Use vim cli mode
#bindkey '^P' up-history
#bindkey '^N' down-history

## backspace and ^h working even after
## returning from command mode
#bindkey '^?' backward-delete-char
#bindkey '^h' backward-delete-char

## ctrl-w removed word backwards
#bindkey '^w' backward-kill-word

## ctrl-r starts searching history backward
#bindkey '^r' history-incremental-search-backward

zstyle :compinstall filename '/home/david/.zshrc'

autoload -Uz compinit
compinit

function add_to_path {
    if [[ $# -lt 2 ]]; then
        echo "add_to_path: missing arguments"
        return
    fi
    if [[ ! -e $HOME/.ignore_$1 ]]; then
        if [[ -e "$2" ]]; then
            export PATH="$2:$PATH"
        else
            echo "please install $1 or add a .ignore_$1 in $HOME"
            return
        fi
    else
        return
    fi

    if [[ ! -z $3 ]]; then
        eval "$3"
    fi
}

function source_path {
    if [[ $# -lt 2 ]]; then
        echo "source_path: missing arguments"
        return
    fi
    if [[ ! -e $HOME/.ignore_$1 ]]; then
        if [[ -e "$2" ]]; then
            source $2
        else
            echo "please install $1 or add a .ignore_$1 in $HOME"
            return
        fi
    else
        return
    fi

    if [[ ! -z $3 ]]; then
        eval "$3"
    fi
}

# rbenv
function rbenv_init {
    eval "$(rbenv init -)"
}
add_to_path rbenv "$HOME/.rbenv/bin" rbenv_init

#heroku
add_to_path heroku /usr/local/heroku/bin

#virtualenvwrapper
source_path venvwrapper /usr/bin/virtualenvwrapper.sh

# npm
function npm_init {
    export NPM_PACKAGES="$HOME/.npm-packages"
    export MANPATH="$NPM_PACKAGES/share/man:$MANPATH"
    export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
}
add_to_path npm "$HOME/.npm-packages/bin"

# local binaries
function add_local_lib {
    export LD_LIBRARY_PATH="$HOME/local/lib:$LD_LIBRARY_PATH"
}
add_to_path localbin "$HOME/local/bin"
