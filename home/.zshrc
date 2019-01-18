#PS4=$'%D{%M%S%.} %N:%i> '
#exec 3>&2 2>>$HOME/tmp/startlog.$$
#setopt xtrace prompt_subst

source "$HOME/.zplugin/bin/zplugin.zsh"
export ZSH_CACHE_DIR="$HOME/.zplugin/cache"

#autoload -Uz compinit
#compinit

zplugin snippet OMZ::lib/theme-and-appearance.zsh
zplugin snippet OMZ::lib/key-bindings.zsh
zplugin snippet OMZ::lib/directories.zsh
zplugin snippet https://github.com/tonyseek/oh-my-zsh-seeker-theme/blob/master/seeker.zsh-theme
zplugin light zdharma/fast-syntax-highlighting
zplugin ice silent wait'0'
zplugin snippet OMZ::lib/git.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/git/git.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/archlinux/archlinux.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/autojump/autojump.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/bundler/bundler.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/celery/_celery
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/cp/cp.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/django/django.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/docker/_docker
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/docker-compose/docker-compose.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/gem/gem.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/git-flow/git-flow.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/heroku/heroku.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/kubectl/kubectl.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/helm/helm.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/node/node.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/npm/npm.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/pip/pip.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/python/python.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/ruby/ruby.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/sudo/sudo.plugin.zsh
zplugin ice silent wait'0'
zplugin snippet OMZ::plugins/systemd/systemd.plugin.zsh

zplugin ice silent wait'!0' atload'zpcompinit; zplugin cdreplay -q'
zplugin snippet OMZ::plugins/tmux/tmux.plugin.zsh
zplugin ice as"program"
zplugin snippet OMZ::plugins/tmux/tmux.extra.conf

#zplugin snippet OMZ::themes/dstufft.zsh-theme
setopt promptsubst

source "$HOME/.homesick/repos/homeshick/homeshick.sh"
fpath=($HOME/.homesick/repos/homeshick/completions $fpath)

# PageUp and PageDown search
bindkey "\e[5~" history-beginning-search-backward
bindkey "\e[6~" history-beginning-search-forward

# Home and End keys
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line


# aliases
alias ms="tmuxinator start"

# force EDITOR
export EDITOR="/usr/bin/nvim"
alias vim="nvim"

# History setup

HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=1000000
setopt incappendhistory 
setopt extendedglob
setopt histignorespace
setopt histignoredups
setopt histignorealldups
setopt histfindnodups
setopt histsavenodups
setopt sharehistory
setopt histexpiredupsfirst

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

#zstyle :compinstall filename '/home/david/.zshrc'



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
    eval "$(rbenv init - --no-rehash)"
}
add_to_path rbenv "$HOME/.rbenv/bin" rbenv_init

#heroku
add_to_path heroku /usr/local/heroku/bin

#virtualenvwrapper
#source_path venvwrapper /usr/bin/virtualenvwrapper.sh

# fzf
function fzf_init {
    source /usr/share/fzf/completion.zsh

    export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
}

source_path fzf /usr/share/fzf/key-bindings.zsh fzf_init


# gcloud completion
source_path gcloud "/opt/google-cloud-sdk/completion.zsh.inc"

# Custom variables
source_path custom_config "$HOME/.custom_config"

# pyenv
function pyenv_init {
    export PYENV_ROOT="$HOME/.pyenv"
    eval "$(pyenv init - --no-rehash)"
    eval "$(pyenv virtualenv-init -)"
    export PYENV_VIRTUALENV_DISABLE_PROMPT=1

    # Youcompleteme requires shared libs
    export PYTHON_CONFIGURE_OPTS="--enable-shared"
    alias cdsitepackages="cd `pyenv prefix`/lib/python*/site-packages"

    function pyenv_prompt_info() { echo "$(pyenv version-name)" }
}
add_to_path pyenv "$HOME/.pyenv/bin" pyenv_init


# npm
function npm_init {
    export NPM_PACKAGES="$HOME/.npm-packages"
    export MANPATH="$NPM_PACKAGES/share/man:$MANPATH"
    export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
}
add_to_path npm "$HOME/.npm-packages/bin" npm_init

# local binaries
function add_local_lib {
    export LD_LIBRARY_PATH="$HOME/local/lib:$LD_LIBRARY_PATH"
}
add_to_path localbin "$HOME/local/bin"

# go
function golang_init {
    export GOPATH="$HOME/.golang"
}
add_to_path golang "$HOME/.golang/bin" golang_init

# direnv
command -v direnv 1>/dev/null && eval "$(direnv hook zsh)"

#unsetopt xtrace
#exec 2>&3 3>&-

# Java rocks!
export _JAVA_AWT_WM_NONREPARENTING=1 

