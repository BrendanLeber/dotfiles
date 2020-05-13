#!/bin/bash
# ~/.bashrc: executed by bash(1) for non-login shells.


# If not running interactively, don't do anything
[ -z "$PS1" ] && return


# I want UTF-8 dammit!
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8


# everybody doesn't need to read my diary
umask 027


# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
    xterm-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
    else
    color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# only set the term capabilities to support 256 color if we're not in tmux
[ -z $TMUX ] && export TERM=xterm-256color


if [[ -f "${HOME}/.dotfiles/tmux.bash" ]]; then
    source "${HOME}/.dotfiles/tmux.bash"
fi


# where the hell am I? (ISO 6709 format)
export LOCATION=+47.305464-122.215806/
export LOCATION_NAME="Auburn, WA"
export LOCATION_GRID="CN87vh"


# display the weather by default in my location
wttr()
{
    # change Paris to your default location
    local request="wttr.in/${1-47.305464,-122.215806}"
    [ "$COLUMNS" -lt 125 ] && request+='?n'
    curl -H "Accept-Language: ${LANG%_*}" --compressed "$request"
}


# display the current phase of the moon
moon()
{
    # change Paris to your default location
    local request="wttr.in/${1-Moon}"
    [ "$COLUMNS" -lt 125 ] && request+='?n'
    curl -H "Accept-Language: ${LANG%_*}" --compressed "$request"
}


# search through FLAC files
function flac-grep () {
    RX="$1";
    shift;
    for i in "$@"; do
        metaflac --export-tags-to=- "$i" | grep -i "$RX";
    done;
}


# find FLAC files matching a regular expression
function flac-find () {
    RX="$1";
    shift;
    for i in "$@"; do
        metaflac --export-tags-to=- "$i" | grep -i "$RX" >/dev/null && echo "$i";
    done;
}


# Alias definitions.
if [[ -f "${HOME}/.bash_aliases" ]]; then
    source "${HOME}/.bash_aliases"
fi


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [[ -f /etc/bash_completion ]] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


# setup some environment variables for (Neo)VIM
export VIMCONFIG="${HOME}/.config/nvim"
export VIMDATA="${HOME}/.local/share/nvim"

# if fzf has been installed as a NeoVIM plugin add it to our path
if [[ -d "${VIMCONFIG}/pack/github/start/fzf/bin" ]]; then
    export PATH="${VIMCONFIG}/pack/github/start/fzf/bin:${PATH}"
fi


# pyenv
if [[ -d "${HOME}/.pyenv" ]]; then
    export PYENV_ROOT="${HOME}/.pyenv"
    export PATH="${PYENV_ROOT}/bin:${PATH}"
    eval "$(pyenv init -)"

    export PYENV_VIRTUALENV_DISABLE_PROMPT=1
    eval "$(pyenv virtualenv-init -)"
fi


# add startup for interactive Python REPLs.
if [[ -f "${HOME}/.dotfiles/startup.py" ]]; then
    export PYTHONSTARTUP="${HOME}/.dotfiles/startup.py"
fi


# add GnuPG Agent environtment variables, if they exist
#if [ -f ~/.gnupg/gpg-agent-info-$(hostname) ]; then
#    source ~/.gnupg/gpg-agent-info-$(hostname)
#elif [ -f ~/.gnupg/gpg-agent-info ]; then
#    source ~/.gnupg/gpg-agent-info
#fi


export SSH_ENV="$HOME/.ssh/environment"

function start_ssh_agent {
    echo "Initializing new SSH agent..."
    /usr/bin/ssh-agent -s | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    source "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

if [[ -f "${SSH_ENV}" ]]; then
    source "${SSH_ENV}" > /dev/null
    ps -ef | grep ${SSH_AGENT_PID} | grep "ssh-agent -s$" >/dev/null || {
        start_ssh_agent;
    }
else
    start_ssh_agent;
fi


# add git bash prompt, if it exists
if [[ -d "${HOME}/.bash-git-prompt" ]]; then
    export GIT_PROMPT_ONLY_IN_REPO=1
    export GIT_PROMPT_END_USER=" \n$WHITE\u@\h$ResetColor \$ "
    export GIT_PROMPT_END_ROOT=" \n$WHITE\u@\h$ResetColor # "
    source "${HOME}/.bash-git-prompt/gitprompt.sh"
fi


# enable hub (and it's completions) if it is installed on the system
if [[ -f /usr/bin/hub ]]; then
    eval "$(hub alias -s)"
    source ~/.dotfiles/hub.bash_completion.sh
fi


# let GPG Agent know which terminal it should be using
export GPG_TTY=$(tty)


# make a directory and change to it
mkcd() {
    mkdir -p "$1" && cd "$1"
}


# open a file in gnome using the default application
op() {
    gnome-open "$@" &> /dev/null
}


PATH="/home/brendan/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/brendan/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/brendan/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/brendan/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/brendan/perl5"; export PERL_MM_OPT;


# if we have a local bin directory add it to our path, leave at end
if [[ -d ~/.local/bin ]]; then
    export PATH="~/.local/bin:$PATH"
fi


# read a .local file if it exists
if [[ -f ~/.bashrc.local ]]; then
    source ~/.bashrc.local
fi

export BML_BASHRC=1
