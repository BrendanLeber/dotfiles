# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# I want UTF-8 dammit!
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

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

# where the hell am I? (ISO 6709 format)
export LOCATION=+47.305464-122.215806/
export LOCATION_NAME="Auburn, WA"
export LOCATION_GRID="CN87vh"

function flac-grep () {
    RX="$1";
    shift;
    for i in "$@"; do
        metaflac --export-tags-to=- "$i" | grep -i "$RX";
    done;
}

function flac-find () {
    RX="$1";
    shift;
    for i in "$@"; do
        metaflac --export-tags-to=- "$i" | grep -i "$RX" >/dev/null && echo "$i";
    done;
}

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# enable perlbrew if it has been installed on this system
if [ -f ~/perl5/perlbrew/etc/bashrc ]; then
    source ~/perl5/perlbrew/etc/bashrc
fi

# enable rakudobrew if it has been installed on this system
if [ -f ~/.rakudobrew/bin/rakudobrew ]; then
    export PATH=~/.rakudobrew/bin:$PATH
fi

# add GnuPG Agent environtment variables, if they exist
#if [ -f ~/.gnupg/gpg-agent-info-$(hostname) ]; then
#    source ~/.gnupg/gpg-agent-info-$(hostname)
#elif [ -f ~/.gnupg/gpg-agent-info ]; then
#    source ~/.gnupg/gpg-agent-info
#fi


# add git bash prompt, if it exists
if [ -d ~/.bash-git-prompt ]; then
    export GIT_PROMPT_ONLY_IN_REPO=1
    export GIT_PROMPT_END_USER=" \n$WHITE\u@\h$ResetColor \$ "
    export GIT_PROMPT_END_ROOT=" \n$WHITE\u@\h$ResetColor # "
    source ~/.bash-git-prompt/gitprompt.sh
fi


# enable hub if it is installed on the system
if [ -f /usr/local/bin/hub ]; then
    eval "$(hub alias -s)"
fi

# Add hub completion and GPG_TTY env var.
if [ -f ~/Source/hub/etc/hub.bash_completion.sh ]; then
   . ~/Source/hub/etc/hub.bash_completion.sh
fi


# let GPG Agent know which terminal it should be using
export GPG_TTY=$(tty)


# if we have a local bin directory add it to our path, leave at end
if [ -d ~/.local/bin ]; then
    export PATH="~/.local/bin:$PATH"
fi

mkcd() {
    mkdir -p "$1" && cd "$1"
}

op() {
    gnome-open "$@" &> /dev/null
}

export BML_BASHRC=1
