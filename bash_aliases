alias realias='vim ~/.bash_aliases; source ~/.bash_aliases'

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grepp='grep -P --color=auto'

alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -lF'
alias lla='ls -AlF'
alias l1='ls -1'

alias damnit='nvim $(git grep -l "<<<< HEAD")'

alias flac2alac='for i in *.flac; do echo $i; ffmpeg -i "$i" -y -v 0 -vcodec copy -acodec alac "${i%.flac}".m4a; done'

alias ducks='du -cksh'

alias ce="python -m venv .venv"
alias ae='deactivate &> /dev/null; source ./.venv/bin/activate'
alias de='deactivate'
