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

alias svndiff='svn diff --diff-cmd diff -x "-Nu -U10 --show-c-func"'
alias svndiffbw='svn diff --diff-cmd diff -x "-Nu -Bw --show-c-func"'
alias svs='svn status'

alias ducks='du -cksh'

alias ce="python -m venv .venv"
alias ae='deactivate &> /dev/null; source ./.venv/bin/activate'
alias de='deactivate'
