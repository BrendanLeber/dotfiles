alias realias='$EDITOR ~/.bash_aliases; source ~/.bash_aliases'

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grepp='grep -P --color=auto'

alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -AlF'
alias l1='ls -1'

alias svndiff='svn diff --diff-cmd diff -x "-Nu -U10 --show-c-func"'
alias svndiffbw='svn diff --diff-cmd diff -x "-Nu -Bw --show-c-func"'

alias pscondemux='ps aux | grep condemux | grep -v grep'

alias t='python ~/bin/t.py --task-dir ~/tasks --list tasks'
alias ducks='du -cksh'

alias svs='svn status'

alias gvim='UBUNTU_MENUPROXY=0 gvim'
