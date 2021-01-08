#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
#export TERM=xterm-256color
export EDITOR=vim

export GOPATH=~/go
export PATH=$GOPATH/bin:"${PATH}"

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
#bind -x '"\C-p": vim $(fzf -m);'
#export FZF_ALT_C_COMMAND="cd ~/; bfs -type d -nohidden | sed s/^\./~/"
export FZF_ALT_C_COMMAND="rg --hidden --files --sort-files --null | xargs -0 dirname | sort -u"
source ~/fzf-git/functions.sh
source ~/fzf-git/key-binding.zsh
source ~/fzf-git/forgit.plugin.zsh

alias cp="cp -iv"
alias mv="mv -iv"
alias mkdir="mkdir -v"
alias vi="vim"
alias pp='pushbullet push "Pixel" link "${1}" "${1}"'

alias f='fasd -f'
alias v='f -e vim'

# some extra git aliases, based on the omz git plugin
alias glgg='git log --graph'
alias glgga='git log --graph --decorate --all'
alias glgm='git log --graph --max-count=10'
alias gl1='git log --oneline --decorate'
alias glol="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"
alias glols="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --stat"
alias glod="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'"
alias glods="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset' --date=short"
alias glola="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --all"
alias glog='git log --oneline --decorate --graph'
alias gloga='git log --oneline --decorate --graph --all'

#autoload -Uz compinit
compinit

# non-prezto theme
#promptinit
#prompt imp
#prompt gnzh
#source liquidprompt/liquidprompt

#prompt djm
#prompt sorin
export PATH=$HOME/.cargo/bin:$PATH
eval "$(starship init zsh)"

function google() {
    w3m 'https://www.google.co.uk/search?q='"${@}"
}

function crep() {
    pattern=$1
    shift
    gawk '/'$1'/{c++} ENDFILE{if (c) print FILENAME":"c; c=0}' "${@}"
}

eval "$(direnv hook zsh)"
export SDKMAN_DIR="/home/djm/.sdkman"
[[ -s "/home/djm/.sdkman/bin/sdkman-init.sh" ]] && source "/home/djm/.sdkman/bin/sdkman-init.sh"
#fpath=(~/.zsh.d/ $fpath)
(( $+commands[doctl] )) && source <(doctl completion zsh)
