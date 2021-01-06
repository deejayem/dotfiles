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
