## This is a mess, and needs tidying up. Hopefully it works on linux and (free)bsd, and can be sourced repeatedly without making PATH grow each time.

export LANG=en_GB.UTF-8

if [[ $- != *i* ]]; then
    # Non-interactive shell
    return
fi

export HOSTNAME=${HOSTNAME:-$(hostname -s)}
export HOST=$HOSTNAME
export USER=${USER:-$(whoami)}

#if [[ x$SCRIPT == x ]]
#then
#    script -q typescripts/script-`date +%Y%m%d%H%M%S``tty|tr / -`-$$
#    exit
#fi

## use _ for "internal" functions, and - for those intended to be used
## interactively

do_interesting_prompt() {
    prompt_choice=$1
    # col for colour, not column
    reset_col='\[\e[0m\]'

    sif_col='\[\e[0;35m\]'
    tyr_col='\[\e[0;36m\]'
    basil_col='\[\e[0;33m\]'
    gendros_col='\[\e[1;37m\]'

    host_col=$reset_col
    # TODO make all of these unique again
    [[ "x$HOST" == "xsif" ]] && host_col=$sif_col
    [[ "x$HOST" == "xtyr" ]] && host_col=$tyr_col
    [[ "x$HOST" == "xsagacity" ]] && host_col=$tyr_col
    [[ "x$HOST" == "xwolfman" ]] && host_col=$tyr_col
    [[ "x$HOST" == "xbasil" ]] && host_col=$basil_col
    [[ "x$HOST" == "xgendros" ]] && host_col=$gendros_col
    [[ "x$HOST" == "xCHESTER" ]] && host_col=$gendros_col
    [[ "x$HOST" == "xtriton" ]] && host_col=$gendros_col

    djm_col='\[\e[0;32m\]'
    wiz_col='\[\e[1;32m\]'
    deejayem_col='\[\e[1;32m\]'

    user_col=$reset_col
    [[ "x$USER" == "xdjm" ]] && user_col=$djm_col
    [[ "x$USER" == "xwizard" ]] && user_col=$wiz_col
    [[ "x$USER" == "xdeejayem" ]] && user_col=$deejayem_col
    [[ "x$USER" == "xdavidmo" ]] && user_col=$deejayem_col

    at_col='\[\e[0;37m\]'
    [[ -n "$SSH_CLIENT" ]] && at_col='\[\e[1;33m\]'

    time_col='\[\e[0;36m\]'
    # don't clash with host_col
    [[ "x$HOST" == "xtyr" ]] && time_col='\[\e[1;33m\]'
    [[ "x$HOST" == "xsagacity" ]] && time_col='\[\e[1;33m\]'
    [[ "x$HOST" == "xwolfman" ]] && time_col='\[\e[1;33m\]'

    dir_col='\[\e[1;34m\]'
    sb_col=$reset_col # Square brackets
    rb_col='\[\e[0;32m\]' # Round brackets
    cb_col='\[\e[0;32m\]' # Curly brackets
    cn_col='\[\e[0;35m\]' # Command numbers

    # Should vaguely correspond to user_col, but emphasise whether or not
    # we're using ssh (shutting down the wrong machine is not cool)
    prompt_col='\[\e[0;32m\]'
    [[ -n "$SSH_CLIENT" ]] && prompt_col='\[\e[1;32m\]'

    # col for colour, not column (still)
    # \h on first line as well as the second, because ^R obscures the second
    if [[ $prompt_choice -eq 1 ]]; then
        export PS1="$time_col\d \t $cb_col{$host_col\h$cb_col}\n$sb_col[$user_col\u$at_col@$host_col\h $dir_col\w$sb_col]$rb_col($cn_col\#:\j:"'$?'"$rb_col)$prompt_col\$$reset_col "
    elif [[ $prompt_choice -eq 2 ]]; then
        export PS1="$sb_col[$user_col\u$at_col@$host_col\h $dir_col\w$sb_col] $time_col\d \t $rb_col($cn_col\#:\j:"'$?'"$rb_col)\n$prompt_col\$$reset_col "
    elif [[ $prompt_choice -eq 3 ]]; then
        export PS1="$sb_col[$user_col\u$at_col@$host_col\h $dir_col\w$sb_col]$rb_col($cn_col\#:\j:"'$?'"$rb_col)$prompt_col\$$reset_col "
    else
        do_interesting_prompt 2
    fi
}

do_boring_prompt() {
    export PS1='\n\d \t {\h}\n[\u@\h \w](\#)\$ '
}

if [ -x "$(command -v gdircolors)" ]; then
    dircolors=`type -p gdircolors`
else
    dircolors=`type -p dircolors`
fi

#if [ -n "${dircolors+x}" ]; then
if [ -n "${dircolors}" ]; then
    if [[ -f ~/.dir_colors ]]; then
        eval `$dircolors -b ~/.dir_colors`
    else
        eval `$dircolors -b /etc/DIR_COLORS`
    fi
fi
#alias ls="ls --color=no"


## PATH - TODO reduce duplication with a function
if [[ -n "${PATH/*\/usr\/local\/mpi\/openmpi\/bin:*/}" ]] ; then
    export PATH="/usr/local/mpi/openmpi/bin:$PATH"
fi

if [[ -n "${PATH/*\/usr\/local\/texlive\/2009\/bin\/i386-freebsd8:*/}" ]] ; then
    export PATH="/usr/local/texlive/2009/bin/i386-freebsd8:$PATH"
fi

if [[ -n "${PATH/*$HOME\/local\/bin:*/}" ]] ; then
    export PATH="$HOME/local/bin:$PATH"
fi

if [[ -n "${PATH/*$HOME\/.local\/bin:*/}" ]] ; then
    export PATH="$HOME/.local/bin:$PATH"
fi

if [[ -n "${PATH/*$HOME\/games:*/}" ]] ; then
    export PATH="$HOME/games:$PATH"
fi

if [[ -n "${PATH/*$HOME\/bin:*/}" ]] ; then
    export PATH="$HOME/bin:$PATH"
fi

## MANPATH
if [[ -n "${MANPATH/*\/usr\/local\/mpi\/openmpi\/man:*/}" ]] ; then
    export PATH="/usr/local/mpi/openmpi/man:$PATH"
fi

if [[ -n "${MANPATH/*$HOME\/local\/man:*/}" ]] ; then
    export PATH="$HOME/local/man:$PATH"
fi

if [[ -n "${MANPATH/*$HOME\/local\/share\/man:*/}" ]] ; then
    export PATH="$HOME/local/share/man:$PATH"
fi


# Should test to see if we're capable of being interesting or not (TODO)
do_interesting_prompt 2
#do_boring_prompt

# Change the window title of X terminals
case $TERM in
    xterm*|rxvt*|Eterm)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
        ;;
#    screen) # don't want this with tmux...
#        PROMPT_COMMAND='echo -ne "\033k${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
#        ;;
esac

[ -f /etc/bash_completion ] && source /etc/bash_completion
#[ -f /usr/local/etc/bash_completion ] && source /usr/local/etc/bash_completion
[ -f /usr/local/share/bash-completion/bash_completion.sh ] && source /usr/local/share/bash-completion/bash_completion.sh
set -C

#export PATH=${HOME}/bin:${HOME}/local/bin:${HOME}/.local/bin:$PATH
#export PATH=/usr/local/games/:$PATH
#export MPD_HOST=redacted@localhost
#export MPD_PORT=6600
#export NNTPSERVER=news.btopenworld.com
#export NNTPSERVER=news.individual.net
#export NNTPSERVER=public.teranews.com
#export NNTPSERVER=reader.news4all.se
export HOSTNAME=`hostname -s`
export HOST=$HOSTNAME
[ -f /usr/bin/vim ] && export EDITOR=/usr/bin/vim
[ -f /usr/local/bin/vim ] && export EDITOR=/usr/local/bin/vim
export PAGER=/usr/bin/less
#export PERL5LIB="${HOME}/perl/share/perl/:${HOME}/lib/perl/:${HOME}/perl5/"
export MOZ_NO_PANGO=1

alias y=yes
alias s="sudo"
alias svi="sudo vim"
alias gvi=gvim
alias vi=vim
alias viclean="rm *~"
alias vidotclean="rm .*~"
alias vibinclean="rm ~/bin/*~"
alias trunc="colrm 80"

alias pc="pocket-cli.py"
alias pp='pushbullet push "LGE Nexus 4" link "${1}" "${1}"'

[ ! -x "$(command -v fetch)" -a  -x "$(command -v wget)"  ] && alias fetch="wget"
[ -f /etc/debian_version ] && alias psearch="apt-cache search"

#alias esv='diatheke -b ESV -k'
alias esv="bible esv"
alias asv="diatheke -b ASV -k"
alias niv="diatheke -b NIV -k"
alias tniv="diatheke -b TNIV -k"
alias kjv="diatheke -b KJV -k"
alias nkjv="diatheke -b NKJV -k"
alias nasb="diatheke -b NASB -k"
alias amp="diatheke -b Amp -k"
alias ylt="diatheke -b YLT -k"
alias nlt="diatheke -b NLT -k"
alias Geneva="diatheke -b Geneva1599 -k"
alias hcsb="diatheke -b HCSB -k"
alias net="diatheke -b NETfree -k"

#alias host=hostx
#alias cc_="gcc -ansi -pedantic-errors -Wall -W -Werror -Wnested-externs -Wmissing-declarations -Wmissing-prototypes -Wstrict-prototypes -Wbad-function-cast -Wredundant-decls -Wcast-qual -Wundef -Winline -Wpointer-arith -Wcast-align -Wchar-subscripts -Wshadow -Wwrite-strings"
#alias noxbell=LD_PRELOAD=$HOME/noxbell/libnoxbell.so

alias m='mpc'
alias madd='mpc add'
alias mcl='mpc clear'
alias mp='mpc play'
alias mls='mpc playlist'
alias msearch='mpc search'
# individual aliases so they can be tab completed
alias msearchalbum='mpc search album'
alias msearchartist='mpc search artist'
alias msearchcomposer='mpc search composer'
alias msearchfile='mpc search filename'
alias msearchtitle='mpc search title'
alias msearchfilename='mpc search filename'
alias mupdate='mpc update'

#source ~/bashmp.sh
keychain ~/.ssh/id_dsa
keychain ~/.ssh/id_rsa
#. ~/.keychain/${HOSTNAME}-sh-gpg
. ~/.keychain/${HOSTNAME}-sh


#if ! type tac &>/dev/null; then
if ! [ -x "$(command -v tac)" ]; then
    alias tac="sed -n '1!G;\$p;h'"
fi

shopt -s extglob

set -o noclobber
set -o vi
bind '"\e."':yank-last-arg
bind -m vi-command ".":insert-last-argument
bind -m vi-insert "\C-l.":clear-screen
bind -m vi-insert "\C-a.":beginning-of-line
bind -m vi-insert "\C-e.":end-of-line
bind -m vi-insert "\C-w.":backward-kill-word

export HISTSIZE=2000
export HISTFILESIZE=2000
export HISTCONTROL=ignorespace:ignoredups

# easily done with tail and head, but where's the fun in that?
nth-from-end() {
    n=$1
    sed -n -e :a -e "\${P;q;};N;$((n+1)),\$D;ba" ${2}
}

most-recent() {
    ls -t|head -1
}

function google () {
    w3m http://www.google.co.uk/search?q="$@";
}

function swap-lines() {
    sed -n "${1}h;${2}{p;x;p};2,3!p" ${3}
}

function maths()
{
     echo "scale=6;${@/x/*}"|bc
}

function title()
{
    wget --quiet -O- "${1}" | sed -n 's/<title>\([^<]*\)<\/title>.*/\1/p'
}

alias set_last_dir='last_dir=$(expr "`history -p !$`" : "\(.*/\)");'

# Run commands on files in the same directory as the file in !$ (old = on last directory)
old() {
    args=($@)
    command=${args[@]:0:${#args[@]}-1}
    file=${args[${#args[@]}-1]}
    last_dir=$(expr "`history -p !$`" : "\(.*/\)")
    echo ${command} ${last_dir}${file}
    ${command} ${last_dir}${file}
}

old2() {
    command=$1
    shift
    last_dir=$(expr "`history -p !$`" : "\(.*/\)")
    echo ${command} ${last_dir}${file}
    for file in $@
    do
        echo \$ ${command} ${last_dir}${file}
        ${command} ${last_dir}${file}
        echo
    done
}

#get_default_net_if()
#{
#    sudo route -n get -inet default
#}

# for use with keyboard shortcuts
x()
{
    xclip -o
}
xspeak()
{
    echo "`x`"|festival --tts
    #espeak "`x`"
}

function ag_less()
{
    ag  --color "${@}"|less -RXF
}

function java_run()
{
    javac "${@}"
    java "${@/.java/}"
}

# useful for aliases
function chdir()
{

    if [ $# -eq 0 ]; then
        #cd -
        echo -n "popd: "
        popd
    else
        #cd $@
        #pwd
        echo -n "pushd: "
        pushd $@
    fi
}

function run_cmd()
{
    echo "${@}"
    "${@}"
}

alias gitlog="git log --since=yesterday --name-only"
alias gitlog1="git log -p -1"
alias gitfp1="git format-patch --stdout -1"
alias gitvimdiff="git difftool --tool=vimdiff"
alias gitgvimdiff="git difftool --tool=gvimdiff"
# useful after commit, before push
alias gitdevdiff="git diff origin/develop"
alias gitmasterdiff="git diff origin/master"
alias hadd='run_cmd $(history -p ^diff^add)'

alias dogs='run_cmd docker ps; run_cmd docker logs -f $(docker ps|sed 1d|awk '"'"'{print $1;}'"'"')'
alias undock='sudo umount `mount|grep -o "/var/lib/docker/aufs/mnt/[a-z0-9]*"`'
#alias gp="chdir ${VIEW_DIR} ; run_cmd git pull ; chdir"
#alias gs="chdir ${VIEW_DIR} ; run_cmd git stash ; chdir"
#alias gsp="chdir ${VIEW_DIR} ; run_cmd git stash pop ; chdir"
#alias gstat="chdir ${VIEW_DIR} ; run_cmd git -c color.status=always status | less -RXF ; chdir"


function esvapi()
{
    ref=${@// /%20}
    #wget --quiet -O- "http://www.esvapi.org/v2/rest/passageQuery?key=IP&passage=$ref"|lynx -dump -stdin|sed -n 's/\[[0-9]\+\] \?//g;/^Footnotes\|References/q;p'
    wget --quiet -O- "http://www.esvapi.org/v2/rest/passageQuery?key=IP&passage=$ref"|lynx -dump -stdin|sed -n '/^\s*$/d;s/\[[0-9]\+\] \?//g;p;/(ESV)$/q'
}

function bgw()
{
    bv=${1}
    shift
    ref=${@// /%20}
    wget --quiet -O- "http://www.biblegateway.com/passage/?search=${ref}&version=${bv}"|lynx -dump -stdin|sed -n "/^$@.*${bv}/,/^Footnotes/p"|grep -v "^Footnotes"
}

function dl()
{
    DAILY_LIGHT_URL="http://www.gnpcb.org/esv/mobile/devotions/daily.light/"
    #wget --quiet -O- "${DAILY_LIGHT_URL}"|lynx -dump -stdin|sed -n '/Morning/,/^\s*\[[0-9]/p'|sed -n '/^\s*\[[0-9]/q;p'|less
    if [ `date +%P` == "am" ]
    then
        wget --quiet -O- "${DAILY_LIGHT_URL}"|lynx -dump -stdin|sed -n '/^Morning$/,/^Evening$/p'|sed -n '/^\s*\[[0-9]/q;p'|head -n -2|less
    elif [ `date +%P`  == "pm" ]
    then
        wget --quiet -O- "${DAILY_LIGHT_URL}"|lynx -dump -stdin|sed -n '/^Evening$/,/^\s*\[[0-9]/p'|sed -n '/^\s*\[[0-9]/q;p'|head -n -1|less
    else
        echo "Neither AM nor PM!"
    fi
}

function pmp()
{
    #sed -n "/`date +'%B %-d.%p'|sed 's,AM,Morning,;s,PM,Evening,'`/,/`date --date=tomorrow +'%B %-d'`/p" rh_pmp.txt | head -n -1 | less
    if [ `date +%P` == "am" ]
    then
        #sed -n "/`date +'%B %-d.Morning'`/,/`date +'%B %-d.Evening'`/p" rh_pmp.txt | head -n -1 | tr -d '\f' | less
        xzcat rh_pmp.txt.xz | sed -n "/`date +'%B %-d.Morning'`/,/`date +'%B %-d.Evening'`/p" | head -n -1 | tr -d '\f' | less
    elif [ `date +%P` == "pm" ]
    then
        #sed -n "/`date +'%B %-d.Evening'`/,/`date --date=tomorrow +'%B %-d.Morning'`/p" rh_pmp.txt | head -n -1 | tr -d '\f' | less
        xzcat rh_pmp.txt.xz | sed -n "/`date +'%B %-d.Evening'`/,/`date --date=tomorrow +'%B %-d.Morning'`/p" | head -n -1 | tr -d '\f' | less
    else
        echo "Neither AM nor PM!"
    fi
}

function wsc()
{
    grep ^[QA]$1: wsc.txt | fold -s | col -b
}
function wscq()
{
    grep ^Q$1: wsc.txt | fold -s | col -b
}
function wsca()
{
    grep ^A$1: wsc.txt | fold -s | col -b
}

function wlc()
{
    grep ^[QA]$1: wlc.txt | fold -s | col -b
}
function wlcq()
{
    grep ^Q$1: wlc.txt | fold -s | col -b
}
function wlca()
{
    grep ^A$1: wlc.txt | fold -s | col -b
}

bible() {
    book=$1
    shift
    case $book in
    esv|niv|tniv|nasb|kjv|nkjv|nlt|hcsb|ylt|asv)
      book=${book^^*}
      ;;
    amp)
      book=${book^*}
      ;;
    net)
      book=NETfree
      ;;
    esac
    diatheke -b "${book}" -k "${*}"|grep -v '^$'|sed -e :a -e '/[0-9]: $/N;s/\([0-9][0-9]*\): \n/\1: /;ta'
}

pll() {
    bible esv $@
    bible niv $@
    bible tniv $@
    bible kjv $@
    bible nkjv $@
    bible nasb $@
    bible net $@
    bible hcsb $@
}


# cricinfo functions (check, replace_urls, add_urls, remove_urls count_urls)
function check()
{
    while read i ; do
        #wget --quiet -O- "${i}" | sed -n 's/<title>\([^|]*\).*/\1/p'
        wget --quiet -O- "${i}" | sed -n 's/<title>\([^|<]*\).*/\1/p'
    done < ${HOME}/urls.txt
}

function replace_urls()
{
    if [ $# -eq 0 ]; then
        cat >| ${HOME}/urls.txt
    else
        : >| ${HOME}/urls.txt
        for url in "$@"; do
            echo $url >> ${HOME}/urls.txt
        done
    fi
}

function add_urls()
{
    if [ $# -eq 0 ]; then
        cat >> ${HOME}/urls.txt
    else
        for url in "$@"; do
            echo $url >> ${HOME}/urls.txt
        done
    fi
}

function remove_urls()
{
    sed -i ${1}d ${HOME}/urls.txt
}

function count_urls()
{
    wc -l ${HOME}/urls.txt
}

#pgrep -f raven || screen -S rav -d -m ssh -X -R 6728:localhost:22221 wadh2045@raven.linux.ox.ac.uk
#pgrep -f ecs || screen -S ecs -d -m ssh -X -R 6728:localhost:22221 m04dm@booth1.ecs.ox.ac.uk
#pgrep -f crow || screen -S crow -d -m ssh -X -D 8080 wadh2045@crow.linux.ox.ac.uk


# cygwin
if [[ "x$OS" == "xWindows_NT" ]]; then
    export nodosfilewarning=1
    export C=/cygdrive/c
    export WIN_HOME=$C/Users/davidmo

    # eclipse :(
    [[ -d $WIN_HOME/workspaces ]] && export WORKSPACES=$WIN_HOME/workspaces
fi

true

# vim: set et ts=4 tw=120 :
