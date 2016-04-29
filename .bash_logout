# don't leave behind any stray ssh sessions - kill them when bash exits,
# unless it was running inside a tmux session (but the idea is to kill
# ssh sessions running inside tmux!)
if [ -n "${SSH_CONNECTION}" -a -z "${TMUX}" ]; then
    for ssh_host in $(grep 'Host\b' .ssh/config|cut -f2 -d' ') ; do
        ssh_pid=$(ps u|awk "/ssh ${ssh_host}/ && !/grep/{print $2}")
        [ -n "${ssh_pid}" ] && kill ${ssh_pid} 2>/dev/null
    done
fi

if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
fi
