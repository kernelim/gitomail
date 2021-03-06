#!/bin/bash

#
# This script-let assumes that a single Gitolite server will send
# emails via a single email account, so that we protect from concurrent
# access to the account via flock, and also protect from concurrent
# runs from pushes to the same repository.
#
# A Gitolite admin may choose to put common Gitomail configuration under
# ~/.gitomailconf.yaml, and per-repository configuration under each
# repository's .git under gitomailconf.yaml
#

if [[ "$1" == "locked" ]] ; then
    mkdir -p ~/.gitomail
    if [[ -e ~/.gitomail/log ]] ; then
        SIZE=$(stat ~/.gitomail/log | grep 'Size: ' | awk -F" " '{print $2}')
        if [[ "$(( ${SIZE} > 100000 ))" == "1" ]] ; then
            mv ~/.gitomail/log{,.bak}
        fi
    fi
    echo "---------------------------------------------------------" >> ~/.gitomail/log
    echo "`date`: `gitomail --version` @ ${GL_REPO}" >> ~/.gitomail/log
    gitomail --repo-name ${GL_REPO} auto-mailer >> ~/.gitomail/log 2>&1
    echo "" >> ~/.gitomail/log
else
    flock ~/.gitomail-lock ${BASH_SOURCE[0]} locked  2>/dev/null >/dev/null &
    disown
fi
