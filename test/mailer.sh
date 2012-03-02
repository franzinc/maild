#! /bin/bash
#
# we are given 1 arg:
# $1 = mailbox

set -eu

#echo args: $*

waitfor=240

(
    flock -x -w $waitfor 200

    cat >> $1
    echo "" >> $1

) 200>/tmp/maild-mailer-lock.exclusivelock
