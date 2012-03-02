#! /bin/bash
#
# The main test driver

set -eu

spoolfile=test/chroot/var/spool/mail/$USER

rm -fr test/chroot
mkdir -p test/chroot/etc
mkdir -p test/chroot/var/state
mkdir -p test/chroot/var/spool/maild
mkdir -p test/chroot/var/spool/mail
cp /dev/null $spoolfile
mkdir -p test/chroot/var/run

here=$(/bin/pwd)

maild/maild -p 9999 -r $here/test/chroot -v -T -bT -q2m &
pid=$!
trap "kill -HUP $pid" EXIT HUP INT
echo maild PID=$pid

# wait for server to start:
sleep 4

generate_emails()
{
    for group in $*; do
	test/client-deliver.cl $USER@franz.com $group &
    done
}

generate_emails 1-20 21-40 41-60 61-80 81-100
sleep 2
generate_emails 101-199 200-299
max=299

sleep 5

echo WAIT
while [ `grep Subject: $spoolfile | wc -l` -ne $max ]; do
    sleep 1
done
echo DONE

if ! test/verify_emails.cl $spoolfile $max; then
    echo VERIFY FAILED
    exit 1
fi
