#!/bin/bash

set -a
SYSTEM_PATH=/usr/local/bin:/usr/bin:/bin:/usr/games:/usr/local/sbin:/usr/sbin:/sbin
set +a

# Work around the fact that Debian calls ack "ack-grep"
# (which it does because another package already has the name ack).
ack() { ack-grep "$@" ;}
