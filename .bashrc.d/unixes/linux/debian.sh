#!/bin/bash

# Work around the fact that Debian calls ack "ack-grep"
# (because another package already has the name ack).
ack() { ack-grep "$@" ;}
