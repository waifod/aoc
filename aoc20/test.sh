#!/bin/sh

time (for i in $(eval echo {1..10000}); do $1 $2 > /dev/null; done)
