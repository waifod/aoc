#!/bin/sh

time (for i in $(eval echo {1..1000}); do $1 $2 > /dev/null; done)
