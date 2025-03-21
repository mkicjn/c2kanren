#!/bin/bash

# This script is meant to be used to informally compare what the two implementations output for each "test" file
# Note that a lot of these "tests" have become more like experiments, and aren't really tests in the usual sense
# It is expected that some of these are broken in one or both implementations

TIMEOUT=10

run_test_file () {
	echo "timeout $TIMEOUT $2 ../rc.lisp $1"
	timeout $TIMEOUT $2 ../rc.lisp $1
	if [ $? = 124 ]; then
		echo "NOTE: timed out"
	fi
}

make -C .. || exit 1
echo
for file in *.lisp; do
	run_test_file $file ../lisp
	run_test_file $file ../lisp-small
	echo
done
