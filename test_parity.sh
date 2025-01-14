#!/bin/bash

TIMEOUT=10

run_test_file () {
	echo "cat $1 | timeout $TIMEOUT $2"
	cat $1 | timeout $TIMEOUT $2
	if [ $? = 124 ]; then
		echo "NOTE: timed out"
	fi
}

make || exit 1
for file in lisp-tests/*; do
	run_test_file $file ./lisp
	run_test_file $file ./lisp-small
	echo
done
