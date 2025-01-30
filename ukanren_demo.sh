#!/bin/bash

demo () {
	echo "cat $1 | $2"
	cat $1 | $2
	echo
}

make || exit 1
echo
demo ukanren.lisp ./lisp
demo ukanren-small.lisp ./lisp-small
demo ukanren-annotated.lisp ./lisp
demo ukanren-old.lisp ./lisp
