#!/bin/bash

demo () {
	echo "cat $1 | $2"
	cat $1 | $2
	echo
}

make || exit 1
demo ukanren-small.lisp ./lisp-small
demo ukanren.lisp ./lisp
demo ukanren-annotated.lisp ./lisp
demo ukanren-old.lisp ./lisp
