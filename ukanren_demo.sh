#!/bin/bash

demo () {
	echo "$1 rc.lisp $2"
	$1 rc.lisp $2
	echo
}

make || exit 1
echo
demo ./lisp ukanren.lisp
demo ./lisp-small ukanren-small.lisp
demo ./lisp ukanren-annotated.lisp
demo ./lisp ukanren-old.lisp
