#!/bin/sh

if command -v tcc >/dev/null; then
	demo () {
		echo
		echo $ ./c2klisp.c rc.lisp $1
		./c2klisp.c rc.lisp $1
	}
else
	make || exit 1
	demo () {
		echo
		echo $ ./c2klisp rc.lisp $1
		./c2klisp rc.lisp $1
	}
fi

demo literate-ukanren.lisp

demo sokuza-kanren.lisp

demo ukanren.lisp

demo minikanren.lisp
