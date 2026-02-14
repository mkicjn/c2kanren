#!/bin/sh

make || exit 1

echo
echo ./c2klisp rc.lisp ukanren.lisp
./c2klisp rc.lisp ukanren.lisp

echo
echo ./c2klisp rc.lisp ukanren-annotated.lisp
./c2klisp rc.lisp ukanren-annotated.lisp
