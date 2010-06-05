#!/bin/sh
cd `dirname $0`
#sbcl --no-sysinit --no-userinit --load start.lisp
clisp -i start.lisp

