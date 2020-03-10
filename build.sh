#!/bin/bash

~/tmp/ccl-dev/dx86cl64 \
       -e '(load "/Users/art/quicklisp/setup.lisp")' \
       -e '(ql:quickload :swank)' \
       -e '(push "~/projects/lisp/barista/" asdf:*central-registry*)' \
       -e '(ql:quickload :barista-plugins)' \
       -e '(ql:quickload :barista/main)' \
       -e '(require :build-application)' \
       -e "(ccl::build-application :name \"Barista\")"


# ~/tmp/ccl-dev/dx86cl64 \
#        -e '(load "/Users/art/quicklisp/setup.lisp")' \
#        -e '(ql:quickload :swank)' \
#        -e '(push "~/projects/lisp/barista/" asdf:*central-registry*)' \
#        -e '(ql:quickload :barista-plugins)' \
#        -e '(ql:quickload :barista/main)' \
#        -e '(require :build-application)' \
#        -e "(ccl:save-application \"barista\" :toplevel-function 'barista/main::main :prepend-kernel t)"

