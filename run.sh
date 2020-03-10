#!/bin/bash

# SWANK
# rlwrap ~/tmp/ccl-dev/dx86cl64 -e '(load "/Users/art/quicklisp/setup.lisp")' -e '(ql:quickload :swank)' -e '(push "~/projects/lisp/barista/" asdf:*central-registry*)' -e '(ql:quickload :barista-plugins)' -e '(swank:create-server :port 5007 :style :spawn :dont-close t)'

rlwrap ~/tmp/ccl-dev/dx86cl64 -e '(load "/Users/art/quicklisp/setup.lisp")' -e '(ql:quickload :swank)' -e '(push "~/projects/lisp/barista/" asdf:*central-registry*)' -e '(ql:quickload :barista-plugins)' -e '(ql:quickload :barista/main)' -e '(barista/main::main)'

# SLYNK
#rlwrap ~/tmp/ccl-dev/dx86cl64 -e '(load "/Users/art/quicklisp/setup.lisp")' -e '(ql:quickload :slynk)' -e '(push "~/projects/lisp/barista/" asdf:*central-registry*)' -e '(ql:quickload :barista-plugins)' -e '(slynk:create-server :port 5007 :style :spawn :dont-close t)' -e '(ql:quickload :barista/main)' -e '(barista/main::main)'

# rlwrap ~/tmp/ccl-dev/dx86cl64 -e '(load "/Users/art/quicklisp/setup.lisp")' -e '(push "~/projects/lisp/barista/" asdf:*central-registry*)' -e '(ql:quickload :barista-plugins)' -e '(ql:quickload :barista/main)' -e '(barista/main::main)'
