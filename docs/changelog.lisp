(uiop:define-package #:barista-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:barista-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "SLIME"
                              "ASDF"
                              "REPL"
                              "HTTP"
                              "SBCL"
                              "CFFI"
                              "GCD"
                              "ObjC"
                              "AppKit"
                              "macOS"
                              "DSL"
                              "GCD"))
  (0.1.0 2025-06-14
         "* Initial public version.
* Core framework with ObjC bridge, plugin system, menu DSL and GCD dispatch.
* Bundled plugins: Pomodoro timer, Clipboard history, Currency rates, System monitor."))
