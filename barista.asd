(asdf:defsystem "barista"
  :description "macOS menu-bar application framework in Common Lisp (SBCL/CFFI)"
  :class :package-inferred-system
  :defsystem-depends-on ()
  :depends-on ("barista/main"
               ;; Explicit system-level deps that package-inferred-system
               ;; cannot discover from :import-from forms alone.
               "cffi"
               "trivial-main-thread"
               "bordeaux-threads"
               "log4cl"
               "log4cl-extras"
               "cl-colors"
               "alexandria"
               "fmt"
               "local-time"
               "local-time-duration"
               "uiop"
               "ubiquitous"
               "swank"
               "slynk")
  :build-operation "program-op"
  :build-pathname "barista"
  :entry-point "barista/main::main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c (eql (asdf:find-system "barista"))))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

