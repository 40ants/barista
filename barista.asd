(asdf:defsystem "barista"
  :class :package-inferred-system
  :depends-on ("barista/main")
  
  :build-operation "program-op"
  :build-pathname "barista"
  :entry-point "barista/main::main")


(defmethod asdf:operate :before ((op asdf:load-op)
                                 (c (eql (asdf:find-system "barista")))
                                 &rest rest)
  (declare (ignorable rest))
  (require "COCOA"))
