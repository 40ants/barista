(asdf:defsystem "barista"
  :class :package-inferred-system
  :depends-on ("barista/main"))


(defmethod asdf:operate :before ((op asdf:load-op)
                                 (c (eql (asdf:find-system "barista")))
                                 &rest rest)
  (require "COCOA"))
