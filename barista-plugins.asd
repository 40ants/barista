(defsystem "barista-plugins"
  :class :package-inferred-system
  :pathname "plugins"
  :depends-on ("barista-plugins/pomodoro"))


(defmethod asdf:operate :before ((op asdf:load-op)
                                 (c (eql (asdf:find-system "barista-plugins")))
                                 &rest rest)
  (require "COCOA"))
