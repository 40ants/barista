(asdf:defsystem "barista"
  :class :package-inferred-system
  :depends-on ("barista/main")
  
  :build-operation "program-op"
  :build-pathname "barista"
  :entry-point "barista/main::main")