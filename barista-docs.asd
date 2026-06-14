(defsystem "barista-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/barista/"
  :class :package-inferred-system
  :description "Provides documentation for barista."
  :source-control (:git "https://github.com/40ants/barista")
  :bug-tracker "https://github.com/40ants/barista/issues"
  :pathname "docs"
  :depends-on ("barista"
               "barista-docs/index"))
