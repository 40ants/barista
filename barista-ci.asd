(defsystem "barista-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/barista/"
  :class :package-inferred-system
  :description "Provides CI settings for barista."
  :source-control (:git "https://github.com/40ants/barista")
  :bug-tracker "https://github.com/40ants/barista/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "barista-ci/ci"))
