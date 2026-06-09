(defsystem "barista-plugins"
  :class :package-inferred-system
  :pathname "plugins"
  :depends-on ("barista-plugins/pomodoro"
               "barista-plugins/git-reps"
               "barista-plugins/system-monitor/system-monitor"))
