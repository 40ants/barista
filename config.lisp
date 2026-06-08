(defpackage #:barista/config
  (:use #:cl)
  (:import-from #:ubiquitous
                #:restore
                #:value)
  (:import-from #:log4cl)
  (:export
   #:restore-config
   #:plugin-enabled-p
   #:set-plugin-enabled
   #:enabled-plugin-names))
(in-package #:barista/config)


;;; ---- Storage path --------------------------------------------------------

(defun config-pathname ()
  "Return the pathname for Barista's ubiquitous config file.
  Stored at ~/.config/barista/settings.lisp."
  (merge-pathnames #p"barista/settings.lisp"
                   (uiop:xdg-config-home)))


;;; ---- Initialisation ------------------------------------------------------

(defun restore-config ()
  "Load Barista configuration from disk.
  If the file does not exist (first launch), starts with empty storage --
  all plugins will be disabled until the user enables them via Settings."
  (let ((path (config-pathname)))
    (log:info "Restoring config from ~A" path)
    (handler-case
        (restore path)
      (ubiquitous:no-storage-file ()
        (log:info "No config file found at ~A; starting with defaults (all plugins disabled)" path)
        ;; Ensure the directory exists so ubiquitous can write later.
        (ensure-directories-exist path)))))


;;; ---- Plugin enable/disable -----------------------------------------------

(defun plugin-key (plugin-name)
  "Return the keyword key for PLUGIN-NAME (a symbol or string) in the config.
  Uses the symbol-name uppercased as a keyword, e.g. 'pomodoro -> :POMODORO."
  (intern (string-upcase (string plugin-name)) :keyword))

(defun plugin-enabled-p (plugin-name)
  "Return T if PLUGIN-NAME is enabled in the configuration.
  Defaults to NIL (disabled) when no value is stored -- this ensures a
  clean first-run experience where the system plugin is shown instead."
  (let ((key (plugin-key plugin-name)))
    (multiple-value-bind (val found)
        (value :plugins key)
      (if found val nil))))

(defun set-plugin-enabled (plugin-name enabled-p)
  "Persist the enabled/disabled state for PLUGIN-NAME.
  ubiquitous automatically writes to disk after each (setf value)."
  (let ((key (plugin-key plugin-name)))
    (log:info "Setting plugin ~A enabled=~A" plugin-name enabled-p)
    (setf (value :plugins key) (if enabled-p t nil))))

(defun enabled-plugin-names ()
  "Return a list of plugin name keywords that are enabled in the config.
  Only keys explicitly set to T are included."
  (let ((plugins-table (value :plugins)))
    (when (hash-table-p plugins-table)
      (loop for key being the hash-keys of plugins-table
            when (gethash key plugins-table)
              collect key))))
