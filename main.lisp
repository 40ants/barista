(defpackage #:barista/main
  (:use #:cl)
  ;; (:import-from #:slynk)
  (:import-from #:swank)
  (:import-from #:barista/menu)
  (:import-from #:barista/plugin
                #:get-available-plugins
                #:start-plugin)
  (:import-from #:log4cl-extras/config)
  (:export
   #:load-plugins
   #:start-plugins
   #:stop-plugins))
(in-package barista/main)


(defun load-plugin (filename)
  (log:info "Loading" filename)
  (load filename))


(defun load-plugins ()
  (log:info "Loading plugins")

  (mapc #'load-plugin (uiop:directory-files "~/.config/barista/plugins/")))


(defun start-plugins ()
  (log:info "Starting plugins")
  (mapc #'start-plugin
        (get-available-plugins)))


(defun stop-plugins ()
  (log:info "Starting plugins")
  (mapc #'barista/plugin:stop-plugin
        (barista/plugin:running-plugins)))


(defvar *interface* nil)


(defun main (&key (block t) (swank nil))
  (log4cl-extras/config:setup '(:level :debug
                                :appenders ((file
                                             :file "/tmp/barista.log"
                                             :layout :plain))))
  
  (when swank
    (log:info "Starting SWANK server")
    (swank:create-server :port 5007 :dont-close t :style :spawn))

  (load-plugins)

  ;; All commands which adds menus should be runned from CAPI process
  ;; that is why we create empty interface object here.
  (setf *interface* (capi:display (make-instance 'capi:interface :title "Barista")))
  (capi:hide-interface *interface*
                       ;; not iconify, just hide
                       nil)
  (capi:execute-with-interface *interface* 'start-plugins)

  (when block
    (log:info "Sleeping")
    (loop (sleep 5))))
