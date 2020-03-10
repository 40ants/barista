(defpackage #:barista/main
  (:use #:cl)
  ;; (:import-from #:slynk)
  (:import-from #:swank)
  (:import-from #:barista/menu)
  (:import-from #:barista/plugin
                #:get-available-plugins
                #:start-plugin)
  (:export
   #:load-plugins
   #:start-plugins))
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


(defun main ()
  ;; (format t "ARGS: ~A~%"
  ;;         (uiop:command-line-arguments))
  (log:config :sane2 :debug :daily "/tmp/barista.log")

  ;; (log:info "Starting SLYNK server")
  ;; (slynk:create-server :port 5007 :dont-close t)

  (log:info "Starting SWANK server")
  (swank:create-server :port 5007 :dont-close t :style :spawn)

  (load-plugins)
  (start-plugins)

  (when sleep
    (log:info "Sleeping")
    (loop (sleep 5))))


  (push "/Users/art/projects/lisp/cl-github/" asdf:*central-registry*)
  
  ;; (load-plugins)
  ;; (start-plugins)
  
  (log:info "Sleeping")
  (loop (sleep 5)))
