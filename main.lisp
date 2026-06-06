(defpackage #:barista/main
  (:use #:cl)
  (:import-from #:swank)
  (:import-from #:slynk)
  (:import-from #:trivial-main-thread)
  (:import-from #:barista/objc
                #:load-objc-frameworks
                #:send
                #:%cls)
  (:import-from #:barista/menu)
  (:import-from #:barista/plugin
                #:get-available-plugins
                #:start-plugin)
  (:import-from #:log4cl-extras/config)
  (:export
   #:load-plugins
   #:start-plugins
   #:stop-plugins
   #:main))
(in-package #:barista/main)


(defun load-plugin (filename)
  (log:info "Loading plugin file ~A" filename)
  (load filename))

(defun load-plugins ()
  "Load all user plugin files from ~/.config/barista/plugins/."
  (let ((dir (uiop:xdg-config-home "barista/plugins/")))
    (log:info "Loading plugins from ~A" dir)
    (when (uiop:directory-exists-p dir)
      (mapc #'load-plugin (uiop:directory-files dir)))))

(defun start-plugins ()
  "Instantiate and start all registered plugins.
  Must be called on the AppKit main thread."
  (log:info "Starting ~A plugin(s)" (length (get-available-plugins)))
  (mapc #'start-plugin (get-available-plugins)))

(defun stop-plugins ()
  "Stop all currently running plugins."
  (log:info "Stopping all plugins")
  (mapc #'barista/plugin:stop-plugin
        (barista/plugin:running-plugins)))


(defun main (&key (swank nil) (slynk nil) (port 5007))
  "Start the Barista menu-bar application.

  Loads user plugins, initialises AppKit on macOS thread 0 via
  trivial-main-thread, and enters the AppKit event loop (never returns
  normally).

  Keyword arguments:
    :swank  -- when true, start a SWANK server on PORT (default 5007)
    :slynk  -- when true, start a SLYNK server on PORT (default 5007)
    :port   -- port number for the SWANK/SLYNK server"
  (log4cl-extras/config:setup
   '(:level :debug
     :appenders ((file
                  :file "/tmp/barista.log"
                  :layout :plain))))

  (when swank
    (log:info "Starting SWANK server on port ~A" port)
    (swank:create-server :port port :dont-close t :style :spawn))

  (when slynk
    (log:info "Starting SLYNK server on port ~A" port)
    (slynk:create-server :port port :dont-close t :style :spawn))

  (load-plugins)

  ;; All AppKit / ObjC operations must run on macOS thread 0.
  ;; trivial-main-thread:with-body-in-main-thread hands off to that thread
  ;; and returns immediately on the calling thread (:blocking nil).
  (trivial-main-thread:with-body-in-main-thread (:blocking nil)
    (log4cl-extras/error:with-log-unhandled (:depth 100)

      ;; SBCL enables FP exception traps by default; AppKit's run loop
      ;; will trigger FLOATING-POINT-INVALID-OPERATION unless they are
      ;; disabled before handing control to NSApplication.
      #+sbcl (sb-int:set-floating-point-modes :traps nil)

      (log:info "Loading ObjC frameworks")
      (load-objc-frameworks)

      ;; Initialise NSApplication and suppress the Dock icon.
      (let ((app (send (%cls "NSApplication") "sharedApplication" :pointer)))
        (log:info "Setting activation policy to Accessory (no Dock icon)")
        ;; NSApplicationActivationPolicyAccessory = 1
        (send app "setActivationPolicy:" :long 1 :void)

        ;; Instantiate all plugins and create their NSStatusItems.
        (log:info "Starting plugins")
        (start-plugins)

        ;; Enter the AppKit event loop -- this call never returns.
        (log:info "Entering AppKit run loop")
        (send app "run" :void)))))
