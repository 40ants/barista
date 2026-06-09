(defpackage #:barista/main
  (:use #:cl)
  (:import-from #:swank)
  (:import-from #:slynk)
  (:import-from #:40ants-logging)
  (:import-from #:trivial-main-thread)
  (:import-from #:defmain)
  (:import-from #:barista/objc
                #:load-objc-frameworks
                #:send
                #:%cls)
  (:import-from #:barista/menu)
  (:import-from #:barista/plugin
                #:get-available-plugins
                #:start-plugin
                #:start-enabled-plugins)
  (:import-from #:barista/config
                #:restore-config)
  (:import-from #:barista/system-plugin
                #:ensure-system-plugin)
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
  "Instantiate and start all registered plugins regardless of config.
  Must be called on the AppKit main thread.
  NOTE: Prefer start-enabled-plugins for normal startup."
  (log:info "Starting ~A plugin(s)" (length (get-available-plugins)))
  (mapc #'start-plugin (get-available-plugins)))

(defun stop-plugins ()
  "Stop all currently running plugins."
  (log:info "Stopping all plugins")
  (mapc #'barista/plugin:stop-plugin
        (barista/plugin:running-plugins)))



(defmain:defmain (main :program-name "barista") ((slynk-port "Port to listen for SLYNK connection on."
                                                             :short nil)
                                                 (swank-port "Port to listen for SWANK connection on."
                                                             :short nil)
                                                 (log-file "Output logs to this file")
                                                 (verbose "Show debug logs"
                                                          :flag t))
  "Start the Barista menu-bar application.

  Loads user plugins, initialises AppKit on macOS thread 0 via
  trivial-main-thread, and enters the AppKit event loop (never returns
  normally)."

  (let ((log-level (if verbose
                     :debug
                     :info)))
    (cond
      (log-file
       (40ants-logging:setup-for-backend :level log-level
                                         :filename log-file))
      (t
       (40ants-logging:setup-for-cli :level log-level))))

  (log:info "Starting Barista")

  (when swank-port
    (log:info "Starting SWANK server on port ~A" swank-port)
    (swank:create-server :port (parse-integer swank-port :junk-allowed t)
                         :dont-close t
                         :style :spawn))

  (when slynk-port
    (log:info "Starting SLYNK server on port ~A" slynk-port)
    (slynk:create-server :port (parse-integer slynk-port :junk-allowed t)
                         :dont-close t
                         :style :spawn))

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

      ;; Restore config on the main thread so that ubiquitous *storage* and
      ;; *storage-pathname* are set in the same thread where (setf value)
      ;; will later be called from menu callbacks.  Moving restore-config
      ;; here ensures reads and writes share the same dynamic environment.
      (log:info "Restoring config")
      (restore-config)

      ;; Initialise NSApplication and suppress the Dock icon.
      (let ((app (send (%cls "NSApplication") "sharedApplication" :pointer)))
        (log:info "Setting activation policy to Accessory (no Dock icon)")
        ;; NSApplicationActivationPolicyAccessory = 1
        (send app "setActivationPolicy:" :long 1 :void)

        ;; Start only plugins enabled in the configuration.
        ;; On first run (no config file) no plugins are enabled,
        ;; so the system plugin will be shown instead.
        (log:info "Starting enabled plugins")
        (start-enabled-plugins)

        ;; Show the system plugin if no user plugins are running.
        (log:info "Checking system plugin visibility")
        (ensure-system-plugin)

        ;; Enter the AppKit event loop -- this call never returns.
        (log:info "Entering AppKit run loop")
        (send app "run" :void)))))
