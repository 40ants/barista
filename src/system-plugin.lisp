(defpackage #:barista/system-plugin
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:barista/classes
                #:status-item
                #:get-menu-thunk)
  (:import-from #:barista/menu
                #:hide
                #:initialize-status-item
                #:initialize-status-item-with-image
                #:build-menu
                #:add-item)
  (:import-from #:barista/plugin
                #:get-available-plugins
                #:running-plugins)
  (:import-from #:barista/config
                #:plugin-enabled-p
                #:set-plugin-enabled)

  (:export
   #:ensure-system-plugin
   #:show-system-plugin
   #:hide-system-plugin
   #:update-system-plugin-visibility
   #:make-plugins-submenu))
(in-package #:barista/system-plugin)


;;; ---- State ---------------------------------------------------------------

(defvar *system-plugin-item* nil
  "The barista/classes:status-item for the system plugin, or NIL if not created yet.")

(defvar *system-plugin-visible* nil
  "T when the system plugin status item is currently shown in the menu bar.")


;;; ---- Icon path resolution ------------------------------------------------

(defun find-icon-path ()
  "Return the pathname to barista-icon.png, searching in order:
  1. BARISTA_RESOURCES_DIR environment variable (set by the .app launcher)
  2. The ASDF source directory of the :barista system (development mode)"
  (let ((env-dir (uiop:getenv "BARISTA_RESOURCES_DIR")))
    (cond
      (env-dir
       (let ((p (merge-pathnames #p"barista-icon.png"
                                 (uiop:parse-native-namestring env-dir))))
         (when (probe-file p) p)))
      (t
       (let* ((src-dir (asdf:system-source-directory :barista))
              (p (merge-pathnames #p"images/icon.png" src-dir)))
         (when (probe-file p) p))))))


;;; ---- Settings menu -------------------------------------------------------

(defun make-plugins-submenu ()
  "Build the Settings > Plugins submenu dynamically.
  Each item shows the plugin name with a native checkmark if currently enabled."
  (build-menu
    (let ((available (get-available-plugins)))
      (if available
          (dolist (name available)
            (let ((enabled   (plugin-enabled-p name))
                  (label     (string-downcase (symbol-name name)))
                  (captured  name))
              (add-item label
                        :state    enabled
                        :callback (lambda ()
                                    (toggle-plugin captured)))))
          (add-item "(no plugins loaded)" :callback nil)))))

(defun make-settings-menu ()
  "Build the top-level Settings menu shown on system-plugin click."
  (let ((plugins-menu (make-plugins-submenu)))
    (build-menu
      (add-item "Plugins"
                :submenu plugins-menu)
      (add-item "Quit"
                :callback (lambda ()
                            (log:info "Quitting via system plugin")
                            (uiop:quit 0))))))


;;; ---- Plugin toggle -------------------------------------------------------

(defun toggle-plugin (plugin-name)
  "Toggle the enabled state of PLUGIN-NAME, persisting to config,
  and start/stop the plugin synchronously.
  Called from a menu callback, so we are already on the AppKit main thread;
  we call the sync variants directly to ensure *running-plugins* is up to
  date before update-system-plugin-visibility checks it."
  (let ((currently-enabled (plugin-enabled-p plugin-name)))
    (log:info "Toggling plugin ~A (currently enabled=~A)" plugin-name currently-enabled)
    (set-plugin-enabled plugin-name (not currently-enabled))
    (if currently-enabled
        (let ((obj (barista/plugin:get-plugin-instance plugin-name)))
          (when obj
            (barista/plugin::%stop-plugin-sync obj)))
        (barista/plugin::%start-plugin-sync plugin-name)))
  (update-system-plugin-visibility))


;;; ---- Visibility management -----------------------------------------------

(defun any-plugins-running-p ()
  "Return T if at least one user plugin is currently running."
  (not (null (running-plugins))))

(defun show-system-plugin ()
  "Show the system plugin status item in the menu bar.
  Creates it on first call; no-op if already visible."
  (unless *system-plugin-item*
    (log:info "Creating system plugin status item")
    (let ((item (make-instance 'status-item
                               :title "Barista"
                               :menu-thunk nil
                               :system-item-p t))
          (icon (find-icon-path)))
      (setf (get-menu-thunk item)
            (lambda () (make-settings-menu)))
      (setf *system-plugin-item* item)
      (if icon
          (initialize-status-item-with-image item icon)
          (progn
            (log:warn "System plugin icon not found; falling back to text title")
            (initialize-status-item item)))))
  (setf *system-plugin-visible* t)
  (log:info "System plugin shown"))

(defun hide-system-plugin ()
  "Hide the system plugin status item from the menu bar.
  No-op if not visible."
  (when (and *system-plugin-visible* *system-plugin-item*)
    (log:info "Hiding system plugin")
    (hide *system-plugin-item*)
    (setf *system-plugin-item* nil
          *system-plugin-visible* nil)))

(defun update-system-plugin-visibility ()
  "Show the system plugin if no user plugins are running; hide it otherwise.
  Must be called from the AppKit main thread (via on-main-thread when in doubt)."
  (if (any-plugins-running-p)
      (hide-system-plugin)
      (show-system-plugin)))


;;; ---- Entry point ---------------------------------------------------------

(defun ensure-system-plugin ()
  "Call after start-enabled-plugins to show the system plugin when needed.
  Must be called on the AppKit main thread."
  (update-system-plugin-visibility))
