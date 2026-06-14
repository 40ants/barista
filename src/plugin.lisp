(defpackage #:barista/plugin
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:barista/utils
                #:on-main-thread)
  (:import-from #:barista/classes
                #:get-string-form-for-macro
                #:get-image)
  (:import-from #:barista/menu
                #:make-menu
                #:hide
                #:initialize-status-item
                #:initialize-status-item-with-image)
  (:import-from #:barista/vars
                #:*debug*
                #:*plugin*)
  (:import-from #:fmt
                #:fmt)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
   (:export
    #:defplugin
    #:start-plugin
    #:stop-plugin
    #:restart-plugin
    #:get-plugin-instance
    #:is-plugin-running
    #:get-title
    #:get-image
    #:replace-menu
    #:get-available-plugins
    #:get-menu
    #:running-plugins
    #:start-enabled-plugins

    #:with-plugin
    #:restart-plugins
    #:initialize-plugin
    #:get-status-item))
(in-package #:barista/plugin)


(defvar *running-plugins* (make-hash-table)
  "A hash-table of running plugins where key is a symbol of the class name
  and value is an instance of this class.")

(defvar *available-plugins* nil
  "A list of available plugins. Each item is a symbol which can be used
  as argument to a start-plugin function.")


(defgeneric get-title (plugin)
  (:documentation "Return the current status-bar label of PLUGIN."))

(defgeneric get-image (plugin)
  (:documentation "Return the current status-bar icon of PLUGIN."))

(defgeneric get-menu (plugin)
  (:documentation "Return the menu-thunk of PLUGIN's status item."))

(defgeneric get-status-item (plugin)
  (:documentation "Return the BARISTA/CLASSES:STATUS-ITEM wrapper of PLUGIN."))


(defclass base-plugin ()
  ((status-item :initform nil
                :type (or barista/classes:status-item null)
                :documentation "A barista/classes:status-item wrapping the AppKit NSStatusItem."
                :accessor get-status-item)
   (threads :initform nil
            :type (or list null)
            :documentation "A list of BT threads created by this plugin."
            :accessor get-threads)))


(defmethod get-title ((plugin base-plugin))
  (barista/classes:get-title (get-status-item plugin)))

(defmethod (setf get-title) (value (plugin base-plugin))
  (setf (barista/classes:get-title (get-status-item plugin))
        value))

(defmethod get-image ((plugin base-plugin))
  (barista/classes:get-image (get-status-item plugin)))

(defmethod (setf get-image) (value (plugin base-plugin))
  "Set the status-bar icon for PLUGIN from VALUE.
  VALUE may be a CL pathname, a namestring, or an NSImage CFFI pointer
  (e.g. from make-ns-image).  Dispatched on the main thread automatically."
  (setf (barista/classes:get-image (get-status-item plugin))
        value))

(defun running-plugins ()
  (loop for name being the hash-keys of *running-plugins*
        collect name))

(defun get-plugin-instance (class-name)
  (gethash class-name *running-plugins*))

(defun is-plugin-running (class-name)
  (not (null (get-plugin-instance class-name))))


(defun stop-threads (plugin)
  (loop for thread in (get-threads plugin)
        do (log:info "Stopping thread ~A" thread)
           (when (bordeaux-threads:thread-alive-p thread)
             (handler-case (bordeaux-threads:destroy-thread thread)
               (error (e)
                 (log:warn "Error destroying thread ~A: ~A" thread e))))))


(defun %stop-plugin-sync (plugin)
  "Stop PLUGIN synchronously.  Must be called on the AppKit main thread."
  (stop-threads plugin)
  (when (get-status-item plugin)
    (hide (get-status-item plugin)))
  (remhash (class-name (class-of plugin)) *running-plugins*))

(defgeneric stop-plugin (plugin)
  (:documentation "Stop PLUGIN, dispatching to the AppKit main thread via GCD.")
  (:method ((plugin symbol))
    (on-main-thread
      (let ((obj (get-plugin-instance plugin)))
        (when obj
          (%stop-plugin-sync obj)))))
  (:method ((plugin base-plugin))
    (on-main-thread
      (%stop-plugin-sync plugin))))


(defgeneric initialize-plugin (plugin)
  (:method ((plugin base-plugin))
    (setf (gethash (class-name (class-of plugin)) *running-plugins*)
          plugin)))


(defun get-available-plugins ()
  *available-plugins*)

(defun %start-plugin-sync (class-name)
  "Start CLASS-NAME synchronously.  Must be called on the AppKit main thread."
  (when (is-plugin-running class-name)
    (log:info "Stopping existing plugin instance ~A" class-name)
    (stop-plugin class-name))
  (log:info "Creating plugin instance ~A" class-name)
  (let ((instance (make-instance class-name)))
    (initialize-plugin instance)))

(defun start-plugin (class-name)
  "Start CLASS-NAME, dispatching to the AppKit main thread via GCD.
  Safe to call from any thread.  Returns immediately."
  (on-main-thread
    (%start-plugin-sync class-name)))

(defun restart-plugin (class-name)
  (start-plugin class-name))

(defun restart-plugins ()
  (mapc #'restart-plugin (running-plugins)))

(defun start-enabled-plugins ()
  "Start only the plugins that are enabled in the configuration.
  Must be called directly on the AppKit main thread (not via GCD) so that
  all plugins are registered in *running-plugins* before the caller checks
  visibility (e.g. ensure-system-plugin)."
  (let ((available (get-available-plugins)))
    (log:info "Starting enabled plugins from ~A available" (length available))
    (dolist (name available)
      (if (uiop:symbol-call :barista/config :plugin-enabled-p name)
          (progn
            (log:info "Starting enabled plugin ~A" name)
            (%start-plugin-sync name))
          (log:info "Skipping disabled plugin ~A" name)))))


;;; ---- defplugin helper utilities ------------------------------------------

(defun get-title-from (options)
  (let ((title (second (assoc :title options))))
    (get-string-form-for-macro title)))

(defun get-menu-from (options)
  (second (assoc :menu options)))

(defun get-image-from (options)
  "Return the :image spec from OPTIONS, or NIL.
  The spec may be a pathname, a string, or a list (:image PATH :size N :template T)."
  (let ((entry (assoc :image options)))
    (when entry
      ;; Support both (:image PATH) and (:image PATH :size N :template T)
      (rest entry))))

(defun get-delay-from (period)
  "Process a period spec and return (values delay-seconds description-string).
  Period may be a keyword like :second, :minute, :hour, or a list:
    (15 :seconds)
    (5 :minutes)"
  (let* ((period (etypecase period
                   (keyword (list 1 period))
                   (list period)))
         (as-string (fmt nil
                         (:a (first period))
                         " "
                         (:a (second period) :downcase)))
         (multiplier (ecase (second period)
                       ((:second :seconds) 1)
                       ((:minute :minutes) 60)
                       ((:hour   :hours)   3600))))
    (values (* (first period) multiplier)
            as-string)))

(defun make-worker-form (period code)
  (multiple-value-bind (delay description)
      (get-delay-from period)
     `(make-thread
      (lambda ()
        (loop do
          (handler-case
           (handler-bind ((error (lambda (condition)
                                   (when *debug*
                                     (invoke-debugger condition))
                                   (log:info "Sleeping after error")
                                   (sleep ,delay)
                                   (log:info "Waking up"))))
             (with-log-unhandled ()
               (with-fields (:plugin *plugin*)
                 ,@code))
             (sleep ,delay))
           (error ()))))
      :name (fmt nil
                 (:a *plugin*)
                 " every "
                 ,description)
      :initial-bindings (append (list (cons '*plugin* *plugin*))
                                bordeaux-threads:*default-special-bindings*))))

(defun get-workers-from (options)
  "Process :every forms and return a list of make-thread call forms."
  (loop for item in options
        when (eql (first item) :every)
          collect (make-worker-form (second item) (cddr item))))


;;; ---- defplugin macro -----------------------------------------------------

(defmacro defplugin (name (&rest slots) &body options)
  "Define a Barista plugin class with background workers and a menu-bar item.

  Options:
    (:title EXPR)                       -- initial status-bar title (text or attributed)
    (:image PATH &key size template)    -- status-bar icon from an image file.
                                           PATH is a pathname or string evaluated
                                           at plugin start time.
                                           SIZE (default 18) scales the icon.
                                           TEMPLATE T for monochrome/symbolic icons
                                           that should adapt to dark/light mode;
                                           leave NIL (default) for colour images.
    (:menu SYMBOL)                      -- name of a defmenu to show on click
    (:every PERIOD FORMS)               -- background worker: evaluate FORMS every PERIOD

  :title and :image are mutually exclusive; :image takes precedence when both
  are supplied."
  (declare (ignorable options))
  (let ((title         (or (get-title-from options)
                           (format nil "~A" name)))
        (image-spec    (get-image-from options))
        (menu-name     (get-menu-from options))
        (workers-forms (get-workers-from options)))
    `(progn
       (defclass ,name (base-plugin)
         (,@slots))

       (pushnew ',name *available-plugins*)

       (defmethod initialize-plugin :after ((plugin ,name))
         ;; Bind *plugin* so that menu item constructors and defmenu
         ;; forms can capture the correct plugin instance.
         (let ((*plugin* plugin))
           ;; Build the menu-thunk: a closure that constructs the NSMenu
           ;; on demand each time the status item is clicked.
           ;; *plugin* must be rebound inside the thunk because it runs
           ;; on the AppKit main thread where *plugin* is otherwise NIL.
           (let ((menu-thunk ,(if menu-name
                                `(let ((mn ',menu-name)
                                       (p  plugin))
                                   (lambda ()
                                     (let ((*plugin* p))
                                       (make-menu mn))))
                                `nil)))
             (setf (get-status-item plugin)
                   (make-instance 'barista/classes:status-item
                                  :title ,title
                                  :menu-thunk menu-thunk))
             ;; Wire up the AppKit NSStatusItem.  When an :image option was
             ;; supplied we use the image initialiser; otherwise plain text.
             ,(if image-spec
                  `(initialize-status-item-with-image
                    (get-status-item plugin)
                    ,(first image-spec)
                    ,@(rest image-spec))
                  `(initialize-status-item (get-status-item plugin))))
           (setf (get-threads plugin)
                 (list ,@workers-forms)))))))


;;; ---- get-menu / set-menu (plugin-level accessors) -----------------------

(defmethod get-menu ((self base-plugin))
  "Return the menu-thunk of the plugin's status item."
  (barista/classes:get-menu-thunk (get-status-item *plugin*)))

(defmethod (setf get-menu) (new-menu-thunk (self base-plugin))
  "Replace the menu-thunk on the plugin's status item.
  NEW-MENU-THUNK should be a nullary function returning an NSMenu pointer,
  or an NSMenu CFFI pointer directly (wrapped into a thunk automatically)."
  (let ((thunk (if (functionp new-menu-thunk)
                   new-menu-thunk
                   (let ((m new-menu-thunk))
                     (lambda () m)))))
    (setf (barista/classes:get-menu-thunk (get-status-item *plugin*))
          thunk)))


;;; ---- replace-menu --------------------------------------------------------

(defun %replace-menu (from to)
  (declare (ignorable from))
  (unless *plugin*
    (error "replace-menu must be called when *plugin* is bound."))
  (let ((mn to)
        (p  *plugin*))
    (setf (barista/classes:get-menu-thunk (get-status-item *plugin*))
          (lambda ()
            (let ((*plugin* p))
              (make-menu mn))))))

(defmacro replace-menu (from to)
  (check-type from symbol)
  (check-type to symbol)
  `(%replace-menu ',from ',to))


;;; ---- with-plugin (debugging helper) -------------------------------------

(defmacro with-plugin (name &body body)
  "Run BODY with barista/vars:*plugin* bound to the named running plugin.
  Useful for interactive debugging."
  (check-type name symbol)
  `(let ((*plugin*
           (gethash ',name barista/plugin::*running-plugins*)))
     (unless *plugin*
       (error "Plugin ~A is not running; start it with start-plugin first." ',name))
     ,@body))
