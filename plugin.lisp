(defpackage #:barista/plugin
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:barista/utils
                #:on-main-thread)
  (:import-from #:barista/classes
                #:get-string-form-for-macro)
  (:import-from #:barista/menu
                #:make-menu
                #:hide
                #:initialize-status-item)
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
   #:replace-menu
   #:get-available-plugins
   #:get-menu
   #:running-plugins

   #:with-plugin
   #:restart-plugins))
(in-package #:barista/plugin)


(defvar *running-plugins* nil
  "A plist of running plugins where key is a symbol of the class name
  and value is an instance of this class.")

(defvar *available-plugins* nil
  "A list of available plugins. Each item is a symbol which can be used
  as argument to a start-plugin function.")


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

(defun running-plugins ()
  (loop for name in *running-plugins* by #'cddr
        collect name))

(defun get-plugin-instance (class-name)
  (getf *running-plugins* class-name))

(defun is-plugin-running (class-name)
  (not (null (get-plugin-instance class-name))))


(defun stop-threads (plugin)
  (loop for thread in (get-threads plugin)
        do (log:info "Stopping thread ~A" thread)
           (bordeaux-threads:destroy-thread thread)))


(defgeneric stop-plugin (plugin)
  (:method ((plugin symbol))
    (on-main-thread
      (let ((obj (get-plugin-instance plugin)))
        (when obj
          (stop-plugin obj)))))
  (:method ((plugin base-plugin))
    (on-main-thread
      (stop-threads plugin)
      (when (get-status-item plugin)
        (hide (get-status-item plugin)))
      (setf (getf *running-plugins* (class-name (class-of plugin)))
            nil))))


(defgeneric initialize-plugin (plugin)
  (:method ((plugin base-plugin))
    (setf (getf *running-plugins* (class-name (class-of plugin)))
          plugin)))


(defun get-available-plugins ()
  *available-plugins*)

(defun start-plugin (class-name)
  (on-main-thread
    (when (is-plugin-running class-name)
      (log:info "Stopping existing plugin instance ~A" class-name)
      (stop-plugin class-name))
    (log:info "Creating plugin instance ~A" class-name)
    (let ((instance (make-instance class-name)))
      (initialize-plugin instance))))

(defun restart-plugin (class-name)
  (start-plugin class-name))

(defun restart-plugins ()
  (mapc #'restart-plugin (running-plugins)))


;;; ---- defplugin helper utilities ------------------------------------------

(defun get-title-from (options)
  (let ((title (second (assoc :title options))))
    (get-string-form-for-macro title)))

(defun get-menu-from (options)
  (second (assoc :menu options)))

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
          (ignore-errors
           (handler-bind ((error (lambda (condition)
                                   (when *debug*
                                     (invoke-debugger condition))
                                   (log:info "Sleeping after error")
                                   (sleep ,delay)
                                   (log:info "Waking up"))))
             (with-log-unhandled ()
               (with-fields (:plugin *plugin*)
                 ,@code))
             (sleep ,delay)))))
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
    (:title EXPR)          -- initial status-bar title
    (:menu SYMBOL)         -- name of a defmenu to show on click
    (:every PERIOD FORMS)  -- background worker: evaluate FORMS every PERIOD"
  (declare (ignorable options))
  (let* ((title         (or (get-title-from options)
                            (format nil "~A" name)))
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
           (let ((menu-thunk ,(if menu-name
                                  `(let ((mn ',menu-name))
                                     (lambda () (make-menu mn)))
                                  `nil)))
             (setf (get-status-item plugin)
                   (make-instance 'barista/classes:status-item
                                  :title ,title
                                  :menu-thunk menu-thunk))
             ;; Wire up the AppKit NSStatusItem (must be on main thread;
             ;; start-plugin already ensures this via on-main-thread).
             (initialize-status-item (get-status-item plugin)))
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
  (let ((mn to))
    (setf (barista/classes:get-menu-thunk (get-status-item *plugin*))
          (lambda () (make-menu mn)))))

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
           (getf barista/plugin::*running-plugins* ',name)))
     (unless *plugin*
       (error "Plugin ~A is not running; start it with start-plugin first." ',name))
     ,@body))
