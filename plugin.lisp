(defpackage #:barista/plugin
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:barista/utils
                #:on-main-thread)
  (:import-from #:barista/classes
                #:get-string-form-for-macro)
  (:import-from #:barista/menu
                #:make-menu
                #:hide)
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
(in-package barista/plugin)


(defvar *running-plugins* nil
  "A plist of running plugins where key is a symbol of the class name
   and value is an instance of this class.")


(defvar *available-plugins* nil
  "A list of available plugins. Each item is a symbol which can be used
   as argument to a `start-plugin' function.")


(defclass base-plugin ()
  ((status-item :initform nil
                :type (or barista/classes:status-item
                          null)
                :documentation "An item for status bar."
                :accessor get-status-item)
   (threads :initform nil
            :type (or list
                      null)
            :documentation "A list of threads, created by plugin."
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
        do (log:info "Stopping thread" thread)
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
     (log:info "Stopping a plugin instance" class-name)
     (stop-plugin class-name))
   (log:info "Creating a plugin instance" class-name)
   (let ((instance (make-instance class-name)))
     (initialize-plugin instance))))

(defun restart-plugin (class-name)
  (start-plugin class-name))

(defun restart-plugins ()
  (mapc #'restart-plugin
        (running-plugins)))

(defun get-title-from (options)
  (let ((title (second (assoc :title options))))
    (get-string-form-for-macro title)))


(defun get-menu-from (options)
  (second (assoc :menu options)))


(defun get-delay-from (period)
  "Processes a period and returns a delay in seconds.
   Period can be a keyword like :second, :minute, :hour or a list:

   (list 15 :seconds)
   (list 5 :minute)
  "
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
                       ((:hour :hours) 3600))))
    
    (values (* (first period)
               multiplier)
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
                                   (log:info "Going to sleep after error")
                                   (sleep ,delay)
                                   (log:info "Awakening"))))
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
  "Processes :every forms and returns a list of lambdas which can be executed
   in threads to call user's code in specified periods of time."
  (loop for item in options
        when (eql (first item)
                  :every)
          collect (make-worker-form (second item)
                                    (cddr item))))


(defmacro defplugin (name (&rest slots) &body options)
  (declare (ignorable options))
  (let* ((title (or (get-title-from options)
                    (format nil "~A" name)))
         (menu-name (get-menu-from options))
         (menu-form (when menu-name
                      `(make-menu ',menu-name)))
         (workers-forms (get-workers-from options)))
    `(progn
       (defclass ,name (base-plugin)
         (,@slots))

       (pushnew ',name *available-plugins*)

       (defmethod initialize-plugin :after ((plugin ,name))
         ;; Here we need to bind *plugin* variable to make
         ;; it avalable during menu items creation so
         ;; that they be able to keep track which plugin
         ;; do they belong.
         (let ((*plugin* plugin))
           (setf (get-status-item plugin)
                 (make-instance 'barista/classes:status-item
                                :title ,title
                                :menu ,menu-form))
           (setf (get-threads plugin)
                 (list ,@workers-forms)))))))


(defmethod get-menu ((self base-plugin))
  (barista/classes:get-menu
   (get-status-item *plugin*)))


(defmethod (setf get-menu) (new-menu (self base-plugin))
  (setf (barista/classes:get-menu
         (get-status-item *plugin*))
        new-menu))


(defun %replace-menu (from to)
  (declare (ignorable from))
  ;; TODO: Сделать замену вложенных menu (как-то)
  (unless *plugin*
    (error "Function replace-menu should be called when *plugin* variable is defined."))
  (let ((old-menu (barista/classes:get-menu (get-status-item *plugin*))))
    (declare (ignorable old-menu))
    ;; TODO: add a check that menu has the same name as `from'
    (setf (barista/classes:get-menu (get-status-item *plugin*))
          (make-menu to))))


(defmacro replace-menu (from to)
  (check-type from symbol)
  (check-type to symbol)
  `(%replace-menu ',from ',to))


(defmacro with-plugin (name &body body)
  "Runs body with barista/vars:*plugin* bound to the named plugin.

   This macro is useful for debugging, for example, when you need
   to run your `update` function the same way, like barista is
   running it on schedule. Or to simulate a click on the menu.

   Argument name is not evaluated and considered a symbol, used in the
   `defplugin' call."
  (check-type name symbol)

  `(let ((*plugin*
           (getf barista/plugin::*running-plugins*
                 ',name)))
     (unless *plugin*
       (error "Plugin ~A is not running, start it with `start-plugin` call."
              name))
     ,@body))
