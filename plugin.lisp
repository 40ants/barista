(defpackage #:barista/plugin
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:barista/utils)
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
  (:export
   #:defplugin
   #:start-plugin
   #:stop-plugin
   #:get-plugin-instance
   #:is-plugin-running
   #:get-title
   #:replace-menu
   #:get-available-plugins))
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
    (let ((obj (get-plugin-instance plugin)))
      (when obj
        (stop-plugin obj))))
  (:method ((plugin base-plugin))
    (stop-threads plugin)
    (when (get-status-item plugin)
      (hide (get-status-item plugin)))
    (setf (getf *running-plugins* (class-name (class-of plugin)))
          nil)))


(defgeneric initialize-plugin (plugin)
  (:method ((plugin base-plugin))
    (setf (getf *running-plugins* (class-name (class-of plugin)))
          plugin)))


(defun get-available-plugins ()
  *available-plugins*)


(defun start-plugin (class-name)
  (when (is-plugin-running class-name)
    (log:info "Stopping a plugin instance" class-name)
    (stop-plugin class-name))

  (log:info "Creating a plugin instance" class-name)
  (let ((instance (make-instance class-name)))
    (initialize-plugin instance)))


(defun get-title-from (options)
  (second (assoc :title options)))


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
                                   (log:error "Unhandled exception" *plugin* ,description condition)
                                   (when *debug*
                                     (invoke-debugger condition))
                                   (sleep ,delay))))
             ,@code
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
