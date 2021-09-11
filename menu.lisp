(defpackage #:barista/menu
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:barista/classes
                #:get-string-form-for-macro
                #:get-callback
                #:get-status-item
                #:get-menu
                #:get-title
                #:get-status-bar)
  (:import-from #:barista/vars
                #:*plugin*)
  (:import-from #:barista/utils
                #:open-url)
  (:export
   #:defmenu
   #:hide
   #:make-menu
   #:build-menu
   #:add-item))
(in-package barista/menu)


(defvar *menu-constructors* nil)

;; From NSStatusBar.h
(defconstant NSVariableStatusItemLength -1.0)


(defun key-pressed-p (event key)

  (let ((flags (objc:invoke event
                            "modifierFlags")))
    ;; https://www.oreilly.com/library/view/cocoa-in-a/0596004621/re186.html
    (case key
      (:shift     (plusp (logand flags
                                 (expt 2 17))))
      (:control   (plusp (logand flags
                                 (expt 2 18))))
      (:alt       (plusp (logand flags
                                 (expt 2 19))))
      (:caps-lock (plusp (logand flags
                                 (expt 2 16))))
      (:command   (plusp (logand flags
                                 (expt 2 20))))
      (t nil))))


(defparameter *barista-menu*
  (flet ((just-log (&rest args)
           (log:info "Main menu called with" args)))
    (build-menu
      (add-item "Restart all plugins"
                :callback (lambda (menu-item)
                            (declare (ignore menu-item))
                            (uiop:symbol-call :barista/plugin :restart-plugins))))))

(defun get-barista-menu ()
  *barista-menu*)


(objc:define-objc-method ("onBaristaItemClick" :void)
                         ((self barista/classes::status-item))

  (let* ((event (objc:invoke (objc:invoke "NSApplication" "sharedApplication")
                             "currentEvent"))
         (menu (if (key-pressed-p event :alt)
                   (get-barista-menu)
                   (get-menu self)))
         (button (objc:invoke (get-status-item self)
                              "button"))
         (frame (objc:invoke button "frame")))

    (objc:invoke menu
                 "popUpMenuPositioningItem:atLocation:inView:"
                 nil
                 (make-array 2 :initial-contents (list 0 (* 1.2 (elt frame 3))))
                 button)))


(defmethod initialize-instance :after ((item barista/classes:status-item) &rest initargs)
  (declare (ignorable initargs))
  (let ((status-item (objc:invoke (get-status-bar item)
                                  "statusItemWithLength:"
                                  NSVariableStatusItemLength)))
    (objc:retain status-item)
    
    (objc:invoke status-item "setHighlightMode:"
                 1)
    
    (objc:invoke status-item "setTitle:"
                 (get-title item))
    
    (let ((button (objc:invoke status-item "button")))
      (objc:invoke button  "setToolTip:"
                   (format nil "Barista plugin \"~A\""
                           (symbol-name (type-of *plugin*)))))

    (objc:invoke status-item "setAction:"
                 (objc:coerce-to-selector "onBaristaItemClick"))
    (objc:invoke status-item "setTarget:"
                 (objc:objc-object-pointer item))
    
    (setf (slot-value item
                      'barista/classes::status-item)
          status-item))
  (log:info "Status item initialized")
  (values))


(defgeneric hide (item)
  (:method ((item barista/classes:status-item))
    ;; This setView is needed to correctly refresh a status bar after
    ;; the menu item will be hidden:
    ;; https://stackoverflow.com/questions/23066802/how-to-remove-statusbaritem-from-code

    (objc:invoke (get-status-item item) "setView:"
                 nil)
    (objc:invoke (get-status-bar item) "removeStatusItem:"
                 (get-status-item item))
    (objc:release (get-status-item item))))


(defun %add-menu-item (menu title &key callback submenu url)
  (let* ((item (make-instance 'barista/classes::menu-item))
         (item-pointer (objc:objc-object-pointer item)))
    (objc:invoke item-pointer "initWithTitle:action:keyEquivalent:"
                 title
                 (objc:coerce-to-selector "theCallback")
                 "")
    ;; If title has a special color or font, then we have to
    ;; put it into a separate attribute. Otherwise menu will not be displayed.
    ;; Why, Apple!? Why?
    ;;
    ;; NOTE: I didn't found how to check objective-c object's type
    ;; in LispWorks. Previously we checked for:
    ;; (typep title ns:ns-attributed-string)
    (when (barista/classes:objc-typep title "NSAttributedString")
      (objc:invoke item "attributedTitle:" title))
    
    (when (and callback url)
      (error "Callback and URL can't be specified simultaneously."))

    (when url
      (setf callback
            (lambda (&rest rest)
              (declare (ignorable rest))
              (open-url url))))
    
    (when callback
      (setf (get-callback item)
            ;; To restore a plugin binding, we need to catch it into a closure
            (funcall
             (lambda (plugin)
               (lambda (&rest args)
                 (let ((*plugin* plugin))
                   (apply callback args))))
             *plugin*)))
    
    (when submenu
      (let ((submenu (make-menu submenu)))
        ;; NOTE: in CCL setf was here:
        (objc:invoke item-pointer "setSubmenu:" submenu)))
    
    (objc:invoke item-pointer
                 "setTarget:" item-pointer)
    (objc:invoke menu "addItem:" item-pointer)
    (values item)))


(defun %make-menu-item-call (menu-var data)
  (destructuring-bind (title &key callback submenu url)
      (uiop:ensure-list data)
    `(%add-menu-item ,menu-var
                     ,(get-string-form-for-macro title)
                     :callback ,callback
                     :submenu ',submenu
                     :url ,url)))


(defmacro defmenu (name (&rest items))
  (let* ((func-name (intern (format nil "MAKE-~A-MENU" (string-upcase name))))
         (menu-var (gensym "MENU"))
         (item-calls (loop for item in items
                           collect (%make-menu-item-call menu-var item))))
    `(progn
       (defun ,func-name ()
         (let ((,menu-var (objc:alloc-init-object "NSMenu")))
           (log:info "Creating menu" ',name)
           ,@item-calls
           (values ,menu-var)))
       (setf (getf *menu-constructors* ',name)
             #',func-name))))


(defun add-item (title &key callback submenu url)
  (declare (ignore title callback submenu url))
  (error "This function should be called inside BUILD-MENU macro."))


(defmacro build-menu (&body body)
  (let* ((menu-var (gensym "MENU")))
    `(let ((,menu-var (objc:alloc-init-object "NSMenu")))
       (flet ((add-item (title &key callback submenu url)
                (%add-menu-item ,menu-var title
                                :callback callback
                                :submenu submenu
                                :url url)))
         ,@body)
       (values ,menu-var))))


(defun make-menu (name-or-menu)
  (cond
   ((typep name-or-menu 'symbol)
    (let* ((name name-or-menu)
           (constructor (or (getf *menu-constructors* name)
                            (symbol-function name))))
      (unless constructor
        (error "Unable to find menu constructor for ~A" name))
      (funcall constructor)))
   ((barista/classes:objc-typep name-or-menu "NSMenu") name-or-menu)
   (t (error "Object ~A has unsupported type. It should be a symbol or NSMenu."))))
