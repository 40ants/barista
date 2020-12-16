(defpackage #:barista/menu
  (:use #:cl)
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


(defmethod initialize-instance :after ((item barista/classes:status-item) &rest initargs)
  (declare (ignorable initargs))
  (let ((status-item (objc:invoke (get-status-bar item)
                                  "statusItemWithLength:"
                                  NSVariableStatusItemLength)))
    (objc:retain status-item)
    (objc:invoke status-item "setTitle:"
                 (get-title item))
    ;; (let ((button (objc:objc-message-send status-item "button")))
    ;;   (#/setToolTip: button #@"Hello world")
    ;;   (#/setTarget: button menu)
    ;;   (#/setAction: button (objc:@selector #/menuClicked)))

    (objc:invoke status-item "setHighlightMode:" t)
    
    (let ((menu (get-menu item)))
      (objc:invoke status-item "setMenu:"
                   menu))
    
    (setf (slot-value item
                      'barista/classes::status-item)
          status-item)))


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
