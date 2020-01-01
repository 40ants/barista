(defpackage #:barista/menu
  (:use #:cl)
  (:import-from #:barista/classes
                #:get-string-form-for-macro
                #:get-callback
                #:get-status-item
                #:get-menu
                #:get-title
                #:get-status-bar
                #:objc-string)
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


(defmethod initialize-instance :after ((item barista/classes:status-item) &rest initargs)
  (declare (ignorable initargs))
  (let ((status-item (#/statusItemWithLength: (get-status-bar item)
                                              #$NSVariableStatusItemLength)))
    (#/retain status-item)
    (#/setTitle: status-item
                 (objc-string (get-title item)))
    ;; (let ((button (objc:objc-message-send status-item "button")))
    ;;   (#/setToolTip: button #@"Hello world")
    ;;   (#/setTarget: button menu)
    ;;   (#/setAction: button (objc:@selector #/menuClicked)))

    (#/setHighlightMode: status-item t)
    
    (let ((menu (get-menu item)))
      ;; TODO: replace with setf
      (#/setMenu: status-item
                  menu))
    
    (setf (slot-value item
                      'barista/classes::status-item)
          status-item)))


;; TODO: may be remove, seems i dont need this method in API
(defun add-menu-item (menu title &key callback)
  (let ((item (#/alloc barista/classes::menu-item)))
    (#/initWithTitle:action:keyEquivalent: item
                                           (objc-string title)
                                           (objc:@selector #/theCallback)
                                           #@"")
    (when callback
      (setf (get-callback item)
            callback))
    (#/setTarget: item item)
    (#/addItem: (get-menu menu) item)
    (values item)))


(defgeneric hide (item)
  (:method ((item barista/classes:status-item))
    ;; This setView is needed to correctly refresh a status bar after
    ;; the menu item will be hidden:
    ;; https://stackoverflow.com/questions/23066802/how-to-remove-statusbaritem-from-code
    (#/setView: (get-status-item item) nil)
    (#/removeStatusItem: (get-status-bar item)
                         (get-status-item item))
    (#/release (get-status-item item))))


(defun %add-menu-item (menu title &key callback submenu url)
  (let ((item (#/alloc barista/classes::menu-item))
        (title (objc-string title)))
    (#/initWithTitle:action:keyEquivalent: item
                                           title
                                           (objc:@selector #/theCallback)
                                           #@"")
    ;; If title has a special color or font, then we have to
    ;; put it into a separate attribute. Otherwise menu will not be displayed.
    ;; Why, Apple!? Why?
    (when (typep title ns:ns-attributed-string)
      (setf (#/attributedTitle item)
            title))
    
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
        (setf (#/submenu item)
              submenu)))
    
    (#/setTarget: item item)
    (#/addItem: menu item)
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
         (let ((,menu-var (#/new ns:ns-menu)))
           (log:info "Creating menu" ',name)
           ,@item-calls
           (values ,menu-var)))
       (setf (getf *menu-constructors* ',name)
             #',func-name))))


(defmacro build-menu (&body body)
  (let* ((menu-var (gensym "MENU")))
    `(let ((,menu-var (#/new ns:ns-menu)))
       (flet ((add-item (title &key callback submenu url)
                (%add-menu-item ,menu-var title
                                :callback callback
                                :submenu submenu
                                :url url)))
         ,@body)
       (values ,menu-var))))


(defun make-menu (name)
  (check-type name symbol)
  (let ((constructor (getf *menu-constructors* name)))
    (unless constructor
      (error "Unable to find menu constructor for ~A" name))
    (funcall constructor)))
