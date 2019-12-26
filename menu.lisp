(defpackage #:barista/menu
  (:use #:cl)
  (:import-from #:barista/classes
                #:get-callback
                #:get-status-item
                #:get-menu
                #:get-title
                #:get-status-bar
                #:objc-string)
  (:import-from #:barista/vars
                #:*plugin*)
  (:export
   #:defmenu
   #:hide
   #:make-menu))
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


(defun %add-menu-item (menu title &key callback submenu)
  (let ((item (#/alloc barista/classes::menu-item)))
    (#/initWithTitle:action:keyEquivalent: item
                                           (objc-string title)
                                           (objc:@selector #/theCallback)
                                           #@"")
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
  (destructuring-bind (title &key callback submenu)
      (uiop:ensure-list data)
    `(%add-menu-item ,menu-var ,title
                     :callback ,callback
                     :submenu ',submenu)))


(defmacro defmenu (name (&rest items))
  (let* ((func-name (intern (format nil "MAKE-~A-MENU" (string-upcase name))))
         (menu-var (gensym "MENU"))
         (item-calls (loop for item in items
                           collect (%make-menu-item-call menu-var item))))
    `(progn
       (defun ,func-name ()
         (let ((,menu-var (#/new ns:ns-menu)))
           ,@item-calls
           (values ,menu-var)))
       (setf (getf *menu-constructors* ',name)
             #',func-name))))


(defun make-menu (name)
  (check-type name symbol)
  (let ((constructor (getf *menu-constructors* name)))
    (unless constructor
      (error "Unable to find menu constructor for ~A" name))
    (funcall constructor)))
