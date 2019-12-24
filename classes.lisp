(defpackage #:barista/classes
  (:use #:cl)
  (:import-from #:log4cl)
  (:export
   #:get-menu
   #:get-status-item
   #:get-status-bar
   #:get-callback
   #:status-item
   #:get-title))
(in-package barista/classes)

;; CCL does not allow to redefine this class
;; or to use this symbol:
;; https://lists.clozure.com/pipermail/openmcl-devel/2008-March/003839.html
;; https://lists.clozure.com/pipermail/openmcl-devel/2008-March/003840.html
(defclass status-item (ns:ns-object)
  ((status-bar :initform (#/systemStatusBar ns:ns-status-bar)
               :reader get-status-bar)
   (status-item :initform nil
                :reader get-status-item)
   (title :initform "Unknown"
          :initarg :title
          :reader get-title)
   (menu :initform nil
         :initarg :menu
         :reader get-menu))
  (:metaclass ns:+ns-object))


(defmethod (setf get-title) (value (self status-item))
  (#/setTitle: (get-status-item self)
               (make-instance 'gui:ns-lisp-string
                              :string value))
  (setf (slot-value self 'title)
        value))


(defmethod (setf get-menu) (new-menu (self status-item))
  (#/setMenu: (get-status-item self)
              new-menu)
  (setf (slot-value self 'menu)
        new-menu))


(defclass menu-item (ns:ns-menu-item)
  ((callback :initform (lambda (self)
                         (log:info "Callback was called" self))
             :accessor get-callback))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/theCallback :void) ((self menu-item))
  (funcall (get-callback self)
           self))
