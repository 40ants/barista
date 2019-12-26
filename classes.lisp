(defpackage #:barista/classes
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:babel)
  (:export
   #:get-menu
   #:get-status-item
   #:get-status-bar
   #:get-callback
   #:status-item
   #:get-title
   #:objc-string))
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
               (objc-string value))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; String ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ns-lisp-unicode-string (ns:ns-string)
  ((lisp-string :initarg :string
                :reader get-string)
   (utf16-points :initform nil
                 :reader get-utf16-points))
  (:metaclass ns:+ns-object))


(defun string-to-utf16 (string)
  (loop with octets = (babel:string-to-octets string
                                              :encoding :utf-16
                                              :use-bom nil)
        with need-yield = nil
        with value = 0
        for item across octets
        if need-yield
          do (setf (ldb (byte 8 8)
                        value)
                   item
                   need-yield nil)
          and
            collect value
        else
          do (setf value item
                   need-yield t)))

(defmethod initialize-instance :after ((s ns-lisp-unicode-string) &rest initargs)
  (declare (ignorable initargs))
  (setf (slot-value s 'utf16-points)
        (string-to-utf16 (get-string s))))


(objc:defmethod (#/length :<NSUI>nteger) ((self ns-lisp-unicode-string))
  (length (get-utf16-points self)))


(objc:defmethod (#/characterAtIndex: :unichar) ((self ns-lisp-unicode-string) (index :<NSUI>nteger))
  (elt (get-utf16-points self) index))


(defun objc-string (text)
  (make-instance 'ns-lisp-unicode-string :string text))
