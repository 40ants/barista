(defpackage #:barista/classes
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:babel)
  (:import-from #:cl-colors)
  (:import-from #:alexandria)
  (:import-from #:barista/vars
                #:+supported-colors+
                #:+default-font-size+)
  (:export
   #:get-menu
   #:get-status-item
   #:get-status-bar
   #:get-callback
   #:status-item
   #:get-title
   #:objc-string
   #:make-attributed-string
   #:make-font
   #:make-default-font
   #:join-attributed-string
   #:get-string-form-for-macro))
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
  (etypecase text
    (string (make-instance 'ns-lisp-unicode-string :string text))
    (ns:ns-attributed-string text)
    (ns:ns-string text)))


;; Mutable dict

(defun make-dict (&optional alist)
  (flet ((string-to-objc (value)
           (typecase value
             (string (objc-string value))
             (t value))))
    (loop with dict = (make-instance 'ns:ns-mutable-dictionary)
          for (key . value) in alist
          do (setf (#/objectForKey: dict
                                    (string-to-objc key))
                   (string-to-objc value))
          finally (return dict))))


(defun keyword-to-color (color)
  (check-type color keyword)
  (when (member color +supported-colors+)
    (symbol-value (intern (format nil "+~A+" color)
                          :cl-colors))))

(defun make-color (name)
  (let ((color (etypecase name
                 (string (cl-colors:as-rgb name))
                 (keyword (keyword-to-color name)))))
    (cond
      (color (gui::color-values-to-nscolor (cl-colors:rgb-red color)
                                           (cl-colors:rgb-green color)
                                           (cl-colors:rgb-blue color)))
      (t (log:warn "Color is not supported" name "See the full list in barista/vars:+supported-colors+")
         (make-color :black)))))


;; Attributed string

(defun make-attributed-string (text &key font color (size +default-font-size+))
  (let* ((font (cond
                 (font (make-font font :size size))
                 (size (make-default-font size))))
         (attributes (make-dict (append (when font
                                          (list (cons #$NSFontAttributeName
                                                      font)))
                                        (when color
                                          (list (cons #$NSForegroundColorAttributeName (make-color color))))))))
    (#/initWithString:attributes: (make-instance 'ns:ns-attributed-string)
                                  (objc-string text)
                                  attributes)))

(defun join-attributed-string (&rest parts)
  (flet ((make-attributed-if-needed (s)
           (etypecase s
             (string (make-attributed-string s))
             (cons (apply 'make-attributed-string s))
             (ns:ns-attributed-string s))))
    (loop with result = (make-instance 'ns:ns-mutable-attributed-string)
          for part in parts
          do (#/appendAttributedString: result
                                        (make-attributed-if-needed part))
          finally (return result))))


(defun is-attributed-string-definition (string)
  (check-type string cons)
  (or (member :color string)
      (member :font string)
      (member :size string)))


(defun get-string-form-for-macro (string)
  (etypecase string
    (string string)
    (symbol string)
    (cons (cond
            ((is-attributed-string-definition string)
             `(barista/classes:make-attributed-string
               ,@string))
            ;; Or it can be a list of string. In this case we need
            ;; to join them
            (t
             `(barista/classes:join-attributed-string
               ,@(mapcar 'get-string-form-for-macro string)))))))


;; Font

(defun make-font (name &key (size +default-font-size+))
  (check-type name string)
  (check-type size integer)
  (let ((result (#/fontWithName:size: ns:ns-font (objc-string name) size)))
    (cond
      ((eql result ccl:+null-ptr+)
       (log:warn "Unknown font" name)
       (values))
      (t result))))


(defun make-default-font (size)
  (check-type size integer)
  (#/menuFontOfSize: ns:ns-font size))
