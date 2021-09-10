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
   #:get-string-form-for-macro
   #:objc-classes
   #:objc-typep))
(in-package barista/classes)

;; Generic utils

(defmacro define-string-constant (lisp-name objc-name)
  `(dspec:def (define-string-constant ,lisp-name)
     (fli:define-foreign-variable (,lisp-name ,objc-name)
       :type objc:objc-object-pointer ; NSString
       :accessor :constant)))

(define-string-constant ns-font-attribute-name "NSFontAttributeName")
(define-string-constant ns-foreground-color-attribute-name "NSForegroundColorAttributeName")


(objc:define-objc-class status-item ()
  ((status-bar :initform (objc:invoke "NSStatusBar" "systemStatusBar")
               :reader get-status-bar)
   (status-item :initform nil
                :reader get-status-item)
   (title :initform "Unknown"
          :initarg :title
          :reader get-title)
   (menu :initform nil
         :initarg :menu
         :reader get-menu))
  (:objc-class-name "BaristaStatusItem"))


(defmethod (setf get-title) (value (self status-item))
  (let ((status-item (get-status-item self)))
    (when status-item
      (objc:invoke status-item "setTitle:"
                   value)))
  (setf (slot-value self 'title)
        value))


(defmethod (setf get-menu) (new-menu (self status-item))
  (setf (slot-value self 'menu)
        new-menu))


(objc:define-objc-class menu-item ()
  ((callback :initform (lambda (self)
                         (log:info "Callback was called" self))
             :accessor get-callback))
  (:objc-class-name "MenuItem")
  (:objc-superclass-name "NSMenuItem"))


(objc:define-objc-method ("theCallback" :void) ((self menu-item))
  (funcall (get-callback self)
           self))


;; Mutable dict

(defun make-dict (&optional alist)
  (loop with dict = (objc:alloc-init-object "NSMutableDictionary")
        for (key . value) in alist
        do (objc:invoke dict "setValue:forKey:"
                        value key)
        finally (return dict)))


(defun keyword-to-color (color)
  (check-type color keyword)
  (when (member color +supported-colors+)
    (symbol-value (intern (format nil "+~A+" color)
                          :cl-colors))))

(defun rgb-color (red green blue &optional (alpha 1.0))
  (objc:invoke "NSColor"
               "colorWithCalibratedRed:green:blue:alpha:"
               red
               green
               blue
               alpha))


(defun make-color (name)
  (let ((color (etypecase name
                 (string (cl-colors:as-rgb name))
                 (keyword (keyword-to-color name)))))
    (cond
     (color (rgb-color (coerce (cl-colors:rgb-red color) 'float)
                       (coerce (cl-colors:rgb-green color) 'float)
                       (coerce (cl-colors:rgb-blue color) 'float)))
     (t (log:warn "Color is not supported" name "See the full list in barista/vars:+supported-colors+")
        (make-color :black)))))


;; Attributed string

(defun make-attributed-string (text &key font color (size +default-font-size+))
  (let* ((font (cond
                 (font (make-font font :size size))
                 (size (make-default-font size))))
         (attributes (make-dict (append (when font
                                          (list (cons (ns-font-attribute-name)
                                                      font)))
                                        (when color
                                          (list (cons (ns-foreground-color-attribute-name)
                                                      (make-color color))))))))
    (objc:invoke (objc:invoke "NSAttributedString" "alloc")
                 "initWithString:attributes:"
                 text
                 attributes)))

(defun join-attributed-string (&rest parts)
  (flet ((make-attributed-if-needed (s)
           (cond
             ((typep s 'string) (make-attributed-string s))
             ((typep s 'cons) (apply 'make-attributed-string s))
             ((objc-typep s "NSAttributedString") s)
             (t (error "Unsupported type of ~A" s)))))
    (loop with result = (objc:alloc-init-object "NSMutableAttributedString")
          for part in parts
          do (objc:invoke result
                          "appendAttributedString:"
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
  (let ((result (objc:invoke "NSFont"
                             "fontWithName:size:"
                             name size)))
    (cond
     ((objc:null-objc-pointer-p result)
      (log:warn "Unknown font" name)
      (values))
     (t result))))


(defun make-default-font (size)
  (check-type size (or float integer))
  (objc:invoke "NSFont" "menuFontOfSize:"
               (coerce size 'float)))


;; Helpers

(defun objc-classes (obj)
  "Returns a list of string of all classes obj belongs to."
  (mapcar #'objc:objc-class-name
          (loop for cls = (objc:invoke obj "class") then (objc:invoke cls "superclass")
                until (objc:null-objc-pointer-p cls)
                collect cls)))


(defun objc-typep (obj class-name)
  (check-type class-name string)

  (when (and (typep obj 'fli::pointer)
             (member class-name
                (objc-classes obj)
                :test #'string-equal))
    t))
