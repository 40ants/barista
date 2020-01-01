(defpackage #:barista/vars
  (:use #:cl)
  (:import-from #:cl-colors)
  (:import-from #:alexandria)
  (:export
   #:*plugin*
   #:*debug*
   #:+default-font-size+
   #:+supported-colors+))
(in-package barista/vars)


(defvar *plugin* nil
  "Current plugin.")


(defvar *debug* nil
  "If True, then debugger will be invoked on any error in the periodic threads.")

(defvar +default-font-size+ 14)

(defparameter +supported-colors+
  (sort
   (uiop:while-collecting (collect)
     (do-external-symbols (s :cl-colors)
       (when (and (boundp s)
                  (typep (symbol-value s)
                         'cl-colors:rgb))
         (collect (alexandria:make-keyword (string-trim (list #\+) (symbol-name s)))))))
   #'string<)
  "A list of supported color names.")
