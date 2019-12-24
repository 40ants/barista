(defpackage #:barista/vars
  (:use #:cl)
  (:export
   #:*plugin*
   #:*debug*))
(in-package barista/vars)


(defvar *plugin* nil
  "Current plugin.")


(defvar *debug* nil
  "If True, then debugger will be invoked on any error in the periodic threads.")

