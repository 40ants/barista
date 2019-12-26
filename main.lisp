(defpackage #:barista/main
  (:use #:cl)
  (:import-from #:barista/menu)
  (:import-from #:barista/plugin)
  (:export
   #:load-plugins))
(in-package barista/main)


(defun load-plugins ()
  )


(defun main ()
  (format t "ARGS: ~A~%"
          (uiop:command-line-arguments)))
