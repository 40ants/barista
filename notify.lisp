(defpackage #:barista/notify
  (:use #:cl)
  (:import-from #:cl-fad
                #:list-directory)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:fmt
                #:fmt)
  (:import-from #:bordeaux-threads)
  (:export #:notify
           #:get-available-sounds))
(in-package #:barista/notify)


(defcached get-available-sounds ()
  (loop with files = (append (list-directory "/System/Library/Sounds")
                             (list-directory "~/Library/Sounds"))
        for filename in files
        for name = (pathname-name filename)
        collect (make-keyword (string-upcase name))))


(defun notify (message &key (title "Barista") sound)
  (when sound
    (unless (member (make-keyword (string-upcase sound))
                    (get-available-sounds))
      (error "Sound ~A not found. Supported sounds are: ~{~A~^, ~}"
             sound
             (get-available-sounds))))
  
  (let ((command (fmt nil
                      "osascript -e 'display notification "
                      (:s message)
                      " with title "
                      (:s title)
                      (:when sound
                        " sound name "
                        (:s (symbol-name sound) ))
                      "'")))
    (bordeaux-threads:make-thread
     (lambda ()
       (uiop:run-program command))
     :name command))
  (values))
