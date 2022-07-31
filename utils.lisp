(defpackage #:barista/utils
  (:use #:cl)
  (:import-from #:fmt
                #:fmt)
  (:import-from #:local-time-duration
                #:duration-as)
  (:import-from #:bordeaux-threads)
  (:export #:format-duration
           #:open-url
           #:on-main-thread))
(in-package #:barista/utils)


(defun format-duration (duration &optional stream)
  (let* ((seconds (duration-as duration :sec))
         (minutes (floor (/ seconds 60)))
         (hours (floor (/ seconds (* 60 60))))
         (days (floor (/ seconds (* 60 60 24))))
         (seconds (mod seconds 60))
         ;; This will be used to check if some bigger part of the duration
         ;; already was printed. In this case, we also want to print lesser
         ;; parts.
         (something-printed nil))
    (values
     (fmt stream
          (:when (not (zerop days))
            days
            "d"
            " ")
          (:when (or something-printed
                     (not (zerop hours)))
            (:when (< hours 10)
              "0")
            hours
            ":"
            (:esc (setf something-printed
                        t)))
          (:when (or something-printed
                     (not (zerop minutes)))
            (:when (< minutes 10)
              "0")
            minutes
            ":"
            (:esc (setf something-printed
                        t)))
          (:when (or something-printed
                     (not (zerop seconds)))
            (:when (< seconds 10)
              "0")
            seconds
            (:esc (setf something-printed
                        t))))
     days
     hours
     minutes
     seconds)))


(fmt:define-format-operation duration
  (:keywords (:duration))
  (:format (destination clause)
           (format-duration (second clause) destination))
  (:compile (destination clause)
            `(format-duration ,(second clause) ,destination)))


(defun open-url (url)
  (let ((command (format nil "open ~A" url)))
    (bordeaux-threads:make-thread
     (lambda ()
       (uiop:run-program command))
     :name command)))


(defmacro on-main-thread (&rest actions)
  `(progn ,@actions)
;;;   `(ccl::call-in-event-process
;;;     #'(lambda ()
;;;         ,@actions))
  )
