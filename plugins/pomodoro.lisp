(defpackage #:barista-plugins/pomodoro
  (:use #:cl)
  (:import-from #:local-time
                #:adjust-timestamp
                #:now)
  (:import-from #:local-time-duration
                #:duration-as
                #:timestamp-difference)
  (:import-from #:barista/menu
                #:defmenu)
  (:import-from #:barista/plugin
                #:replace-menu
                #:get-title
                #:defplugin)
  (:import-from #:barista/vars
                #:*plugin*)
  (:import-from #:fmt
                #:fmt)
  (:import-from #:barista/notify
                #:notify))
(in-package barista-plugins/pomodoro)


(defparameter *stopped-symbol* "❌")
(defparameter *started-symbol* "✅")
(defparameter *in-progress-symbol* "♽")
(defparameter *interval* (* 60 15))


(defun start (self)
  (declare (ignorable self))
  (setf (get-state *plugin*) :started
        (get-count-to *plugin*) (adjust-timestamp (now)
                                  (offset :sec *interval*)))
  (update)
  (barista/plugin:replace-menu start stop)
  (notify "Pomodoro starting" :title "Pomodoro" :sound :hero))


(defun stop (self)
  (declare (ignorable self))
  (setf (get-title *plugin*) *stopped-symbol*
        (get-state *plugin*) :stopped)
  (replace-menu stop start)
  (notify "Pomodoro stopped" :title "Pomodoro" :sound :glass))


(defun update ()
  (case (get-state *plugin*)
    (:started
     (let* ((difference (timestamp-difference
                         (get-count-to *plugin*)
                         (now)))
            (seconds (duration-as difference :sec))
            (new-title (fmt nil *in-progress-symbol* " "
                            (:duration difference))))
       (if (> seconds 0)
           (setf (get-title *plugin*)
                 new-title)
           (stop *plugin*))))))


(defmenu start
    (("Start"
      :callback 'start)))


(defmenu stop
    (("Stop"
      :callback 'stop)))


(defplugin pomodoro
    ((count-to :initform nil
               :accessor get-count-to)
     (state :initform :disabled
            :accessor get-state))
  (:title *stopped-symbol*)
  (:menu start)
  (:every :second
          (update)))
