(defpackage #:barista-plugins/system-monitor
  (:use #:cl)
  (:import-from #:barista/menu
                #:build-menu
                #:add-item)
  (:import-from #:barista/plugin
                #:defplugin
                #:get-image)
  (:import-from #:barista/classes
                #:join-attributed-string)
  (:import-from #:barista/vars
                #:*plugin*)
  (:import-from #:barista/bar-icon
                #:render-bars-icon)
  (:import-from #:barista-plugins/system-monitor/metrics
                #:cpu-ratio
                #:gpu-ratio
                #:memory-ratio
                #:memory-used-bytes
                #:memory-total-bytes)
  (:import-from #:fmt
                #:fmt))
(in-package #:barista-plugins/system-monitor)


;;; ---- temp file path -------------------------------------------------------

(defun icon-path ()
  "Return the pathname used for the rendered status-bar PNG icon."
  (merge-pathnames #p"barista-sysmon.png"
                   (uiop:temporary-directory)))


;;; ---- attributed-string row helper ----------------------------------------

(defun metric-color (ratio)
  "Return the keyword colour for RATIO: :GREEN4, :DARKORANGE, or :RED."
  (cond ((< ratio 0.5) :GREEN4)
        ((< ratio 0.8) :DARKORANGE)
        (t             :RED)))

(defun bytes->gb (bytes)
  "Convert BYTES to gigabytes as a float."
  (/ (float bytes) (* 1024 1024 1024)))

(defun make-metric-row (label value-string ratio)
  "Build an NSAttributedString for one menu row.
  LABEL is a plain grey string; VALUE-STRING is coloured by RATIO."
  (join-attributed-string
   (list label :color :GRAY50)
   (list value-string :color (metric-color ratio))))

(defun make-menu-fn (plugin)
  "Return a thunk that builds the system-monitor NSMenu from PLUGIN's slots."
  (lambda ()
    (let* ((cpu  (get-cpu-ratio  plugin))
           (gpu  (get-gpu-ratio  plugin))
           (mem  (get-mem-ratio  plugin))
           (used (get-mem-used   plugin))
           (tot  (get-mem-total  plugin))
           (mem-str (format nil "~,1f / ~,1f GB"
                            (bytes->gb used)
                            (bytes->gb tot))))
      (build-menu
        (add-item (make-metric-row "CPU    " (fmt nil (:a (round (* 100 cpu))) "%") cpu))
        (add-item (make-metric-row "GPU    " (fmt nil (:a (round (* 100 gpu))) "%") gpu))
        (add-item (make-metric-row "Memory " mem-str mem))))))


;;; ---- plugin ---------------------------------------------------------------

(defplugin system-monitor
    ((cpu-ratio  :initform 0.0 :accessor get-cpu-ratio)
     (gpu-ratio  :initform 0.0 :accessor get-gpu-ratio)
     (mem-ratio  :initform 0.0 :accessor get-mem-ratio)
     (mem-used   :initform 0   :accessor get-mem-used)
     (mem-total  :initform 1   :accessor get-mem-total)
     (menu-ready :initform nil :accessor get-menu-ready))
  (:title "Stats")
  (:every (3 :seconds)
          ;; Wire up the dynamic menu-thunk on the first tick, after
          ;; initialize-plugin :after (from defplugin) has created the
          ;; status-item.  A second :after on the same class would silently
          ;; replace the defplugin-generated one, so we defer to here instead.
          (unless (get-menu-ready *plugin*)
            (setf (barista/classes:get-menu-thunk
                   (barista/plugin:get-status-item *plugin*))
                  (make-menu-fn *plugin*))
            (setf (get-menu-ready *plugin*) t))
          (let ((cpu (cpu-ratio))
                (gpu (gpu-ratio))
                (mem (memory-ratio)))
            (setf (get-cpu-ratio *plugin*) cpu
                  (get-gpu-ratio *plugin*) gpu
                  (get-mem-ratio *plugin*) mem
                  (get-mem-used  *plugin*) (memory-used-bytes)
                  (get-mem-total *plugin*) (memory-total-bytes))
            (setf (barista/plugin:get-title *plugin*)
                  "")
            (setf (get-image *plugin*)
                  (render-bars-icon (list cpu gpu mem) (icon-path))))))



