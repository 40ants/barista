(defpackage #:barista-plugins/system-monitor/metrics
  (:use #:cl)
  (:import-from #:cffi)
  (:export
   #:cpu-ratio
   #:memory-ratio
   #:memory-used-bytes
   #:memory-total-bytes
   #:gpu-ratio))
(in-package #:barista-plugins/system-monitor/metrics)


;;; ---- IOKit foreign library -----------------------------------------------

(cffi:define-foreign-library iokit
  (:darwin (:framework "IOKit")))

(defun ensure-iokit ()
  "Load IOKit.framework if not already loaded."
  (handler-case (cffi:load-foreign-library 'iokit)
    (error () nil)))


;;; ---- CPU — host_statistics / HOST_CPU_LOAD_INFO --------------------------
;;;
;;; host_statistics(mach_host_self(), HOST_CPU_LOAD_INFO=3, &info, &count)
;;; Returns four uint32 tick counters: user / sys / idle / nice.
;;; CPU% is computed as delta(user+sys) / delta(total) between two calls.

(defconstant +host-cpu-load-info+       3)
(defconstant +host-cpu-load-info-count+ 4)  ; sizeof(4×uint32) / sizeof(int32)

(cffi:defcstruct cpu-load-info
  (user-ticks :uint32)
  (sys-ticks  :uint32)
  (idle-ticks :uint32)
  (nice-ticks :uint32))

(defvar *prev-cpu-ticks* nil
  "Previous (user sys idle nice) tick snapshot, or NIL on first call.")

(defun %read-cpu-ticks ()
  "Return (values user sys idle nice) absolute Mach tick counters."
  (cffi:with-foreign-objects ((info  '(:struct cpu-load-info))
                              (count :uint32))
    (setf (cffi:mem-ref count :uint32) +host-cpu-load-info-count+)
    (let ((ret (cffi:foreign-funcall "host_statistics"
                                     :uint32  (cffi:foreign-funcall "mach_host_self" :uint32)
                                     :int     +host-cpu-load-info+
                                     :pointer info
                                     :pointer count
                                     :int)))
      (when (/= 0 ret)
        (error "host_statistics failed: kern_return_t=~A" ret))
      (values
       (cffi:foreign-slot-value info '(:struct cpu-load-info) 'user-ticks)
       (cffi:foreign-slot-value info '(:struct cpu-load-info) 'sys-ticks)
       (cffi:foreign-slot-value info '(:struct cpu-load-info) 'idle-ticks)
       (cffi:foreign-slot-value info '(:struct cpu-load-info) 'nice-ticks)))))

(defun cpu-ratio ()
  "Return CPU load as a float in [0.0, 1.0] (user+sys / total).
  Uses a delta between successive calls; returns 0.0 on the first call
  (no baseline yet).  Thread-safe: reads are atomic on the same thread."
  (multiple-value-bind (user sys idle nice)
      (handler-case (%read-cpu-ticks)
        (error () (return-from cpu-ratio 0.0)))
    (let ((current (list user sys idle nice)))
      (prog1
          (if *prev-cpu-ticks*
              (destructuring-bind (pu ps p-idle pn) *prev-cpu-ticks*
                (declare (ignore pn))
                (let* ((d-user  (- user pu))
                       (d-sys   (- sys  ps))
                       (d-idle  (- idle p-idle))
                       (d-total (+ d-user d-sys d-idle)))
                  (if (zerop d-total)
                      0.0
                      (min 1.0 (/ (float (+ d-user d-sys))
                                  (float d-total))))))
              0.0)
        (setf *prev-cpu-ticks* current)))))


;;; ---- Memory — host_statistics64 / HOST_VM_INFO64 + sysctl hw.memsize ----
;;;
;;; Returns used/total bytes.
;;; used = (total - free_pages * page_size)
;;; total = sysctl("hw.memsize")

(defconstant +host-vm-info64+       4)   ; HOST_VM_INFO64
(defconstant +host-vm-info64-count+ 38)  ; HOST_VM_INFO64_COUNT (vm_statistics64)

(cffi:defcstruct vm-statistics64
  (free-count                              :uint32)
  (active-count                            :uint32)
  (inactive-count                          :uint32)
  (wire-count                              :uint32)
  (zero-fill-count                         :uint64)
  (reactivations                           :uint64)
  (page-ins                                :uint64)
  (page-outs                               :uint64)
  (faults                                  :uint64)
  (cow-faults                              :uint64)
  (lookups                                 :uint64)
  (hits                                    :uint64)
  (purges                                  :uint64)
  (purgeable-count                         :uint32)
  (speculative-count                       :uint32)
  (decompressions                          :uint64)
  (compressions                            :uint64)
  (swap-ins                                :uint64)
  (swap-outs                               :uint64)
  (compressor-page-count                   :uint32)
  (throttled-count                         :uint32)
  (external-page-count                     :uint32)
  (internal-page-count                     :uint32)
  (total-uncompressed-pages-in-compressor  :uint64))

(defun %memory-stats ()
  "Return (values free-pages page-size total-bytes) via Mach + sysctl."
  (cffi:with-foreign-objects ((stats '(:struct vm-statistics64))
                              (count :uint32))
    (setf (cffi:mem-ref count :uint32) +host-vm-info64-count+)
    (let ((ret (cffi:foreign-funcall "host_statistics64"
                                     :uint32  (cffi:foreign-funcall "mach_host_self" :uint32)
                                     :int     +host-vm-info64+
                                     :pointer stats
                                     :pointer count
                                     :int)))
      (when (/= 0 ret)
        (error "host_statistics64 failed: kern_return_t=~A" ret))
              (let ((free-pages (cffi:foreign-slot-value
                           stats '(:struct vm-statistics64) 'free-count))
              (page-size  (cffi:foreign-funcall "getpagesize" :int))
              (total      (cffi:with-foreign-objects ((val  :int64)
                                                      (size :size))
                            (setf (cffi:mem-ref size :size) 8)
                            (cffi:foreign-funcall "sysctlbyname"
                                                  :string  "hw.memsize"
                                                  :pointer val
                                                  :pointer size
                                                  :pointer (cffi:null-pointer)
                                                  :size    0
                                                  :int)
                            (cffi:mem-ref val :int64))))
          (values free-pages page-size total)))))

(defun memory-total-bytes ()
  "Return total physical RAM in bytes."
  (handler-case
      (multiple-value-bind (fp ps total) (%memory-stats)
        (declare (ignore fp ps))
        total)
    (error () 0)))

(defun memory-used-bytes ()
  "Return used physical RAM in bytes (total − free pages × page size)."
  (handler-case
      (multiple-value-bind (free-pages page-size total) (%memory-stats)
        (- total (* free-pages page-size)))
    (error () 0)))

(defun memory-ratio ()
  "Return memory pressure as a float in [0.0, 1.0] (used / total)."
  (handler-case
      (multiple-value-bind (free-pages page-size total) (%memory-stats)
        (if (zerop total)
            0.0
            (min 1.0 (/ (float (- total (* free-pages page-size)))
                        (float total)))))
    (error () 0.0)))


;;; ---- GPU — IOKit IOAccelerator -------------------------------------------
;;;
;;; IOServiceGetMatchingServices("IOAccelerator") → iterate → CFProperties
;;; → "PerformanceStatistics" dict → "Device Utilization %" (CFNumber)
;;;
;;; Works on Apple Silicon (M-series) without sudo.
;;; Returns NIL when IOKit is unavailable or the key is absent.

(defun %cf-string (s)
  "Create a CFString (toll-free bridged to NSString) from CL string S."
  (cffi:foreign-funcall "objc_msgSend"
                        :pointer (cffi:foreign-funcall "objc_getClass"
                                                       :string "NSString"
                                                       :pointer)
                        :pointer (cffi:foreign-funcall "sel_registerName"
                                                       :string "stringWithUTF8String:"
                                                       :pointer)
                        :string s
                        :pointer))

(defun %cf-dict-get (dict key-str)
  "Look up KEY-STR (a CL string) in a CFDictionaryRef DICT.
  Returns the value pointer or NIL."
  (let* ((key (cffi:foreign-funcall "objc_msgSend"
                                    :pointer (cffi:foreign-funcall "objc_getClass"
                                                                   :string "NSString"
                                                                   :pointer)
                                    :pointer (cffi:foreign-funcall "sel_registerName"
                                                                   :string "stringWithUTF8String:"
                                                                   :pointer)
                                    :string key-str
                                    :pointer))
         (val (cffi:foreign-funcall "CFDictionaryGetValue"
                                    :pointer dict
                                    :pointer key
                                    :pointer)))
    (when (and val (not (cffi:null-pointer-p val)))
      val)))

(defun %cf-number->int64 (num)
  "Extract a 64-bit integer from CFNumberRef NUM.  Returns NIL on failure."
  (cffi:with-foreign-object (out :int64)
    (let ((ok (cffi:foreign-funcall "CFNumberGetValue"
                                    :pointer num
                                    :int     4   ; kCFNumberSInt64Type
                                    :pointer out
                                    :bool)))
      (when ok
        (cffi:mem-ref out :int64)))))

(defun gpu-ratio ()
  "Return GPU utilisation as a float in [0.0, 1.0] via IOKit IOAccelerator.
  Returns 0.0 when IOKit is unavailable or the metric cannot be read."
  (ensure-iokit)
  (handler-case
      (cffi:with-foreign-object (iterator :uint32)
        (let* ((matching (cffi:foreign-funcall "IOServiceMatching"
                                               :string "IOAccelerator"
                                               :pointer))
               (ret      (cffi:foreign-funcall "IOServiceGetMatchingServices"
                                               :uint32  0  ; kIOMainPortDefault
                                               :pointer matching
                                               :pointer iterator
                                               :int)))
          (when (/= 0 ret)
            (return-from gpu-ratio 0.0))
          (let ((iter (cffi:mem-ref iterator :uint32))
                result)
            (unwind-protect
                (loop
                  (let ((service (cffi:foreign-funcall "IOIteratorNext"
                                                       :uint32 iter
                                                       :uint32)))
                    (when (zerop service) (return))
                    (unwind-protect
                        (cffi:with-foreign-object (props :pointer)
                          (when (zerop (cffi:foreign-funcall
                                        "IORegistryEntryCreateCFProperties"
                                        :uint32  service
                                        :pointer props
                                        :pointer (cffi:null-pointer)
                                        :uint32  0
                                        :int))
                            (let ((dict (cffi:mem-ref props :pointer)))
                              (unwind-protect
                                  (let ((perf (%cf-dict-get dict "PerformanceStatistics")))
                                    (when perf
                                      (let ((num (%cf-dict-get perf "Device Utilization %")))
                                        (when num
                                          (let ((v (%cf-number->int64 num)))
                                            (when v
                                              (setf result
                                                    (min 1.0 (/ (float v) 100.0)))))))))
                                (cffi:foreign-funcall "CFRelease" :pointer dict :void)))))
                      (cffi:foreign-funcall "IOObjectRelease" :uint32 service :int))
                    (when result (return))))
              (cffi:foreign-funcall "IOObjectRelease" :uint32 iter :int))
            (or result 0.0))))
    (error () 0.0)))
