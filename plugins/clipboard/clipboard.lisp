(defpackage #:barista-plugins/clipboard/clipboard
  (:use #:cl)
  (:import-from #:barista/objc
                #:send
                #:%cls
                #:ns-str
                #:lisp-str)
  (:import-from #:barista/menu
                #:build-menu
                #:add-item
                #:add-separator)
  (:import-from #:barista/plugin
                #:defplugin)
  (:import-from #:barista/vars
                #:*plugin*)
  (:import-from #:f-underscore
                #:f_%))
(in-package #:barista-plugins/clipboard/clipboard)


;;; ---- NSPasteboard helpers ------------------------------------------------

(defun general-pasteboard ()
  "Return the NSPasteboard generalPasteboard singleton."
  (send (%cls "NSPasteboard") "generalPasteboard" :pointer))

(defun pasteboard-change-count ()
  "Return the current changeCount of the general pasteboard (integer)."
  (send (general-pasteboard) "changeCount" :long))

(defun pasteboard-string ()
  "Return the current string content of the general pasteboard, or NIL."
  (let* ((pb      (general-pasteboard))
         (type    (cffi:mem-ref
                   (cffi:foreign-symbol-pointer "NSPasteboardTypeString")
                   :pointer))
         (ns-str  (send pb "stringForType:" :pointer type :pointer)))
    (when (and ns-str (not (cffi:null-pointer-p ns-str)))
      (lisp-str ns-str))))

(defun pasteboard-set-string (s)
  "Replace the general pasteboard contents with string S."
  (let ((pb   (general-pasteboard))
        (type (cffi:mem-ref
               (cffi:foreign-symbol-pointer "NSPasteboardTypeString")
               :pointer)))
    (send pb "clearContents" :long)
    (send pb "setString:forType:"
          :pointer (ns-str s)
          :pointer type
          :bool)))


;;; ---- history helpers -----------------------------------------------------

(defconstant +max-history+ 10
  "Maximum number of clipboard entries to remember.")

(defun truncate-for-display (s &optional (max-len 60))
  "Return S truncated to MAX-LEN characters with … appended if needed."
  (if (> (length s) max-len)
      (concatenate 'string (subseq s 0 max-len) "…")
      s))

(defun add-to-history (history item)
  "Return a new history list with ITEM at the front.
  Removes any existing duplicate and trims to +max-history+."
  (let ((deduped (remove item history :test #'string=)))
    (subseq (cons item deduped)
            0 (min +max-history+ (1+ (length deduped))))))


;;; ---- copy-to-clipboard callback ------------------------------------------

(defun make-copy-callback (text plugin)
  "Return a thunk that writes TEXT back to the clipboard and moves it to
  the top of PLUGIN's history."
  (f_% (pasteboard-set-string text)
       (setf (get-history plugin)
             (add-to-history (get-history plugin) text))))


;;; ---- menu builder --------------------------------------------------------

(defun build-clipboard-menu (plugin)
  "Build the clipboard history NSMenu from PLUGIN's current history."
  (let ((history (get-history plugin)))
    (build-menu
      (if history
          (progn
            (dolist (item history)
              (add-item (truncate-for-display item)
                        :callback (make-copy-callback item plugin)))
            (add-separator)
            (add-item "Clear history"
                      :callback (f_% (setf (get-history plugin) nil))))
          (add-item "(empty)" :callback nil)))))


;;; ---- plugin ---------------------------------------------------------------

(defplugin clipboard
    ((history    :initform nil :accessor get-history)
     (last-count :initform -1  :accessor get-last-count)
     (menu-ready :initform nil :accessor get-menu-ready))
  (:title "📋")
  (:every :second
          ;; Wire up dynamic menu on first tick (same pattern as system-monitor).
          (unless (get-menu-ready *plugin*)
            (setf (barista/classes:get-menu-thunk
                   (barista/plugin:get-status-item *plugin*))
                  (let ((p *plugin*))
                    (lambda () (build-clipboard-menu p))))
            (setf (get-menu-ready *plugin*) t))
          ;; Poll the pasteboard changeCount; only read string on change.
          (let ((count (pasteboard-change-count)))
            (unless (= count (get-last-count *plugin*))
              (setf (get-last-count *plugin*) count)
              (let ((text (pasteboard-string)))
                (when (and text (not (string= text "")))
                  (setf (get-history *plugin*)
                        (add-to-history (get-history *plugin*) text))))))))
