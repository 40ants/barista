(defpackage #:barista/classes
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cffi)
  (:import-from #:cl-colors)
  (:import-from #:alexandria)
  (:import-from #:barista/objc
                #:send
                #:%cls
                #:ns-str
                #:alloc-init
                #:call-on-main-thread)
  (:import-from #:barista/vars
                #:+supported-colors+
                #:+default-font-size+)
   (:export
    #:get-menu-thunk
    #:get-ns-status-item
    #:get-button-address
    #:get-title
    #:get-image
    #:system-item-p
    #:status-item
    #:make-attributed-string
    #:make-ns-image
    #:make-image-file
    #:image-file
    #:image-file-p
    #:image-file-path
    #:image-file-pixel-width
    #:image-file-pixel-height
    #:make-font
    #:make-default-font
    #:join-attributed-string
    #:get-string-form-for-macro))
(in-package #:barista/classes)


;;; ---- NSString attribute name constants -----------------------------------
;;;
;;; These are NSString globals exported from AppKit.framework.
;;; We load their pointer values lazily from the foreign symbol table.

(defvar *ns-font-attribute-name* nil)
(defvar *ns-foreground-color-attribute-name* nil)

(defun ns-font-attribute-name ()
  "Return the NSString constant NSFontAttributeName."
  (or *ns-font-attribute-name*
      (setf *ns-font-attribute-name*
            (cffi:mem-ref
             (cffi:foreign-symbol-pointer "NSFontAttributeName")
             :pointer))))

(defun ns-foreground-color-attribute-name ()
  "Return the NSString constant NSForegroundColorAttributeName."
  (or *ns-foreground-color-attribute-name*
      (setf *ns-foreground-color-attribute-name*
            (cffi:mem-ref
             (cffi:foreign-symbol-pointer "NSForegroundColorAttributeName")
             :pointer))))


;;; ---- status-item CLOS class ----------------------------------------------
;;;
;;; Pure CLOS wrapper around an NSStatusItem CFFI pointer.
;;; No LispWorks ObjC bridge needed.

(defclass status-item ()
  ((ns-status-item :initform nil
                   :accessor get-ns-status-item
                   :documentation "Raw CFFI pointer to the AppKit NSStatusItem.")
   (button-address :initform nil
                   :accessor get-button-address
                   :documentation "Integer pointer address of the NSStatusBarButton,
   captured at initialisation time and used for *click-table* keying and cleanup.
   Storing it avoids calling (send ns-item \"button\") on a potentially-released
   object during hide.")
   (title :initform "Unknown"
          :initarg :title
          :accessor %get-title
          :documentation "Current menu-bar label string or NSAttributedString pointer.")
   (menu-thunk :initform nil
               :initarg :menu-thunk
               :accessor get-menu-thunk
               :documentation "Nullary function that builds and returns an NSMenu pointer.")
   (system-item-p :initform nil
                  :initarg :system-item-p
                  :accessor system-item-p
                  :documentation "When T, this is the system plugin item.
  The click handler skips appending the Settings/Quit section to its menu
  because it already is the Settings menu."))
  (:documentation "Wraps an NSStatusItem for one Barista plugin."))

(defgeneric get-title (item)
  (:method ((item status-item))
    (%get-title item)))

(defgeneric (setf get-title) (value item)
  (:method (value (item status-item))
    (call-on-main-thread
     (lambda ()
       ;; Re-read the ns-item pointer here, on the main thread, at execution
       ;; time.  We must NOT capture it in the closure: a worker thread may
       ;; schedule this lambda and, before the main thread runs it, the plugin
       ;; can be stopped by hide (which releases the NSStatusItem and sets the
       ;; slot to NIL).  Re-reading the now-NIL slot lets us skip the send and
       ;; avoid a use-after-free that would otherwise corrupt another plugin's
       ;; icon when the allocator reuses the freed pointer's address.
       (let ((ns-item (get-ns-status-item item)))
         (when ns-item
           (cond
             ;; NSAttributedString pointer -- use setAttributedTitle:
             ((and (cffi:pointerp value)
                   (not (cffi:null-pointer-p value)))
              (send ns-item "setAttributedTitle:" :pointer value :void))
             ;; plain string
             ((stringp value)
              (send ns-item "setTitle:" :pointer (ns-str value) :void))
             (t
              (log:warn "Unsupported title type" value)))))))
    (setf (%get-title item) value)))


;;; ---- image-file -----------------------------------------------------------

(defclass image-file ()
  ((path :initarg :path :reader image-file-path
         :documentation "Pathname of the PNG file on disk.")
   (pixel-width :initarg :pixel-width :reader image-file-pixel-width
                :documentation "Actual pixel width of the image data.")
   (pixel-height :initarg :pixel-height :reader image-file-pixel-height
                 :documentation "Actual pixel height of the image data."))
  (:documentation "An image backed by an on-disk file, with known pixel dimensions.
Used so that (setf get-image) can compute the correct logical (point) size
from the screen backing scale factor, producing crisp Retina rendering."))

(defun make-image-file (path pixel-width pixel-height)
  "Create an IMAGE-FILE from PATH with the given PIXEL-WIDTH and PIXEL-HEIGHT."
  (make-instance 'image-file
                 :path path
                 :pixel-width pixel-width
                 :pixel-height pixel-height))

(defun image-file-p (x)
  "Return T when X is an IMAGE-FILE instance."
  (typep x 'image-file))


;;; ---- NSImage helpers -----------------------------------------------------

(defun make-ns-image (path &key (size nil) (point-size nil) (template nil))
  "Load an NSImage from PATH (pathname or string).
PATH may be a CL pathname or a namestring.

Keyword arguments:
  SIZE       -- when non-NIL, a number; calls setSize: with SIZE x SIZE points.
  POINT-SIZE -- when non-NIL, a cons (WIDTH . HEIGHT) in points.  The image
                pixel data may be at a higher resolution (e.g. 2x for Retina);
                setSize: is called so AppKit treats it as the given point size.
  TEMPLATE   -- when T, marks the image as a template image so macOS adapts
                it to the current appearance (dark/light mode).  Use T only
                for monochrome/symbolic icons.  For colour images leave NIL
                (the default) so pixels render as-is.

Returns the NSImage pointer, or NIL if the file could not be loaded."
  (let* ((path-str (if (pathnamep path) (namestring path) path))
         (image    (send (send (%cls "NSImage") "alloc" :pointer)
                         "initWithContentsOfFile:"
                         :pointer (ns-str path-str) :pointer)))
    (when (and image (not (cffi:null-pointer-p image)))
      (cond
        (size
         (send image "setSize:"
               :double (float size 1.0d0)
               :double (float size 1.0d0)
               :void))
        (point-size
         (send image "setSize:"
               :double (float (car point-size) 1.0d0)
               :double (float (cdr point-size) 1.0d0)
               :void)))
      (send image "setTemplate:" :bool (if template t nil) :void)
      image)))

(defun backing-scale-factor ()
  "Return the backing scale factor of the main screen (1.0, 2.0, or 3.0)."
  (let ((screen (send (%cls "NSScreen") "mainScreen" :pointer)))
    (if (cffi:null-pointer-p screen)
        1.0d0
        (send screen "backingScaleFactor" :double))))

(defgeneric get-image (item)
  (:documentation "Return the current NSImage pointer set on ITEM, or NIL."))

(defgeneric (setf get-image) (value item)
  (:documentation "Set an NSImage on ITEM's NSStatusItem.
VALUE may be:
  - an IMAGE-FILE instance — loaded with correct Retina point size
  - an NSImage CFFI pointer (from make-ns-image)
  - a CL pathname or namestring  (loaded automatically, template=NIL)
  - NIL to clear the image"))

(defmethod get-image ((item status-item))
  nil)

(defmethod (setf get-image) (value (item status-item))
  (call-on-main-thread
   (lambda ()
     (let ((ns-item (get-ns-status-item item)))
       (when ns-item
         (let ((image (cond
                         ((image-file-p value)
                          (let ((scale (backing-scale-factor))
                                (pw    (image-file-pixel-width value))
                                (ph    (image-file-pixel-height value)))
                           (make-ns-image (image-file-path value)
                                          :point-size (cons (/ pw scale)
                                                            (/ ph scale)))))
                        ((and (cffi:pointerp value)
                              (not (cffi:null-pointer-p value)))
                         value)
                        ((or (pathnamep value) (stringp value))
                         (make-ns-image value))
                        ((null value) (cffi:null-pointer))
                        (t (error "Unsupported image value: ~S" value)))))
           (send ns-item "setImage:" :pointer image :void))))))
  value)


;;; ---- NSColor helpers -----------------------------------------------------

(defun keyword-to-color (color)
  (check-type color keyword)
  (when (member color +supported-colors+)
    (symbol-value (intern (format nil "+~A+" color)
                          :cl-colors))))

(defun rgb-color (red green blue &optional (alpha 1.0d0))
  "Create an NSColor from R/G/B/A float components (0.0--1.0)."
  (send (%cls "NSColor")
        "colorWithCalibratedRed:green:blue:alpha:"
        :double (float red 1.0d0)
        :double (float green 1.0d0)
        :double (float blue 1.0d0)
        :double (float alpha 1.0d0)
        :pointer))

(defun make-color (name)
  "Resolve NAME (string CSS color or keyword) to an NSColor pointer."
  (let ((color (etypecase name
                 (string  (cl-colors:as-rgb name))
                 (keyword (keyword-to-color name)))))
    (cond
      (color
       (rgb-color (coerce (cl-colors:rgb-red   color) 'double-float)
                  (coerce (cl-colors:rgb-green color) 'double-float)
                  (coerce (cl-colors:rgb-blue  color) 'double-float)))
      (t
       (log:warn "Color ~A is not supported. See barista/vars:+supported-colors+" name)
       (make-color :black)))))


;;; ---- NSMutableDictionary helper ------------------------------------------

(defun make-dict (&optional alist)
  "Create an NSMutableDictionary pre-populated from ALIST of (key . value) pairs."
  (loop with dict = (alloc-init "NSMutableDictionary")
        for (key . value) in alist
        do (send dict "setValue:forKey:" :pointer value :pointer key :void)
        finally (return dict)))


;;; ---- NSFont helpers ------------------------------------------------------

(defun make-font (name &key (size +default-font-size+))
  "Return an NSFont for NAME at SIZE points, or NIL if the font is unknown."
  (check-type name string)
  (check-type size (or float integer))
  (let ((result (send (%cls "NSFont")
                      "fontWithName:size:"
                      :pointer (ns-str name)
                      :double  (float size 1.0d0)
                      :pointer)))
    (cond
      ((or (null result) (cffi:null-pointer-p result))
       (log:warn "Unknown font ~A" name)
       nil)
      (t result))))

(defun make-default-font (size)
  "Return the standard menu font at SIZE points."
  (check-type size (or float integer))
  (send (%cls "NSFont") "menuFontOfSize:" :double (float size 1.0d0) :pointer))


;;; ---- NSAttributedString helpers ------------------------------------------

(defun make-attributed-string (text &key font color (size +default-font-size+))
  "Create an NSAttributedString from TEXT with optional FONT, COLOR, and SIZE."
  (let* ((resolved-font (cond
                          (font (make-font font :size size))
                          (size (make-default-font size))))
         (attributes
           (make-dict
            (append
             (when resolved-font
               (list (cons (ns-font-attribute-name) resolved-font)))
             (when color
               (list (cons (ns-foreground-color-attribute-name) (make-color color))))))))
    (send (send (%cls "NSAttributedString") "alloc" :pointer)
          "initWithString:attributes:"
          :pointer (ns-str text)
          :pointer attributes
          :pointer)))

(defun join-attributed-string (&rest parts)
  "Concatenate PARTS into a single NSMutableAttributedString.
  Each part may be a plain string, a (text :color ... :font ... :size ...) list,
  or an existing NSAttributedString pointer."
  (flet ((coerce-part (s)
           (cond
             ((stringp s)
              (make-attributed-string s))
             ((consp s)
              (apply #'make-attributed-string s))
             ((and (cffi:pointerp s) (not (cffi:null-pointer-p s)))
              s)
             (t
              (error "join-attributed-string: unsupported part type ~S" s)))))
    (loop with result = (alloc-init "NSMutableAttributedString")
          for part in parts
          do (send result
                   "appendAttributedString:"
                   :pointer (coerce-part part)
                   :void)
          finally (return result))))


;;; ---- macro helper --------------------------------------------------------

(defun is-attributed-string-definition (form)
  "True when FORM is a list with :color, :font, or :size keywords."
  (check-type form cons)
  (or (member :color form)
      (member :font  form)
      (member :size  form)))

(defun get-string-form-for-macro (string)
  "Return a compile-time form that evaluates to an NSString or NSAttributedString.
  Called by defmenu/build-menu macros to process user-supplied title values."
  (etypecase string
    (string string)
    (symbol string)
    (cons
     (cond
       ((is-attributed-string-definition string)
        `(barista/classes:make-attributed-string ,@string))
       ;; list of parts -- join them
       (t
        `(barista/classes:join-attributed-string
          ,@(mapcar #'get-string-form-for-macro string)))))))
