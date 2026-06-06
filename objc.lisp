(defpackage #:barista/objc
  (:use #:cl)
  (:import-from #:cffi)
  (:export
   ;; framework loading
   #:load-objc-frameworks
   ;; runtime primitives
   #:%sel
   #:%cls
   #:%alloc-class
   #:%reg-class
   #:%add-method
   #:%add-protocol
   #:%get-protocol
   ;; message send
   #:send
   ;; common patterns
   #:alloc-init
   ;; NSString bridge
   #:ns-str
   #:lisp-str
   ;; geometry
   #:with-cgrect))
(in-package #:barista/objc)


;;; ---- foreign library definitions -----------------------------------------

(cffi:define-foreign-library libobjc
  (:darwin "/usr/lib/libobjc.A.dylib")
  (t (:default "objc")))

(cffi:define-foreign-library foundation
  (:darwin (:framework "Foundation")))

(cffi:define-foreign-library appkit
  (:darwin (:framework "AppKit")))

(defun load-objc-frameworks ()
  "Load the ObjC runtime, Foundation, and AppKit.
  Safe to call multiple times; already-loaded libraries are skipped."
  (cffi:load-foreign-library 'libobjc)
  (cffi:load-foreign-library 'foundation)
  (cffi:load-foreign-library 'appkit))


;;; ---- ObjC runtime C functions --------------------------------------------

(cffi:defcfun ("sel_registerName"       %sel)         :pointer
  (name :string))

(cffi:defcfun ("objc_getClass"          %cls)         :pointer
  (name :string))

(cffi:defcfun ("objc_allocateClassPair" %alloc-class) :pointer
  (superclass :pointer) (name :string) (extra-bytes :size))

(cffi:defcfun ("objc_registerClassPair" %reg-class)   :void
  (cls :pointer))

(cffi:defcfun ("class_addMethod"        %add-method)  :bool
  (cls :pointer) (name :pointer) (imp :pointer) (types :string))

(cffi:defcfun ("class_addProtocol"      %add-protocol) :bool
  (cls :pointer) (protocol :pointer))

(cffi:defcfun ("objc_getProtocol"       %get-protocol) :pointer
  (name :string))


;;; ---- message sending -----------------------------------------------------

(defmacro send (receiver selector &rest type-args-return)
  "Send Objective-C message SELECTOR to RECEIVER via objc_msgSend.

  TYPE-ARGS-RETURN is a flat list of :type value ... :return-type.
  The last element is always the return-type keyword.

  Examples:
    (send obj \"init\"                  :pointer)
    (send obj \"setTitle:\"             :pointer (ns-str s) :void)
    (send bar \"statusItemWithLength:\" :double -1.0d0 :pointer)"
  `(cffi:foreign-funcall "objc_msgSend"
                         :pointer ,receiver
                         :pointer (%sel ,selector)
                         ,@type-args-return))


;;; ---- common ObjC patterns ------------------------------------------------

(defun alloc-init (class-name)
  "Allocate and initialise a new instance of CLASS-NAME."
  (send (send (%cls class-name) "alloc" :pointer) "init" :pointer))


;;; ---- NSString <-> Lisp string --------------------------------------------

(defun ns-str (s)
  "Create an NSString from CL string S."
  (send (%cls "NSString") "stringWithUTF8String:" :string s :pointer))

(defun lisp-str (ns)
  "Convert NSString NS to a CL string (UTF-8).
  Returns NIL if NS is a null pointer."
  (when (and ns (not (cffi:null-pointer-p ns)))
    (let ((ptr (cffi:foreign-funcall "objc_msgSend"
                                     :pointer ns
                                     :pointer (%sel "UTF8String")
                                     :pointer)))
      (when (and ptr (not (cffi:null-pointer-p ptr)))
        (cffi:foreign-string-to-lisp ptr)))))


;;; ---- NSRect (CGRect) helpers ---------------------------------------------
;;; CGRect = { CGPoint origin{x,y}, CGSize size{width,height} } -- four doubles.
;;; On arm64 macOS a 32-byte HFA is passed in FP registers d0-d3 and can be
;;; expressed to CFFI as four separate :double arguments.

(cffi:defcstruct cgpoint
  (x :double)
  (y :double))

(cffi:defcstruct cgsize
  (width  :double)
  (height :double))

(cffi:defcstruct cgrect
  (origin (:struct cgpoint))
  (size   (:struct cgsize)))

(defmacro with-cgrect ((var x y w h) &body body)
  "Bind VAR to a stack-allocated CGRect and execute BODY."
  `(cffi:with-foreign-object (,var '(:struct cgrect))
     (setf (cffi:foreign-slot-value
            (cffi:foreign-slot-pointer ,var '(:struct cgrect) 'origin)
            '(:struct cgpoint) 'x) (float ,x 1.0d0)
           (cffi:foreign-slot-value
            (cffi:foreign-slot-pointer ,var '(:struct cgrect) 'origin)
            '(:struct cgpoint) 'y) (float ,y 1.0d0)
           (cffi:foreign-slot-value
            (cffi:foreign-slot-pointer ,var '(:struct cgrect) 'size)
            '(:struct cgsize) 'width) (float ,w 1.0d0)
           (cffi:foreign-slot-value
            (cffi:foreign-slot-pointer ,var '(:struct cgrect) 'size)
            '(:struct cgsize) 'height) (float ,h 1.0d0))
     ,@body))
