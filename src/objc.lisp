(defpackage #:barista/objc
  (:use #:cl)
  (:import-from #:cffi)
  (:import-from #:bordeaux-threads)
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
   ;; main-thread dispatch via GCD
   #:call-on-main-thread
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

(cffi:define-foreign-library libdispatch
  (:darwin "/usr/lib/system/libdispatch.dylib")
  (t (:default "dispatch")))

(cffi:define-foreign-library foundation
  (:darwin (:framework "Foundation")))

(cffi:define-foreign-library appkit
  (:darwin (:framework "AppKit")))

(defun load-objc-frameworks ()
  "Load the ObjC runtime, libdispatch, Foundation, and AppKit.
  Safe to call multiple times; already-loaded libraries are skipped."
  (cffi:load-foreign-library 'libobjc)
  (cffi:load-foreign-library 'libdispatch)
  (cffi:load-foreign-library 'foundation)
  (cffi:load-foreign-library 'appkit))


;;; ---- ObjC runtime C functions --------------------------------------------

(cffi:defcfun ("sel_registerName"       %sel)         :pointer
  "Register a selector name with the ObjC runtime and return its pointer."
  (name :string))

(cffi:defcfun ("objc_getClass"          %cls)         :pointer
  "Look up an ObjC class by name and return its pointer."
  (name :string))

(cffi:defcfun ("objc_allocateClassPair" %alloc-class) :pointer
  "Allocate a new ObjC class pair subclassing SUPERCLASS."
  (superclass :pointer) (name :string) (extra-bytes :size))

(cffi:defcfun ("objc_registerClassPair" %reg-class)   :void
  "Register an allocated ObjC class pair with the runtime."
  (cls :pointer))

(cffi:defcfun ("class_addMethod"        %add-method)  :bool
  "Add a method (IMP) to an ObjC class."
  (cls :pointer) (name :pointer) (imp :pointer) (types :string))

(cffi:defcfun ("class_addProtocol"      %add-protocol) :bool
  "Adopt a protocol on an ObjC class."
  (cls :pointer) (protocol :pointer))

(cffi:defcfun ("objc_getProtocol"       %get-protocol) :pointer
  "Look up an ObjC protocol by name and return its pointer."
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


;;; ---- Grand Central Dispatch: main-thread delivery -----------------------
;;;
;;; trivial-main-thread's task-queue approach does not work when the main
;;; thread is blocked inside [NSApp run].  Instead we use GCD's main queue,
;;; which AppKit drains on every run-loop iteration.
;;;
;;; dispatch_get_main_queue() in C headers is a macro that expands to
;;; &_dispatch_main_q -- a global variable exported from libdispatch.
;;; We bind it as a foreign variable and take its address.
;;;
;;; dispatch_async_f takes a plain C function + void* context, avoiding
;;; the need to construct ObjC block structs.

(defun %dispatch-get-main-queue ()
  "Return a pointer to the GCD main queue.
  dispatch_get_main_queue() in C is a macro for &_dispatch_main_q."
  (cffi:foreign-symbol-pointer "_dispatch_main_q"))

(cffi:defcfun ("dispatch_async_f" %dispatch-async-f) :void
  (queue    :pointer)
  (context  :pointer)
  (work     :pointer))

;;; We keep a global table of pending thunks so the GC cannot collect them
;;; before the main thread runs them.
(defvar *dispatch-thunks* (make-hash-table)
  "Maps integer key -> CL thunk, keeping thunks alive until GCD fires them.")
(defvar *dispatch-thunk-lock* (bordeaux-threads:make-lock "dispatch-thunk-lock"))
(defvar *dispatch-thunk-counter* 0)

(cffi:defcallback %dispatch-invoke :void ((ctx :pointer))
  "C function passed to dispatch_async_f; ctx encodes the thunk table key."
  (let* ((key (cffi:pointer-address ctx))
         (fn  (bordeaux-threads:with-lock-held (*dispatch-thunk-lock*)
                (prog1 (gethash key *dispatch-thunks*)
                  (remhash key *dispatch-thunks*)))))
    (when fn
      (handler-case (funcall fn)
        (error (e)
          (format *error-output* "~&[barista] dispatch error: ~a~%" e))))))

(defun call-on-main-thread (thunk)
  "Schedule THUNK to run on the AppKit main thread via GCD.
  Returns immediately; THUNK runs asynchronously on the next run-loop turn.
  Safe to call from any thread, including the main thread itself."
  (let* ((key (bordeaux-threads:with-lock-held (*dispatch-thunk-lock*)
                (let ((k (incf *dispatch-thunk-counter*)))
                  (setf (gethash k *dispatch-thunks*) thunk)
                  k)))
         ;; Encode the key as a pointer-sized integer context value.
         ;; We never dereference this pointer; it is only used as a unique key.
         (ctx (cffi:make-pointer key)))
    (%dispatch-async-f (%dispatch-get-main-queue)
                       ctx
                       (cffi:callback %dispatch-invoke))))


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
