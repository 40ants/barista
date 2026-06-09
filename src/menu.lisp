(defpackage #:barista/menu
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cffi)
  (:import-from #:barista/objc
                #:send
                #:%sel
                #:%cls
                #:%alloc-class
                #:%reg-class
                #:%add-method
                #:alloc-init
                #:ns-str)
  (:import-from #:barista/classes
                #:get-string-form-for-macro
                #:get-ns-status-item
                #:get-menu-thunk
                #:get-title)
  (:import-from #:barista/vars
                #:*plugin*)
  (:import-from #:barista/utils
                #:open-url)
   (:export
    #:defmenu
    #:hide
    #:make-menu
    #:build-menu
    #:add-item
    #:add-separator
    #:initialize-status-item
    #:initialize-status-item-with-image))
(in-package #:barista/menu)


;;; ---- menu constructor registry -------------------------------------------

(defvar *menu-constructors* nil
  "Plist of menu-name symbol -> constructor function.")

;; From NSStatusBar.h
(defconstant +ns-variable-status-item-length+ -1.0d0)

;; NSEventModifierFlags bit for the Option/Alt key
(defconstant +ns-alternate-key-mask+ (expt 2 19))

;; NSControlStateValue constants (NSCell.h)
(defconstant +ns-control-state-off+ 0)
(defconstant +ns-control-state-on+  1)


;;; ---- action callback dispatch (BaristaActionTarget) ----------------------
;;;
;;; A single shared NSObject subclass receives all menu-item action messages.
;;; CL closures are stored in a hash-table keyed by NSMenuItem pointer address.

(defvar *action-callbacks* (make-hash-table)
  "Maps NSMenuItem pointer-address (integer) -> CL thunk.")

(defvar *action-target-class*    nil)
(defvar *action-target-instance* nil)

(cffi:defcallback %barista-action-cb :void
    ((self :pointer) (cmd :pointer) (sender :pointer))
  (declare (ignore self cmd))
  ;; sender is the NSMenuItem. Look up the CL closure and call it.
  ;; Callbacks accept zero arguments; *plugin* is rebound inside the closure.
  (let ((fn (gethash (cffi:pointer-address sender) *action-callbacks*)))
    (when fn
      (handler-case (funcall fn sender)
        (error (e)
          (log:error "Menu item callback error: ~A" e))))))

(defun ensure-action-target ()
  "Lazily create the BaristaActionTarget ObjC class and shared instance."
  (unless *action-target-class*
    (let ((cls (%alloc-class (%cls "NSObject") "BaristaActionTarget" 0)))
      (%add-method cls
                   (%sel "performAction:")
                   (cffi:callback %barista-action-cb)
                   "v@:@")
      (%reg-class cls)
      (setf *action-target-class* cls)))
  (unless *action-target-instance*
    (setf *action-target-instance* (alloc-init "BaristaActionTarget")))
  *action-target-instance*)


;;; ---- click handler (BaristaClickTarget) ----------------------------------
;;;
;;; Each NSStatusItem's action/target is set to a shared BaristaClickTarget
;;; instance.  The sender (NSStatusItem) pointer is used to look up the
;;; corresponding plugin's menu-thunk and to detect Alt-key presses.

(defvar *click-table* (make-hash-table)
  "Maps NSStatusBarButton pointer-address (integer) -> barista/classes:status-item instance.
  Keyed by button (not NSStatusItem) because AppKit passes the button as the action sender.")

(defvar *click-target-class*    nil)
(defvar *click-target-instance* nil)

(defun %append-system-section (menu)
  "Append a separator, a Settings > Plugins submenu, and a Quit item to MENU.
  Called automatically after every plugin menu-thunk so users always have
  access to plugin management regardless of which plugin they clicked."
  ;; Separator line
  (send menu "addItem:"
        :pointer (send (%cls "NSMenuItem") "separatorItem" :pointer)
        :void)
  ;; Settings > Plugins submenu -- built lazily via symbol-call to avoid a
  ;; compile-time circular dependency: menu <- system-plugin -> menu.
  (let ((plugins-menu (uiop:symbol-call :barista/system-plugin
                                        :make-plugins-submenu)))
    (%add-menu-item menu "Plugins" :submenu plugins-menu))
  ;; Quit
  (%add-menu-item menu "Quit"
                  :callback (lambda ()
                              (log:info "Quitting")
                              (uiop:quit 0))))

(cffi:defcallback %barista-click-cb :void
    ((self :pointer) (cmd :pointer) (sender :pointer))
  (declare (ignore self cmd))
  ;; sender is the NSStatusBarButton (not the NSStatusItem).
  ;; We look up the Lisp status-item object by button address, then
  ;; pass sender directly as the inView argument for popUpMenu...
  (handler-case
      (let* ((status-item-obj (gethash (cffi:pointer-address sender) *click-table*))
             (app   (send (%cls "NSApplication") "sharedApplication" :pointer))
             (event (send app "currentEvent" :pointer))
             (flags (send event "modifierFlags" :ulong))
             (alt-p (plusp (logand flags +ns-alternate-key-mask+)))
             (menu  (if alt-p
                        (get-barista-menu)
                        (when status-item-obj
                          (let ((thunk (get-menu-thunk status-item-obj)))
                            (when thunk (funcall thunk)))))))
        (when (and menu (not (cffi:null-pointer-p menu)))
          ;; Append separator + Settings + Quit to every plugin menu so the
          ;; user can always manage plugins without the system icon.
          ;; Skip for: Alt-click maintenance menu (already has Quit), and
          ;; the system plugin itself (its menu IS the Settings menu).
          (unless (or alt-p
                      (and status-item-obj
                           (barista/classes:system-item-p status-item-obj)))
            (%append-system-section menu))
          ;; sender IS the button -- pass it directly as inView.
          (send menu
                "popUpMenuPositioningItem:atLocation:inView:"
                :pointer (cffi:null-pointer)  ; no anchoring item
                :double  0.0d0                ; CGPoint.x
                :double  0.0d0                ; CGPoint.y
                :pointer sender               ; inView = the button itself
                :void)))
    (error (e)
      (log:error "Status item click handler error: ~A" e))))

(defun ensure-click-target ()
  "Lazily create the BaristaClickTarget ObjC class and shared instance."
  (unless *click-target-class*
    (let ((cls (%alloc-class (%cls "NSObject") "BaristaClickTarget" 0)))
      (%add-method cls
                   (%sel "handleClick:")
                   (cffi:callback %barista-click-cb)
                   "v@:@")
      (%reg-class cls)
      (setf *click-target-class* cls)))
  (unless *click-target-instance*
    (setf *click-target-instance* (alloc-init "BaristaClickTarget")))
  *click-target-instance*)


;;; ---- status-item initialisation ------------------------------------------

(defun initialize-status-item (item)
  "Create the AppKit NSStatusItem for the barista/classes:status-item ITEM.
  Must be called on the AppKit main thread."
  (let* ((bar      (send (%cls "NSStatusBar") "systemStatusBar" :pointer))
         (ns-item  (send bar "statusItemWithLength:"
                         :double +ns-variable-status-item-length+ :pointer)))
    ;; Retain so it survives beyond the autorelease pool sweep.
    (send ns-item "retain" :pointer)

    ;; Set initial title.
    (let ((title (get-title item)))
      (cond
        ((and (cffi:pointerp title) (not (cffi:null-pointer-p title)))
         (send ns-item "setAttributedTitle:" :pointer title :void))
        ((stringp title)
         (send ns-item "setTitle:" :pointer (ns-str title) :void))))

    ;; Tooltip.
    (let ((button (send ns-item "button" :pointer)))
      (when (and button (not (cffi:null-pointer-p button)))
        (send button "setToolTip:"
              :pointer (ns-str (format nil "Barista plugin \"~A\""
                                       (symbol-name (type-of *plugin*))))
              :void)))

    ;; Wire up the click handler.
    ;; AppKit routes the action through the button, so we set action/target
    ;; on the button and key *click-table* by button address -- that is what
    ;; %barista-click-cb receives as `sender`.
    (let ((click-target (ensure-click-target))
          (button (send ns-item "button" :pointer)))
      (send button "setAction:" :pointer (%sel "handleClick:") :void)
      (send button "setTarget:" :pointer click-target :void)
      ;; Register so the click callback can find the Lisp status-item object.
      (setf (gethash (cffi:pointer-address button) *click-table*) item))

    (setf (barista/classes:get-ns-status-item item) ns-item)
    (log:info "NSStatusItem initialized for ~A" (type-of *plugin*))
    (values)))


;;; ---- hide / remove -------------------------------------------------------

(defgeneric hide (item)
  (:documentation "Remove ITEM from the status bar and release the NSStatusItem.")
  (:method ((item barista/classes:status-item))
    (let ((ns-item (get-ns-status-item item))
          (bar     (send (%cls "NSStatusBar") "systemStatusBar" :pointer)))
      (when (and ns-item (not (cffi:null-pointer-p ns-item)))
        ;; Deregister from click table (keyed by button address).
        (let ((button (send ns-item "button" :pointer)))
          (when (and button (not (cffi:null-pointer-p button)))
            (remhash (cffi:pointer-address button) *click-table*)))
        ;; setView:nil workaround for stale status bar display:
        ;; https://stackoverflow.com/questions/23066802
        (send ns-item "setView:" :pointer (cffi:null-pointer) :void)
        (send bar "removeStatusItem:" :pointer ns-item :void)
        (send ns-item "release" :void)
        (setf (barista/classes:get-ns-status-item item) nil)))))


;;; ---- NSMenu / NSMenuItem construction ------------------------------------

(defun %make-ns-menu ()
  "Allocate a new, empty NSMenu."
  (let ((menu (alloc-init "NSMenu")))
    (send menu "setAutoenablesItems:" :bool nil :void)
    menu))

(defun %add-menu-item (menu title &key callback submenu url state)
  "Add one item to NSMenu pointer MENU.
  TITLE may be a plain string or an NSAttributedString pointer.
  STATE, when non-NIL, sets the checkmark state: T means checked (on), NIL means unchecked.
  Returns the NSMenuItem pointer."
  (let* ((action (if (or callback url) (%sel "performAction:") (cffi:null-pointer)))
         (ns-item (send (send (%cls "NSMenuItem") "alloc" :pointer)
                        "initWithTitle:action:keyEquivalent:"
                        :pointer (if (stringp title) (ns-str title) (ns-str ""))
                        :pointer action
                        :pointer (ns-str "")
                        :pointer)))

    ;; Attributed title takes precedence over plain text.
    (when (and (cffi:pointerp title) (not (cffi:null-pointer-p title)))
      (send ns-item "setAttributedTitle:" :pointer title :void))

    (when (and callback url)
      (error "~A: :callback and :url cannot be specified simultaneously." '%add-menu-item))

    (when url
      (setf callback
            (let ((u url))
              (lambda (&rest _) (declare (ignore _)) (open-url u)))))

    (when callback
      ;; Capture *plugin* into the closure so callbacks always know their plugin.
      ;; The stored thunk accepts the NSMenuItem sender pointer as its argument,
      ;; matching the old LispWorks behaviour where callbacks received the item.
      (let ((target          (ensure-action-target))
            (captured-plugin *plugin*)
            (cb              callback))
        (send ns-item "setTarget:" :pointer target :void)
        (setf (gethash (cffi:pointer-address ns-item) *action-callbacks*)
              (lambda (sender)
                (declare (ignore sender))
                (let ((*plugin* captured-plugin))
                  (funcall cb))))))

    (when submenu
      (send ns-item "setSubmenu:" :pointer (make-menu submenu) :void))

    ;; Apply checkmark state if requested.
    (when (not (null state))
      (send ns-item "setState:"
            :long (if state +ns-control-state-on+ +ns-control-state-off+)
            :void))

    (send menu "addItem:" :pointer ns-item :void)
    ns-item))


;;; ---- defmenu macro -------------------------------------------------------

(defun %make-menu-item-call (menu-var data)
  "Expand one item-spec DATA into a %add-menu-item call form."
  (destructuring-bind (title &key callback submenu url)
      (uiop:ensure-list data)
    `(%add-menu-item ,menu-var
                     ,(get-string-form-for-macro title)
                     :callback ,callback
                     :submenu  ',submenu
                     :url      ,url)))

(defmacro defmenu (name (&rest items))
  "Define a named menu constructor and register it in *menu-constructors*.

  Example:
    (defmenu my-menu
        ((\"Item one\" :callback #'handler)
         (\"Item two\" :url \"https://example.com\")))
  "
  (let* ((func-name (intern (format nil "MAKE-~A-MENU" (string-upcase name))))
         (menu-var  (gensym "MENU"))
         (item-calls (loop for item in items
                           collect (%make-menu-item-call menu-var item))))
    `(progn
       (defun ,func-name ()
         (let ((,menu-var (%make-ns-menu)))
           (log:info "Creating menu ~A" ',name)
           ,@item-calls
           (values ,menu-var)))
       (setf (getf *menu-constructors* ',name) #',func-name))))


;;; ---- build-menu / add-item (imperative API) ------------------------------

(defvar *current-menu* nil
  "Dynamically bound to the NSMenu being built inside build-menu.")

(defun add-item (title &key callback submenu url state)
  "Add an item to the menu currently being built by build-menu.
  Must be called inside a build-menu body.
  STATE: when T sets the native macOS checkmark (NSControlStateValueOn)."
  (unless *current-menu*
    (error "add-item must be called inside a build-menu body."))
  (%add-menu-item *current-menu* title
                  :callback callback
                  :submenu  submenu
                  :url      url
                  :state    state))

(defun add-separator ()
  "Add a native NSMenuItem separator to the menu currently being built.
  Must be called inside a build-menu body."
  (unless *current-menu*
    (error "add-separator must be called inside a build-menu body."))
  (send *current-menu* "addItem:"
        :pointer (send (%cls "NSMenuItem") "separatorItem" :pointer)
        :void))

(defmacro build-menu (&body body)
  "Evaluate BODY with *current-menu* bound to a fresh NSMenu, then return it."
  `(let ((*current-menu* (%make-ns-menu)))
     ,@body
     (values *current-menu*)))


;;; ---- make-menu (lookup or build) -----------------------------------------

(defun make-menu (name-or-menu)
  "Return an NSMenu pointer for NAME-OR-MENU.
  NAME-OR-MENU may be a symbol (looked up in *menu-constructors* or as a
  function), or an already-built NSMenu CFFI pointer."
  (cond
    ((symbolp name-or-menu)
     (let ((constructor (or (getf *menu-constructors* name-or-menu)
                            (and (fboundp name-or-menu)
                                 (symbol-function name-or-menu)))))
       (unless constructor
         (error "No menu constructor found for ~A" name-or-menu))
       (funcall constructor)))
    ((and (cffi:pointerp name-or-menu)
          (not (cffi:null-pointer-p name-or-menu)))
     name-or-menu)
    (t
     (error "make-menu: ~S is neither a symbol nor an NSMenu pointer." name-or-menu))))


;;; ---- image status-item initialisation ------------------------------------

(defun initialize-status-item-with-image (item image-path &key (size 18.0d0) template)
  "Like initialize-status-item but uses an image file instead of a text title.
  IMAGE-PATH is a CL pathname or namestring to a PNG/ICNS file.
  SIZE     -- desired icon size in points (default 18); passed to make-ns-image.
  TEMPLATE -- when T, marks the image as a template (monochrome/symbolic icons
              that should adapt to dark/light mode).  Leave NIL (default) for
              colour images so pixels are rendered as-is."
  (let* ((bar     (send (%cls "NSStatusBar") "systemStatusBar" :pointer))
         (ns-item (send bar "statusItemWithLength:"
                        :double +ns-variable-status-item-length+ :pointer)))
    ;; Retain so it survives beyond the autorelease pool sweep.
    (send ns-item "retain" :pointer)

    ;; Load the image via the shared helper in barista/classes.
    (let ((image (barista/classes:make-ns-image image-path
                                                :size size
                                                :template template)))
      (when image
        ;; setImage: directly on the NSStatusItem -- the API that reliably
        ;; works across macOS versions (confirmed by eliza-limits-monitor).
        (send ns-item "setImage:" :pointer image :void)))

    ;; Wire up the click handler (same as initialize-status-item).
    (let ((click-target (ensure-click-target))
          (button (send ns-item "button" :pointer)))
      (send button "setAction:" :pointer (%sel "handleClick:") :void)
      (send button "setTarget:" :pointer click-target :void)
      (setf (gethash (cffi:pointer-address button) *click-table*) item))

    (setf (barista/classes:get-ns-status-item item) ns-item)
    (log:info "NSStatusItem (image) initialized")
    (values)))


;;; ---- built-in Barista maintenance menu -----------------------------------

(defparameter *barista-menu* nil
  "Lazily-built maintenance menu shown when Alt is held during a click.")

(defun get-barista-menu ()
  "Return the Barista maintenance NSMenu, building it the first time."
  (or *barista-menu*
      (setf *barista-menu*
            (build-menu
              (add-item "Restart all plugins"
                        :callback (lambda ()
                                    (uiop:symbol-call :barista/plugin :restart-plugins)))
              (add-item "Quit"
                        :callback (lambda ()
                                    (log:info "Quitting")
                                    (uiop:quit 0)))))))
