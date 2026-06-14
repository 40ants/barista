# Barista — Architecture

Barista is a macOS menu-bar application framework written in Common Lisp (SBCL).
Plugins run as threads inside the main Lisp process and display live information
in the macOS status bar. Each plugin gets its own status-bar icon and menu.

## Repository layout

```
barista.asd           ASDF system (package-inferred)
barista-plugins.asd   Bundled plugins system
objc.lisp             CFFI bridge to ObjC runtime + GCD
classes.lisp          CLOS status-item, NSColor/NSFont/NSAttributedString helpers
menu.lisp             Menu DSL, ObjC click/action targets, menu construction
plugin.lisp           defplugin macro, plugin lifecycle, worker threads
utils.lisp            on-main-thread, format-duration, open-url
vars.lisp             Global specials: *plugin*, *debug*, constants
main.lisp             Entry point: load plugins, start AppKit, SLYNK server
notify.lisp           macOS notifications via osascript
plugins/
  pomodoro.lisp       Built-in Pomodoro timer plugin
  git-reps.lisp       Built-in git repository monitor plugin
app.ros               Roswell launcher (--slynk / --swank / --port flags)
build.sh              Builds standalone Barista.app via qlot + ros dump
docs/
  architecture.md     This file
```

## Startup sequence

```
main()
  └─ log4cl setup
  └─ (optional) slynk:create-server / swank:create-server
  └─ load-plugins          ; load *.lisp from ~/.config/barista/plugins/
  └─ trivial-main-thread:with-body-in-main-thread  ; hand off to macOS thread 0
       └─ sb-int:set-floating-point-modes :traps nil   ; required before [NSApp run]
       └─ load-objc-frameworks                         ; libobjc + libdispatch + Foundation + AppKit
       └─ [NSApplication sharedApplication]
       └─ setActivationPolicy: Accessory              ; no Dock icon
       └─ start-plugins                               ; instantiate all defplugin classes
       └─ [NSApp run]                                 ; never returns
```

`trivial-main-thread` is used **only once at startup** to move execution onto
macOS thread 0 before AppKit is initialised. After `[NSApp run]` takes over,
the main thread belongs to AppKit and must not be blocked by Lisp code.
All subsequent main-thread work is delivered via GCD (see below).

## Package dependency graph

```
barista/objc
    ↑
barista/vars   barista/classes
         ↑      ↑
         barista/utils
              ↑
         barista/menu
              ↑
         barista/plugin
              ↑
         barista/main
```

`barista/objc` has no Barista dependencies — it is a self-contained CFFI bridge.

## ObjC bridge (objc.lisp)

All ObjC interaction goes through raw CFFI calls to `libobjc.A.dylib`.
No ObjC wrapper library is used.

### Core bindings

| Lisp name | C function | Purpose |
|---|---|---|
| `%sel` | `sel_registerName` | String → ObjC selector |
| `%cls` | `objc_getClass` | String → class pointer |
| `%alloc-class` | `objc_allocateClassPair` | Create a new ObjC class at runtime |
| `%reg-class` | `objc_registerClassPair` | Publish the class to the runtime |
| `%add-method` | `class_addMethod` | Add a method (IMP) to a class |
| `%add-protocol` / `%get-protocol` | `class_addProtocol` / `objc_getProtocol` | Protocol adoption |

### Message sending

```lisp
(send receiver "selectorName:" :type arg ... :return-type)
;; expands to:
(cffi:foreign-funcall "objc_msgSend" :pointer receiver :pointer (%sel "selectorName:") ...)
```

`send` is a macro — types and return type are specified inline at each call site,
avoiding the variadic-args ABI issues with declaring `objc_msgSend` as varargs.

### Common patterns

```lisp
(alloc-init "NSMenu")              ; [[NSMenu alloc] init]
(ns-str "hello")                   ; NSString from Lisp string
(lisp-str ns-ptr)                  ; Lisp string from NSString pointer
(with-cgrect (r 0 0 100 20) ...)   ; stack-allocated CGRect
```

## Main-thread dispatch (GCD)

After AppKit takes over the main thread, UI mutations from worker threads must
be delivered via Grand Central Dispatch.

```lisp
(barista/objc:call-on-main-thread thunk)
```

Internally uses `dispatch_async_f(dispatch_get_main_queue(), ctx, callback)`.
`dispatch_get_main_queue()` is a C macro for `&_dispatch_main_q` (a global
variable in `libdispatch.dylib`) — obtained via `cffi:foreign-symbol-pointer`.

Thunks are kept alive in `*dispatch-thunks*` (a hash-table) until GCD fires
them, preventing GC collection before execution.

`on-main-thread` in `utils.lisp` is the macro wrapper used throughout:

```lisp
(on-main-thread
  (send ns-item "setTitle:" :pointer (ns-str title) :void))
```

## ObjC subclasses created at runtime

Barista registers three custom ObjC classes using `%alloc-class` + `%add-method`
+ `%reg-class`. All use `cffi:defcallback` for their method implementations.

### BaristaClickTarget

Handles clicks on status-bar icons.

- Method: `handleClick:` (`v@:@`), set as `setAction:` on each `NSStatusBarButton`
- `sender` in the callback is the `NSStatusBarButton`
- Looks up the Lisp `status-item` object in `*click-table*` (keyed by button
  pointer address)
- Reads `NSApplication currentEvent modifierFlags` to detect Alt key
- **Alt held**: shows the Barista maintenance menu (Restart all plugins / Quit)
- **Normal click**: calls the plugin's `menu-thunk` to build a fresh `NSMenu`,
  then calls `popUpMenuPositioningItem:atLocation:inView:` with the button as
  the view

### BaristaActionTarget

Handles clicks on individual menu items.

- Method: `performAction:` (`v@:@`), set as `setAction:` on each `NSMenuItem`
- `sender` in the callback is the `NSMenuItem`
- Dispatches to a CL closure stored in `*action-callbacks*` (keyed by
  `NSMenuItem` pointer address)
- A single shared instance is the target for all menu items across all plugins

### BaristaClickTarget and action-callbacks are global singletons

Both ObjC classes are registered once and reused across all plugins and menus.
Registration is lazy (first use). The hash-tables `*click-table*` and
`*action-callbacks*` are the dispatch mechanisms.

## status-item (classes.lisp)

Pure CLOS class — no ObjC backing class needed.

```
status-item
  ns-status-item  — CFFI pointer to the AppKit NSStatusItem
  title           — current label (plain string or NSAttributedString pointer)
  menu-thunk      — (or null function): called on each click to build NSMenu
```

`(setf get-title)` automatically dispatches to the main thread via
`call-on-main-thread` so worker threads can safely update the status-bar label.

`NSFontAttributeName` and `NSForegroundColorAttributeName` are resolved via
`cffi:foreign-symbol-pointer` from AppKit's exported globals (in C these are
accessed as macros, not function calls).

## Plugin system

### defplugin macro

```lisp
(defplugin pomodoro
    ((count-to :initform nil :accessor get-count-to)
     (state    :initform :disabled :accessor get-state))
  (:title "🍅")
  (:menu start)
  (:every :second (update)))
```

Expands to:

1. `defclass pomodoro (base-plugin) (...)` — CLOS class with plugin-specific slots
2. `(pushnew 'pomodoro *available-plugins*)` — registration
3. `initialize-plugin :after` method that:
   - Binds `*plugin*` to the new instance
   - Creates a `status-item` with `:title` and `:menu-thunk`
   - Calls `initialize-status-item` to create the `NSStatusItem` in AppKit
   - Starts worker threads (one per `:every` clause)

### *plugin* dynamic variable

`*plugin*` (`barista/vars`) is the central context variable. It is bound:

- During `initialize-plugin :after` — so menu construction captures the right plugin
- Inside each worker thread (via `:initial-bindings`) — so `update` functions
  can call `(setf (get-title *plugin*) ...)` etc.
- Inside menu-thunks — thunks capture the plugin instance at construction time
  and rebind `*plugin*` on each invocation (menus are rebuilt on every click)
- Inside menu item callbacks — captured into closures at menu-build time

### Menu-thunk pattern

Menus are **built fresh on every click**. When a plugin is started, a
`menu-thunk` closure is stored on the `status-item`:

```lisp
(let ((mn 'start) (p plugin))
  (lambda ()
    (let ((*plugin* p))
      (make-menu mn))))
```

When the user clicks the icon, `%barista-click-cb` calls `(funcall thunk)`,
which calls the `make-<name>-menu` function (generated by `defmenu`), creating
fresh `NSMenu` and `NSMenuItem` objects with new pointer addresses registered
in `*action-callbacks*`.

`replace-menu` (used by pomodoro to switch between start/stop menus) replaces
the thunk on the `status-item` with a new closure pointing at the new menu name.

### Worker threads

Each `:every` clause generates a `bordeaux-threads:make-thread` call:

```lisp
(loop do
  (handler-bind (...)
    (with-log-unhandled ()
      (with-fields (:plugin *plugin*)
        <user code>))
    (sleep delay)))
```

`*plugin*` is propagated via `:initial-bindings`. Errors are logged and the
loop continues after sleeping. `*debug* = t` causes the debugger to be invoked
instead.

### Plugin lifecycle

```
start-plugin(name)          ; on-main-thread
  → stop-plugin if running
  → make-instance name
  → initialize-plugin       ; registers in *running-plugins*
  → initialize-plugin :after (defplugin-generated)
      → make status-item + NSStatusItem
      → start worker threads

stop-plugin(plugin)         ; on-main-thread
  → destroy worker threads
  → hide status-item        ; removes NSStatusItem from bar, releases it
  → deregister from *running-plugins*

restart-plugins()
  → (mapc #'restart-plugin (running-plugins))
```

## Menu DSL

### defmenu (compile-time)

```lisp
(defmenu start
    (("Start" :submenu start-submenu :callback (f_% (start *last-used-interval*)))
     ("---")
     ("Item" :url "https://example.com")))
```

Generates `make-start-menu` function and registers it in `*menu-constructors*`.
Each item spec: `(title &key callback submenu url)`.
Title can be a plain string, a `(:color ... :font ... :size ...)` attributed
string spec, or a list of such specs to be joined.

### build-menu (imperative, runtime)

```lisp
(build-menu
  (add-item "Item one" :callback #'my-fn)
  (add-item "Item two" :submenu 'my-submenu))
```

Binds `*current-menu*` to a fresh `NSMenu` and returns it. Used when the menu
structure cannot be known at compile time (e.g. git-reps builds items from a
dynamic list of repositories).

## Build

```bash
./build.sh           # produces dist/Barista.app
INSTALL=1 ./build.sh # also copies to /Applications/
```

Steps:
1. `qlot install` (if `.qlot/` absent) — installs pinned dependencies
2. `qlot exec ros -L sbcl-bin dump executable app.ros` — produces `app`
3. `mv app barista`
4. Assemble `dist/Barista.app` bundle with `Contents/MacOS/barista` + `Info.plist`
5. Optionally generate `.icns` from `images/icon.png` via `sips` + `iconutil`

The executable is a self-contained SBCL image (~18 MB) with all Lisp code
and dependencies baked in.

## Running with a REPL

```bash
# From the built executable:
./dist/Barista.app/Contents/MacOS/barista --slynk
./dist/Barista.app/Contents/MacOS/barista --slynk --port 4005

# From source via Roswell:
qlot exec ros app.ros --slynk

# Connect from SLY:
M-x sly-connect RET 127.0.0.1 RET 5007 RET
```

Also supports `--swank` for SLIME. Default port is 5007.

Logs are written to `/tmp/barista.log`.

## User plugins

Place `.lisp` files in `~/.config/barista/plugins/`. They are loaded at startup
before AppKit initialises. A minimal plugin:

```lisp
(defpackage #:my-plugin
  (:use #:cl)
  (:import-from #:barista/plugin #:defplugin #:get-title)
  (:import-from #:barista/menu   #:defmenu #:build-menu #:add-item)
  (:import-from #:barista/vars   #:*plugin*))
(in-package #:my-plugin)

(defmenu my-menu
    (("Hello" :callback (lambda () (format t "clicked~%")))))

(defplugin my-plugin ()
  (:title "Hi")
  (:menu my-menu)
  (:every (5 :seconds)
    (setf (get-title *plugin*)
          (format nil "~A" (get-universal-time)))))
```

## Known constraints

- **macOS only** — AppKit and GCD are macOS-specific. The ObjC bridge targets
  arm64 (Apple Silicon); x86-64 may need ABI adjustments for struct-by-value
  arguments (CGPoint, CGRect).
- **`setHighlightMode:`** — omitted in the SBCL port; deprecated in macOS 14+.
- **`notify.lisp`** — uses `osascript` subprocess; no dependency on AppKit
  notification APIs.
- **`git-reps` plugin** — depends on `cl-strings`, `priority-queue`, `cl-ppcre`
  which must be available via Quicklisp/qlot.
