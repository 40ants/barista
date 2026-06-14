(uiop:define-package #:barista-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:barista-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:barista-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "barista-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS")))
  )


(defsection @index (:title "Barista — a macOS menu-bar application framework in Common Lisp."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "Unlicense"
                                   "ASDF"
                                   "API"
                                   "URL"
                                   "URI"
                                   "REPL"
                                   "SLY"
                                   "SLIME"
                                   "SBCL"
                                   "CFFI"
                                   "GCD"
                                   "ObjC"
                                   "AppKit"
                                   "macOS"
                                   "PNG"
                                   "ICNS"
                                   "DMG"
                                   "NSStatusItem"
                                   "NSMenu"
                                   "NSImage"
                                   "NSString"
                                   "NSAttributedString"
                                   "NSPasteboard"
                                   "XBar"
                                   "TextBar"
                                   "Dock"
                                   "OS"
                                   "IDE"
                                   "UI"
                                   "CLOS"
                                   "USD"
                                   "EUR"
                                   "XML"
                                   "CPU"
                                   "GPU"
                                   "IOK"
                                   "PERIOD"
                                   "EXPR"
                                   "PATH"
                                   "SIZE"
                                   "TEMPLATE"
                                   "FORMS"
                                   "DSL"
                                   "NSS"
                                   "NSM"
                                   "NSI"
                                   "NSF"
                                   "NSA"
                                   "NSC"
                                   "CGR"
                                   "CL"
                                   "UTF-8"
                                   "NOTE"
                                   "IOKit"
                                   "NSColor"
                                   "NSFont"
                                   "Mach"))
  (barista system)
  """
Barista is a macOS menu-bar application framework written in Common Lisp.
It lets you display live information — text, emojis, icons, or dynamically
rendered images — in the macOS status bar.

Plugins run as threads inside a single Lisp process and communicate with
AppKit via Grand Central Dispatch (GCD). Each plugin gets its own
`NSStatusItem` with an icon and a dropdown menu.

This is similar to [XBar](https://github.com/matryer/xbar) and
[TextBar](http://richsomerfield.com/apps/textbar/), but with key advantages:

| Feature | XBar | Barista |
|---|---|---|
| Plugin execution | Subprocess per refresh | Threads in one Lisp process |
| State persistence | External storage | All in memory |
| Plugin language | Any (mostly Bash) | Common Lisp |
| Menu API | Text-based | Programmatic DSL + ObjC interop |
| Debugging | None | Live REPL via SLYNK/SWANK |

Because plugins are Lisp code running in the same process, you can connect
your IDE (SLIME/SLY) to the running Barista instance and debug plugins
interactively.
"""
  (@introduction section)
  (@installation section)
  (@available-plugins section)
  (@writing-plugins section)
  (@api section))


(defsection-copy @readme @index)


(defsection @introduction (:title "Introduction")
  """
![](./docs/images/pomodoro.gif)

Barista turns the macOS menu bar into a live Lisp-powered dashboard.

A **plugin** is a Common Lisp class defined with the `defplugin` macro.
Each plugin can:

* Display a **text label** (plain string, emoji, or attributed string with
  colors and fonts) as its status-bar icon.
* Display an **image icon** (PNG or ICNS file, or a dynamically rendered
  `NSImage`).
* Run **background worker threads** on a schedule (every N seconds, minutes,
  or hours).
* Show a **dropdown menu** when the user clicks the icon — built either
  declaratively at compile time (`defmenu`) or dynamically at runtime
  (`build-menu`).

### How it works

Barista loads user plugins from `~/.config/barista/plugins/` at startup,
then initializes AppKit on the macOS main thread and enters the
`[NSApp run]` event loop. Worker threads update `NSStatusItem` objects via
GCD (`dispatch_async` on the main queue), ensuring all UI mutations happen
on the main thread.

### Key features

* **In-memory state** — no external files or databases; plugin state lives
  in CLOS slots for the lifetime of the process.
* **Programmatic menus** — build menus with Lisp closures, not text formats.
  Items support callbacks, submenus, checkmarks, and opening URLs.
* **Dynamic icons** — render images at runtime (e.g. bar-chart graphs) and
  update them from worker threads.
* **Live debugging** — connect SLIME/SLY to the running process and inspect
  or modify plugins on the fly.
* **Configuration persistence** — plugin enable/disable state is saved to
  `~/.config/barista/settings.lisp` and restored on launch.
""")


(defsection @installation (:title "Installation")
  """
### From a pre-built release

Download the latest `Barista-x.y.z.dmg` from the
[GitHub Releases](https://github.com/40ants/barista/releases) page,
open it and drag **Barista.app** to your **Applications** folder.

Because the app is not signed with an Apple Developer certificate, macOS
will attach a quarantine flag to it when downloaded from the internet and
refuse to open it with a *"Barista is damaged and can't be opened"* message.

To remove the quarantine flag, run the following command in Terminal after
copying the app to `/Applications`:

```
xattr -dr com.apple.quarantine /Applications/Barista.app
```

Then open the app normally.


### Build from source

Barista uses [Qlot](https://github.com/fukamachi/qlot) for dependency
management and [Roswell](https://github.com/roswell/roswell) for building.

```
git clone https://github.com/40ants/barista.git
cd barista
qlot install
./build.sh
```

This produces `dist/Barista.app`. To also copy it to `/Applications`:

```
INSTALL=1 ./build.sh
```

### Running with a REPL

You can connect your IDE to a running Barista instance for live debugging:

```
# From the built executable:
./dist/Barista.app/Contents/MacOS/barista --slynk
./dist/Barista.app/Contents/MacOS/barista --slynk --port 4005

# From source via Roswell:
qlot exec ros app.ros --slynk
```

Connect from SLY:

```
M-x sly-connect RET 127.0.0.1 RET 5007 RET
```

SLIME is also supported via the `--swank` flag. Default port is 5007.
Logs are written to `/tmp/barista.log`.
""")


(defsection @available-plugins (:title "Available Plugins")
  """
Barista ships with several bundled plugins. Enable or disable them from
the menu-bar icon → **Plugins** submenu (visible when no other plugin is
active, or via Alt-click on any plugin icon).

### Pomodoro

A [Pomodoro Technique](https://en.wikipedia.org/wiki/Pomodoro_Technique)
timer. Shows a tomato emoji (🍅) when idle and a countdown when running.
Choose 5, 15, or 25 minute intervals from the dropdown menu. The countdown
turns red in the last 3 minutes. A macOS notification fires when the timer
starts and stops.

### Clipboard

A clipboard history manager. Polls the system pasteboard every second and
keeps the last 10 copied items. Click the icon (📋) to see the history;
click any item to copy it back to the clipboard. Includes a "Clear history"
option.

### Currency

Displays USD and EUR exchange rates from the
[Central Bank of Russia](https://www.cbr.ru/) daily XML feed. The status-bar
label flips between `$` and `€` rates every 5 seconds. Rates are fetched on
startup and refreshed every 30 minutes. The dropdown menu shows both rates
side by side.

### System Monitor

Shows real-time CPU, GPU, and memory usage as a colored bar-chart icon in
the status bar (rendered dynamically as a PNG). The dropdown menu displays
numeric percentages for each metric. Colors change from green to orange to
red as usage increases. Metrics are read directly from Mach/IOKit APIs via
CFFI — no external commands.
""")


(defsection @writing-plugins (:title "Writing Custom Plugins")
  """
Plugins live as `.lisp` files in `~/.config/barista/plugins/`. Each file is
loaded at startup before AppKit initialises.

### Minimal text plugin

The simplest plugin shows a text label and updates it on a schedule:

```lisp
(defpackage #:my-plugins/clock
  (:use #:cl)
  (:import-from #:barista/plugin
                #:defplugin
                #:get-title)
  (:import-from #:barista/vars
                #:*plugin*))
(in-package #:my-plugins/clock)

(defplugin clock ()
  (:title "🕐")
  (:every :minute
    (setf (get-title *plugin*)
          (local-time:format-timestring nil (local-time:now)
                                        :format '(:hour ":" (:min 2))))))
```

### defplugin options

The `defplugin` macro accepts the following options:

* `(:title EXPR)` — initial menu-bar label. May be a plain string, an emoji,
  or an `NSAttributedString` built with `make-attributed-string` /
  `join-attributed-string`.
* `(:image PATH &key size template)` — display a PNG or ICNS file as the
  status-bar icon. `:size` scales the image (default 18). `:template t`
  marks it as a template image so macOS adapts it to dark/light mode.
  Takes precedence over `:title`.
* `(:menu SYMBOL)` — name of a `defmenu` form to display on click.
* `(:every PERIOD FORMS…)` — starts a background worker thread. `PERIOD` is
  a keyword (`:second`, `:minute`, `:hour`) or a list like `(15 :seconds)`.

You can define custom slots on the plugin class by listing them after the
plugin name, just like `defclass`:

```lisp
(defplugin my-plugin
    ((counter :initform 0 :accessor get-counter))
  (:title "0")
  (:every :second
    (incf (get-counter *plugin*))
    (setf (get-title *plugin*)
          (format nil "~D" (get-counter *plugin*)))))
```

### Image icon plugin

```lisp
(defpackage #:my-plugins/status
  (:use #:cl)
  (:import-from #:barista/plugin
                #:defplugin
                #:get-image)
  (:import-from #:barista/vars
                #:*plugin*))
(in-package #:my-plugins/status)

;; Colour icon — pixels render as-is (default).
(defplugin status-colour ()
  (:image #p"~/.config/barista/icons/my-icon.png" :size 18))

;; Monochrome/symbolic icon — macOS recolours it for dark/light mode.
(defplugin status-mono ()
  (:image #p"~/.config/barista/icons/mono-icon.png" :size 18 :template t))
```

You can also update the icon at runtime from a worker thread:

```lisp
(:every (30 :seconds)
  (let ((icon-path (render-current-state-to-png)))
    (setf (get-image *plugin*) icon-path)))
```

`setf get-image` accepts a pathname, a namestring, or an `NSImage` pointer.

### Menus

There are two ways to define a dropdown menu.

**Declarative** — `defmenu` (compile-time, static structure):

```lisp
(defmenu start
    (("Start 5 min"  :callback (lambda () (start 5)))
     ("Start 15 min" :callback (lambda () (start 15)))
     ("---")
     ("Help"         :url "https://example.com")))
```

Item spec: `(title &key callback submenu url state disabled)`.
* `"---"` adds a separator.
* `:callback` is a zero-argument function called on click.
* `:submenu` is a symbol naming another `defmenu`.
* `:url` opens a URL in the default browser.
* `:state t` shows a native checkmark.
* `:disabled t` greys out the item.

**Imperative** — `build-menu` / `add-item` (runtime, dynamic structure):

```lisp
(defun build-my-menu (plugin)
  (build-menu
    (dolist (item (get-items plugin))
      (add-item item :callback (make-item-callback item)))
    (add-separator)
    (add-item "Clear" :callback (lambda () (clear plugin)))))
```

Use `build-menu` when the menu structure depends on runtime data.

### Dynamic menu wiring

When a plugin needs a dynamic menu (built at runtime), wire up the
`menu-thunk` on the first worker tick instead of using the `:menu` option:

```lisp
(defplugin my-plugin
    ((menu-ready :initform nil :accessor get-menu-ready))
  (:title "Data")
  (:every (5 :seconds)
    (unless (get-menu-ready *plugin*)
      (setf (barista/classes:get-menu-thunk
             (barista/plugin:get-status-item *plugin*))
            (let ((p *plugin*))
              (lambda () (build-dynamic-menu p))))
      (setf (get-menu-ready *plugin*) t))
    (update-data *plugin*)))
```

This pattern is used by the clipboard, currency, and system-monitor plugins.

### The *plugin* variable

`barista/vars:*plugin*` is the central context variable. It is bound inside
worker threads, menu thunks, and menu callbacks automatically. Always use it
to access the current plugin instance:

```lisp
(setf (get-title *plugin*) "Updated!")
(setf (get-image *plugin*) #p"/path/to/icon.png")
```

### Switching menus at runtime

Use `replace-menu` to swap the dropdown menu of a running plugin:

```lisp
(barista/plugin:replace-menu start-menu-symbol stop-menu-symbol)
```

This replaces the `menu-thunk` on the status item.

### Enabling plugins

On first launch Barista shows a system icon in the menu bar. Click it and
choose **Plugins** to enable the plugins you want. The selection is saved
to `~/.config/barista/settings.lisp` and restored on every subsequent launch.

You can also Alt-click any plugin icon to access a maintenance menu with
options to restart all plugins or quit Barista.
""")


(defautodoc @api (:system "barista"
                  :ignore-packages ("barista/objc")))
