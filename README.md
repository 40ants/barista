<a id="x-28BARISTA-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Barista — a macOS menu-bar application framework in Common Lisp.

<a id="barista-asdf-system-details"></a>

## BARISTA ASDF System Details

* Description: mac`OS` menu-bar application framework in Common Lisp (`SBCL`/`CFFI`)
* Depends on: [40ants-logging][422a], [alexandria][8236], [bordeaux-threads][3dbf], [cffi][0383], [cl-colors][b139], [cxml][6303], [defmain][3266], [dexador][8347], [fmt][a3a3], [local-time][46a1], [local-time-duration][6422], [log4cl][7f8b], [log4cl-extras][691c], [slynk][b440], [swank][3803], [trivial-main-thread][efc1], [ubiquitous][a899], [uiop][5274], [zpng][ce80]

Barista is a mac`OS` menu-bar application framework written in Common Lisp.
It lets you display live information — text, emojis, icons, or dynamically
rendered images — in the mac`OS` status bar.

Plugins run as threads inside a single Lisp process and communicate with
AppKit via Grand Central Dispatch (`GCD`). Each plugin gets its own
`NSStatusItem` with an icon and a dropdown menu.

This is similar to [XBar][4aa0] and
[TextBar][511d], but with key advantages:

| Feature | XBar | Barista |
| --- | --- | --- |
| Plugin execution | Subprocess per refresh | Threads in one Lisp process |
| State persistence | External storage | All in memory |
| Plugin language | Any (mostly Bash) | Common Lisp |
| Menu API | Text-based | Programmatic DSL + ObjC interop |
| Debugging | None | Live REPL via SLYNK/SWANK |

Because plugins are Lisp code running in the same process, you can connect
your `IDE` (`SLIME`/`SLY`) to the running Barista instance and debug plugins
interactively.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40INTRODUCTION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Introduction

![](/Users/art/projects/lisp/barista/docs/images/pomodoro.gif)

Barista turns the mac`OS` menu bar into a live Lisp-powered dashboard.

A **plugin** is a Common Lisp class defined with the `defplugin` macro.
Each plugin can:

* Display a **text label** (plain string, emoji, or attributed string with
  colors and fonts) as its status-bar icon.
* Display an **image icon** (`PNG` or `ICNS` file, or a dynamically rendered
  `NSImage`).
* Run **background worker threads** on a schedule (every N seconds, minutes,
  or hours).
* Show a **dropdown menu** when the user clicks the icon — built either
  declaratively at compile time (`defmenu`) or dynamically at runtime
  (`build-menu`).

<a id="how-it-works"></a>

### How it works

Barista loads user plugins from `~/.config/barista/plugins/` at startup,
then initializes AppKit on the mac`OS` main thread and enters the
`[NSApp run]` event loop. Worker threads update `NSStatusItem` objects via
`GCD` (`dispatch_async` on the main queue), ensuring all `UI` mutations happen
on the main thread.

<a id="key-features"></a>

### Key features

* **In-memory state** — no external files or databases; plugin state lives
  in `CLOS` slots for the lifetime of the process.
* **Programmatic menus** — build menus with Lisp closures, not text formats.
  Items support callbacks, submenus, checkmarks, and opening `URL`s.
* **Dynamic icons** — render images at runtime (e.g. bar-chart graphs) and
  update them from worker threads.
* **Live debugging** — connect `SLIME`/`SLY` to the running process and inspect
  or modify plugins on the fly.
* **Configuration persistence** — plugin enable/disable state is saved to
  `~/.config/barista/settings.lisp` and restored on launch.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

<a id="from-a-pre-built-release"></a>

### From a pre-built release

Download the latest `Barista-x.y.z.dmg` from the
[GitHub Releases][9e19] page,
open it and drag **Barista.app** to your **Applications** folder.

Because the app is not signed with an Apple Developer certificate, mac`OS`
will attach a quarantine flag to it when downloaded from the internet and
refuse to open it with a *"Barista is damaged and can't be opened"* message.

To remove the quarantine flag, run the following command in Terminal after
copying the app to `/Applications`:

```
xattr -dr com.apple.quarantine /Applications/Barista.app
```
Then open the app normally.

<a id="build-from-source"></a>

### Build from source

Barista uses [Qlot][e3ea] for dependency
management and [Roswell][795a] for building.

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
<a id="running-with-a-repl"></a>

### Running with a REPL

You can connect your `IDE` to a running Barista instance for live debugging:

```
# From the built executable:
./dist/Barista.app/Contents/MacOS/barista --slynk
./dist/Barista.app/Contents/MacOS/barista --slynk --port 4005

# From source via Roswell:
qlot exec ros app.ros --slynk
```
Connect from `SLY`:

```
M-x sly-connect RET 127.0.0.1 RET 5007 RET
```
`SLIME` is also supported via the `--swank` flag. Default port is 5007.
Logs are written to `/tmp/barista.log`.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40AVAILABLE-PLUGINS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Available Plugins

Barista ships with several bundled plugins. Enable or disable them from
the menu-bar icon → **Plugins** submenu (visible when no other plugin is
active, or via Alt-click on any plugin icon).

<a id="pomodoro"></a>

### Pomodoro

A [Pomodoro Technique][7a11]
timer. Shows a tomato emoji (🍅) when idle and a countdown when running.
Choose 5, 15, or 25 minute intervals from the dropdown menu. The countdown
turns red in the last 3 minutes. A mac`OS` notification fires when the timer
starts and stops.

<a id="clipboard"></a>

### Clipboard

A clipboard history manager. Polls the system pasteboard every second and
keeps the last 10 copied items. Click the icon (📋) to see the history;
click any item to copy it back to the clipboard. Includes a "Clear history"
option.

<a id="currency"></a>

### Currency

Displays `USD` and `EUR` exchange rates from the
[Central Bank of Russia][3c6a] daily `XML` feed. The status-bar
label flips between `$` and `€` rates every 5 seconds. Rates are fetched on
startup and refreshed every 30 minutes. The dropdown menu shows both rates
side by side.

<a id="system-monitor"></a>

### System Monitor

Shows real-time `CPU`, `GPU`, and memory usage as a colored bar-chart icon in
the status bar (rendered dynamically as a `PNG`). The dropdown menu displays
numeric percentages for each metric. Colors change from green to orange to
red as usage increases. Metrics are read directly from Mach/`IOK`it `API`s via
`CFFI` — no external commands.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40WRITING-PLUGINS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Writing Custom Plugins

Plugins live as `.lisp` files in `~/.config/barista/plugins/`. Each file is
loaded at startup before AppKit initialises.

<a id="minimal-text-plugin"></a>

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
<a id="defplugin-options"></a>

### defplugin options

The `defplugin` macro accepts the following options:

* `(:title EXPR)` — initial menu-bar label. May be a plain string, an emoji,
  or an `NSAttributedString` built with `make-attributed-string` /
  `join-attributed-string`.
* `(:image PATH &key size template)` — display a `PNG` or `ICNS` file as the
  status-bar icon. `:size` scales the image (default 18). `:template t`
  marks it as a template image so mac`OS` adapts it to dark/light mode.
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
<a id="image-icon-plugin"></a>

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

<a id="menus"></a>

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
* `:url` opens a `URL` in the default browser.
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

<a id="dynamic-menu-wiring"></a>

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

<a id="the-plugin-variable"></a>

### The *plugin* variable

`barista/vars:*plugin*` is the central context variable. It is bound inside
worker threads, menu thunks, and menu callbacks automatically. Always use it
to access the current plugin instance:

```lisp
(setf (get-title *plugin*) "Updated!")
(setf (get-image *plugin*) #p"/path/to/icon.png")
```
<a id="switching-menus-at-runtime"></a>

### Switching menus at runtime

Use `replace-menu` to swap the dropdown menu of a running plugin:

```lisp
(barista/plugin:replace-menu start-menu-symbol stop-menu-symbol)
```
This replaces the `menu-thunk` on the status item.

<a id="enabling-plugins"></a>

### Enabling plugins

On first launch Barista shows a system icon in the menu bar. Click it and
choose **Plugins** to enable the plugins you want. The selection is saved
to `~/.config/barista/settings.lisp` and restored on every subsequent launch.

You can also Alt-click any plugin icon to access a maintenance menu with
options to restart all plugins or quit Barista.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FBAR-ICON-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/BAR-ICON

<a id="x-28-23A-28-2816-29-20BASE-CHAR-20-2E-20-22BARISTA-2FBAR-ICON-22-29-20PACKAGE-29"></a>

#### [package](d27f) `barista/bar-icon`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FBAR-ICON-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FBAR-ICON-3ARENDER-BARS-ICON-20FUNCTION-29"></a>

##### [function](9b5b) `barista/bar-icon:render-bars-icon` ratios path

Render a bar-chart icon for `RATIOS` (a list of floats in `0.0, 1.0`) to `PATH`.
Each ratio gets one coloured vertical bar; colour is chosen by usage-color.
Writes a truecolor-alpha `PNG` and returns `PATH`.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FCLASSES-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/CLASSES

<a id="x-28-23A-28-2815-29-20BASE-CHAR-20-2E-20-22BARISTA-2FCLASSES-22-29-20PACKAGE-29"></a>

#### [package](6c43) `barista/classes`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FCLASSES-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FCLASSES-24STATUS-ITEM-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### STATUS-ITEM

<a id="x-28BARISTA-2FCLASSES-3ASTATUS-ITEM-20CLASS-29"></a>

###### [class](f97a) `barista/classes:status-item` ()

Wraps an `NSS`tatusItem for one Barista plugin.

**Readers**

<a id="x-28BARISTA-2FCLASSES-3AGET-BUTTON-ADDRESS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20BARISTA-2FCLASSES-3ASTATUS-ITEM-29-29"></a>

###### [reader](6e21) `barista/classes:get-button-address` (status-item) (= nil)

Integer pointer address of the `NSS`tatusBarButton,
captured at initialisation time and used for *click-table* keying and cleanup.
Storing it avoids calling (send ns-item "button") on a potentially-released
object during hide.

<a id="x-28BARISTA-2FCLASSES-3AGET-MENU-THUNK-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20BARISTA-2FCLASSES-3ASTATUS-ITEM-29-29"></a>

###### [reader](1aaa) `barista/classes:get-menu-thunk` (status-item) (:menu-thunk = nil)

Nullary function that builds and returns an `NSM`enu pointer.

<a id="x-28BARISTA-2FCLASSES-3AGET-NS-STATUS-ITEM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20BARISTA-2FCLASSES-3ASTATUS-ITEM-29-29"></a>

###### [reader](a25e) `barista/classes:get-ns-status-item` (status-item) (= nil)

Raw `CFFI` pointer to the AppKit `NSS`tatusItem.

<a id="x-28BARISTA-2FCLASSES-3ASYSTEM-ITEM-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20BARISTA-2FCLASSES-3ASTATUS-ITEM-29-29"></a>

###### [reader](b489) `barista/classes:system-item-p` (status-item) (:system-item-p = nil)

When T, this is the system plugin item.
The click handler skips appending the Settings/Quit section to its menu
because it already is the Settings menu.

**Accessors**

<a id="x-28BARISTA-2FCLASSES-3AGET-BUTTON-ADDRESS-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20BARISTA-2FCLASSES-3ASTATUS-ITEM-29-29"></a>

###### [accessor](6e21) `barista/classes:get-button-address` (status-item) (= nil)

Integer pointer address of the `NSS`tatusBarButton,
captured at initialisation time and used for *click-table* keying and cleanup.
Storing it avoids calling (send ns-item "button") on a potentially-released
object during hide.

<a id="x-28BARISTA-2FCLASSES-3AGET-MENU-THUNK-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20BARISTA-2FCLASSES-3ASTATUS-ITEM-29-29"></a>

###### [accessor](1aaa) `barista/classes:get-menu-thunk` (status-item) (:menu-thunk = nil)

Nullary function that builds and returns an `NSM`enu pointer.

<a id="x-28BARISTA-2FCLASSES-3AGET-NS-STATUS-ITEM-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20BARISTA-2FCLASSES-3ASTATUS-ITEM-29-29"></a>

###### [accessor](a25e) `barista/classes:get-ns-status-item` (status-item) (= nil)

Raw `CFFI` pointer to the AppKit `NSS`tatusItem.

<a id="x-28BARISTA-2FCLASSES-3ASYSTEM-ITEM-P-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20BARISTA-2FCLASSES-3ASTATUS-ITEM-29-29"></a>

###### [accessor](b489) `barista/classes:system-item-p` (status-item) (:system-item-p = nil)

When T, this is the system plugin item.
The click handler skips appending the Settings/Quit section to its menu
because it already is the Settings menu.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FCLASSES-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28BARISTA-2FCLASSES-3AGET-IMAGE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](1742) `barista/classes:get-image` plugin

Return the current status-bar icon of `PLUGIN`.

<a id="x-28BARISTA-2FCLASSES-3AGET-TITLE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](d94d) `barista/classes:get-title` item

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FCLASSES-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FCLASSES-3AGET-STRING-FORM-FOR-MACRO-20FUNCTION-29"></a>

##### [function](2fdb) `barista/classes:get-string-form-for-macro` string

Return a compile-time form that evaluates to an `NSS`tring or `NSA`ttributedString.
Called by defmenu/build-menu macros to process user-supplied title values.

<a id="x-28BARISTA-2FCLASSES-3AJOIN-ATTRIBUTED-STRING-20FUNCTION-29"></a>

##### [function](0dc6) `barista/classes:join-attributed-string` &rest parts

Concatenate `PARTS` into a single `NSM`utableAttributedString.
Each part may be a plain string, a (text :color ... :font ... :size ...) list,
or an existing `NSA`ttributedString pointer.

<a id="x-28BARISTA-2FCLASSES-3AMAKE-ATTRIBUTED-STRING-20FUNCTION-29"></a>

##### [function](6d3d) `barista/classes:make-attributed-string` text &key font color (size +default-font-size+)

Create an `NSA`ttributedString from `TEXT` with optional `FONT`, `COLOR`, and `SIZE`.

<a id="x-28BARISTA-2FCLASSES-3AMAKE-DEFAULT-FONT-20FUNCTION-29"></a>

##### [function](a36b) `barista/classes:make-default-font` size

Return the standard menu font at `SIZE` points.

<a id="x-28BARISTA-2FCLASSES-3AMAKE-FONT-20FUNCTION-29"></a>

##### [function](4c75) `barista/classes:make-font` name &key (size +default-font-size+)

Return an `NSF`ont for `NAME` at `SIZE` points, or `NIL` if the font is unknown.

<a id="x-28BARISTA-2FCLASSES-3AMAKE-NS-IMAGE-20FUNCTION-29"></a>

##### [function](6a5f) `barista/classes:make-ns-image` path &key (size nil) (template nil)

Load an `NSI`mage from `PATH` (pathname or string).
`PATH` may be a `CL` pathname or a namestring.

Keyword arguments:
  `SIZE`     -- when non-`NIL`, a number; calls setSize: with `SIZE` x `SIZE` points.
  `TEMPLATE` -- when T, marks the image as a template image so mac`OS` adapts
              it to the current appearance (dark/light mode).  Use T only
              for monochrome/symbolic icons.  For colour images leave `NIL`
              (the default) so pixels render as-is.

Returns the `NSI`mage pointer, or `NIL` if the file could not be loaded.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FCONFIG-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/CONFIG

<a id="x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22BARISTA-2FCONFIG-22-29-20PACKAGE-29"></a>

#### [package](b766) `barista/config`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FCONFIG-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FCONFIG-3AENABLED-PLUGIN-NAMES-20FUNCTION-29"></a>

##### [function](6927) `barista/config:enabled-plugin-names`

Return a list of plugin name keywords that are enabled in the config.
Only keys explicitly set to T are included.

<a id="x-28BARISTA-2FCONFIG-3APLUGIN-ENABLED-P-20FUNCTION-29"></a>

##### [function](1ebe) `barista/config:plugin-enabled-p` plugin-name

Return T if `PLUGIN-NAME` is enabled in the configuration.
Defaults to `NIL` (disabled) when no value is stored -- this ensures a
clean first-run experience where the system plugin is shown instead.

<a id="x-28BARISTA-2FCONFIG-3ARESTORE-CONFIG-20FUNCTION-29"></a>

##### [function](d0c6) `barista/config:restore-config`

Load Barista configuration from disk.
If the file does not exist (first launch), bootstraps an empty storage and
points *storage-pathname* at the target path so that subsequent
(setf (value ...)) calls persist to the correct file.

Note: when handler-case catches no-storage-file, ubiquitous leaves
*storage-pathname* pointing at its default global path and does `NOT` set
it to the requested path.  We must set both *storage-pathname* and
*storage* explicitly to make offload work correctly later.

<a id="x-28BARISTA-2FCONFIG-3ASET-PLUGIN-ENABLED-20FUNCTION-29"></a>

##### [function](306a) `barista/config:set-plugin-enabled` plugin-name enabled-p

Persist the enabled/disabled state for `PLUGIN-NAME`.
ubiquitous automatically writes to disk after each (setf value).

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FMAIN-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/MAIN

<a id="x-28-23A-28-2812-29-20BASE-CHAR-20-2E-20-22BARISTA-2FMAIN-22-29-20PACKAGE-29"></a>

#### [package](f2e4) `barista/main`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FMAIN-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FMAIN-3ALOAD-PLUGINS-20FUNCTION-29"></a>

##### [function](a013) `barista/main:load-plugins`

Load all user plugin files from ~/.config/barista/plugins/.

<a id="x-28BARISTA-2FMAIN-3AMAIN-20FUNCTION-29"></a>

##### [function](5c31) `barista/main:main` &rest argv

Start the Barista menu-bar application.

Loads user plugins, initialises AppKit on mac`OS` thread 0 via
trivial-main-thread, and enters the AppKit event loop (never returns
normally).

<a id="x-28BARISTA-2FMAIN-3ASTART-PLUGINS-20FUNCTION-29"></a>

##### [function](c81e) `barista/main:start-plugins`

Instantiate and start all registered plugins regardless of config.
Must be called on the AppKit main thread.
`NOTE`: Prefer start-enabled-plugins for normal startup.

<a id="x-28BARISTA-2FMAIN-3ASTOP-PLUGINS-20FUNCTION-29"></a>

##### [function](70df) `barista/main:stop-plugins`

Stop all currently running plugins.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FMENU-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/MENU

<a id="x-28-23A-28-2812-29-20BASE-CHAR-20-2E-20-22BARISTA-2FMENU-22-29-20PACKAGE-29"></a>

#### [package](0a75) `barista/menu`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FMENU-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28BARISTA-2FMENU-3AHIDE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](6f71) `barista/menu:hide` item

Remove `ITEM` from the status bar and release the `NSS`tatusItem.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FMENU-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FMENU-3AADD-ITEM-20FUNCTION-29"></a>

##### [function](656f) `barista/menu:add-item` title &key callback submenu url state disabled

Add an item to the menu currently being built by build-menu.
Must be called inside a build-menu body.
`STATE`:    when T sets the native mac`OS` checkmark (`NSC`ontrolStateValueOn).
`DISABLED`: when T greys out the item and makes it non-interactive.

<a id="x-28BARISTA-2FMENU-3AADD-SEPARATOR-20FUNCTION-29"></a>

##### [function](e30e) `barista/menu:add-separator`

Add a native `NSM`enuItem separator to the menu currently being built.
Must be called inside a build-menu body.

<a id="x-28BARISTA-2FMENU-3AINITIALIZE-STATUS-ITEM-20FUNCTION-29"></a>

##### [function](9770) `barista/menu:initialize-status-item` item

Create the AppKit `NSS`tatusItem for the barista/classes:status-item `ITEM`.
Must be called on the AppKit main thread.

<a id="x-28BARISTA-2FMENU-3AINITIALIZE-STATUS-ITEM-WITH-IMAGE-20FUNCTION-29"></a>

##### [function](985f) `barista/menu:initialize-status-item-with-image` ITEM IMAGE-PATH &KEY (SIZE 18.0d0) TEMPLATE

Like initialize-status-item but uses an image file instead of a text title.
`IMAGE-PATH` is a `CL` pathname or namestring to a `PNG`/`ICNS` file.
`SIZE`     -- desired icon size in points (default 18); passed to make-ns-image.
`TEMPLATE` -- when T, marks the image as a template (monochrome/symbolic icons
            that should adapt to dark/light mode).  Leave `NIL` (default) for
            colour images so pixels are rendered as-is.

<a id="x-28BARISTA-2FMENU-3AMAKE-MENU-20FUNCTION-29"></a>

##### [function](5b37) `barista/menu:make-menu` name-or-menu

Return an `NSM`enu pointer for `NAME-OR-MENU`.
`NAME-OR-MENU` may be a symbol (looked up in *menu-constructors* or as a
function), or an already-built `NSM`enu `CFFI` pointer.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FMENU-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28BARISTA-2FMENU-3ABUILD-MENU-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](2db0) `barista/menu:build-menu` &body body

Evaluate `BODY` with *current-menu* bound to a fresh `NSM`enu, then return it.

<a id="x-28BARISTA-2FMENU-3ADEFMENU-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](987a) `barista/menu:defmenu` name (&rest items)

Define a named menu constructor and register it in *menu-constructors*.

Example:
  (defmenu my-menu
      (("Item one" :callback #'handler)
       ("Item two" :url "https://example.com")))

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FNOTIFY-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/NOTIFY

<a id="x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22BARISTA-2FNOTIFY-22-29-20PACKAGE-29"></a>

#### [package](c9b1) `barista/notify`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FNOTIFY-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FNOTIFY-3AGET-AVAILABLE-SOUNDS-20FUNCTION-29"></a>

##### [function](e5e5) `barista/notify:get-available-sounds`

<a id="x-28BARISTA-2FNOTIFY-3ANOTIFY-20FUNCTION-29"></a>

##### [function](b8a4) `barista/notify:notify` MESSAGE &KEY (TITLE "Barista") SOUND

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FPLUGIN-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/PLUGIN

<a id="x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22BARISTA-2FPLUGIN-22-29-20PACKAGE-29"></a>

#### [package](86d3) `barista/plugin`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FPLUGIN-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28BARISTA-2FCLASSES-3AGET-IMAGE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](1742) `barista/classes:get-image` plugin

Return the current status-bar icon of `PLUGIN`.

<a id="x-28BARISTA-2FPLUGIN-3AGET-MENU-20GENERIC-FUNCTION-29"></a>

##### [generic-function](509a) `barista/plugin:get-menu` plugin

Return the menu-thunk of `PLUGIN`'s status item.

<a id="x-28BARISTA-2FPLUGIN-3AGET-STATUS-ITEM-20GENERIC-FUNCTION-29"></a>

##### [generic-function](6b7a) `barista/plugin:get-status-item` plugin

Return the [`barista/classes:status-item`][2c55] wrapper of `PLUGIN`.

<a id="x-28BARISTA-2FPLUGIN-3AGET-TITLE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](d71d) `barista/plugin:get-title` plugin

Return the current status-bar label of `PLUGIN`.

<a id="x-28BARISTA-2FPLUGIN-3AINITIALIZE-PLUGIN-20GENERIC-FUNCTION-29"></a>

##### [generic-function](5136) `barista/plugin:initialize-plugin` plugin

<a id="x-28BARISTA-2FPLUGIN-3ASTOP-PLUGIN-20GENERIC-FUNCTION-29"></a>

##### [generic-function](0e3f) `barista/plugin:stop-plugin` plugin

Stop `PLUGIN`, dispatching to the AppKit main thread via `GCD`.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FPLUGIN-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FPLUGIN-3AGET-AVAILABLE-PLUGINS-20FUNCTION-29"></a>

##### [function](b768) `barista/plugin:get-available-plugins`

<a id="x-28BARISTA-2FPLUGIN-3AGET-PLUGIN-INSTANCE-20FUNCTION-29"></a>

##### [function](28d1) `barista/plugin:get-plugin-instance` class-name

<a id="x-28BARISTA-2FPLUGIN-3AIS-PLUGIN-RUNNING-20FUNCTION-29"></a>

##### [function](f2fc) `barista/plugin:is-plugin-running` class-name

<a id="x-28BARISTA-2FPLUGIN-3ARESTART-PLUGIN-20FUNCTION-29"></a>

##### [function](a02a) `barista/plugin:restart-plugin` class-name

<a id="x-28BARISTA-2FPLUGIN-3ARESTART-PLUGINS-20FUNCTION-29"></a>

##### [function](939b) `barista/plugin:restart-plugins`

<a id="x-28BARISTA-2FPLUGIN-3ARUNNING-PLUGINS-20FUNCTION-29"></a>

##### [function](6b37) `barista/plugin:running-plugins`

<a id="x-28BARISTA-2FPLUGIN-3ASTART-ENABLED-PLUGINS-20FUNCTION-29"></a>

##### [function](9aad) `barista/plugin:start-enabled-plugins`

Start only the plugins that are enabled in the configuration.
Must be called directly on the AppKit main thread (not via `GCD`) so that
all plugins are registered in *running-plugins* before the caller checks
visibility (e.g. ensure-system-plugin).

<a id="x-28BARISTA-2FPLUGIN-3ASTART-PLUGIN-20FUNCTION-29"></a>

##### [function](28ba) `barista/plugin:start-plugin` class-name

Start `CLASS-NAME`, dispatching to the AppKit main thread via `GCD`.
Safe to call from any thread.  Returns immediately.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FPLUGIN-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28BARISTA-2FPLUGIN-3ADEFPLUGIN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](bcd9) `barista/plugin:defplugin` name (&rest slots) &body options

Define a Barista plugin class with background workers and a menu-bar item.

Options:
  (:title `EXPR`)                       -- initial status-bar title (text or attributed)
  (:image `PATH` &key size template)    -- status-bar icon from an image file.
                                         `PATH` is a pathname or string evaluated
                                         at plugin start time.
                                         `SIZE` (default 18) scales the icon.
                                         `TEMPLATE` T for monochrome/symbolic icons
                                         that should adapt to dark/light mode;
                                         leave `NIL` (default) for colour images.
  (:menu `SYMBOL`)                      -- name of a defmenu to show on click
  (:every `PERIOD` `FORMS`)               -- background worker: evaluate `FORMS` every `PERIOD`

:title and :image are mutually exclusive; :image takes precedence when both
are supplied.

<a id="x-28BARISTA-2FPLUGIN-3AREPLACE-MENU-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](b267) `barista/plugin:replace-menu` from to

<a id="x-28BARISTA-2FPLUGIN-3AWITH-PLUGIN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](bf1a) `barista/plugin:with-plugin` name &body body

Run `BODY` with barista/vars:*plugin* bound to the named running plugin.
Useful for interactive debugging.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FSYSTEM-PLUGIN-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/SYSTEM-PLUGIN

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-22BARISTA-2FSYSTEM-PLUGIN-22-29-20PACKAGE-29"></a>

#### [package](749d) `barista/system-plugin`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FSYSTEM-PLUGIN-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FSYSTEM-PLUGIN-3AENSURE-SYSTEM-PLUGIN-20FUNCTION-29"></a>

##### [function](de6e) `barista/system-plugin:ensure-system-plugin`

Call after start-enabled-plugins to show the system plugin when needed.
Must be called on the AppKit main thread.

<a id="x-28BARISTA-2FSYSTEM-PLUGIN-3AHIDE-SYSTEM-PLUGIN-20FUNCTION-29"></a>

##### [function](b9b0) `barista/system-plugin:hide-system-plugin`

Hide the system plugin status item from the menu bar.
No-op if not visible.

<a id="x-28BARISTA-2FSYSTEM-PLUGIN-3AMAKE-PLUGINS-SUBMENU-20FUNCTION-29"></a>

##### [function](97fb) `barista/system-plugin:make-plugins-submenu`

Build the Settings > Plugins submenu dynamically.
Each item shows the plugin name with a native checkmark if currently enabled.

<a id="x-28BARISTA-2FSYSTEM-PLUGIN-3ASHOW-SYSTEM-PLUGIN-20FUNCTION-29"></a>

##### [function](9a77) `barista/system-plugin:show-system-plugin`

Show the system plugin status item in the menu bar.
Creates it on first call; no-op if already visible.

<a id="x-28BARISTA-2FSYSTEM-PLUGIN-3AUPDATE-SYSTEM-PLUGIN-VISIBILITY-20FUNCTION-29"></a>

##### [function](18c6) `barista/system-plugin:update-system-plugin-visibility`

Show the system plugin if no user plugins are running; hide it otherwise.
Must be called from the AppKit main thread (via on-main-thread when in doubt).

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/UTILS

<a id="x-28-23A-28-2813-29-20BASE-CHAR-20-2E-20-22BARISTA-2FUTILS-22-29-20PACKAGE-29"></a>

#### [package](5118) `barista/utils`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28BARISTA-2FUTILS-3AFORMAT-DURATION-20FUNCTION-29"></a>

##### [function](c7e4) `barista/utils:format-duration` duration &optional stream

<a id="x-28BARISTA-2FUTILS-3AOPEN-URL-20FUNCTION-29"></a>

##### [function](b6fc) `barista/utils:open-url` url

Open `URL` in the default browser via a background thread.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FUTILS-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28BARISTA-2FUTILS-3AON-MAIN-THREAD-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](4f7a) `barista/utils:on-main-thread` &rest actions

Schedule `ACTIONS` to run on the AppKit main thread via `GCD`.
Returns immediately (fire-and-forget).  Safe to call from any thread.

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-40BARISTA-2FVARS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### BARISTA/VARS

<a id="x-28-23A-28-2812-29-20BASE-CHAR-20-2E-20-22BARISTA-2FVARS-22-29-20PACKAGE-29"></a>

#### [package](3696) `barista/vars`

<a id="x-28BARISTA-DOCS-2FINDEX-3A-3A-7C-40BARISTA-2FVARS-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-28BARISTA-2FVARS-3A-2ADEBUG-2A-20-28VARIABLE-29-29"></a>

##### [variable](f868) `barista/vars:*debug*` nil

If True, then debugger will be invoked on any error in the periodic threads.

<a id="x-28BARISTA-2FVARS-3A-2APLUGIN-2A-20-28VARIABLE-29-29"></a>

##### [variable](6f1f) `barista/vars:*plugin*` nil

Current plugin.

<a id="x-28BARISTA-2FVARS-3A-2BDEFAULT-FONT-SIZE-2B-20-28VARIABLE-29-29"></a>

##### [variable](a542) `barista/vars:+default-font-size+` 14

Default font size in points for status-bar text.

<a id="x-28BARISTA-2FVARS-3A-2BSUPPORTED-COLORS-2B-20-28VARIABLE-29-29"></a>

##### [variable](6f3a) `barista/vars:+supported-colors+` (:aliceblue :antiquewhite :antiquewhite1 :antiquewhite2 :antiquewhite3
 :antiquewhite4 :aquamarine :aquamarine1 :aquamarine2 :aquamarine3 :aquamarine4
 :azure :azure1 :azure2 :azure3 :azure4 :beige :bisque :bisque1 :bisque2
 :bisque3 :bisque4 :black :blanchedalmond :blue :blue1 :blue2 :blue3 :blue4
 :blueviolet :brown :brown1 :brown2 :brown3 :brown4 :burlywood :burlywood1
 :burlywood2 :burlywood3 :burlywood4 :cadetblue :cadetblue1 :cadetblue2
 :cadetblue3 :cadetblue4 :chartreuse :chartreuse1 :chartreuse2 :chartreuse3
 :chartreuse4 :chocolate :chocolate1 :chocolate2 :chocolate3 :chocolate4 :coral
 :coral1 :coral2 :coral3 :coral4 :cornflowerblue :cornsilk :cornsilk1
 :cornsilk2 :cornsilk3 :cornsilk4 :cyan :cyan1 :cyan2 :cyan3 :cyan4 :darkblue
 :darkcyan :darkgoldenrod :darkgoldenrod1 :darkgoldenrod2 :darkgoldenrod3
 :darkgoldenrod4 :darkgray :darkgreen :darkgrey :darkkhaki :darkmagenta
 :darkolivegreen :darkolivegreen1 :darkolivegreen2 :darkolivegreen3
 :darkolivegreen4 :darkorange :darkorange1 :darkorange2 :darkorange3
 :darkorange4 :darkorchid :darkorchid1 :darkorchid2 :darkorchid3 :darkorchid4
 :darkred :darksalmon :darkseagreen :darkseagreen1 :darkseagreen2
 :darkseagreen3 :darkseagreen4 :darkslateblue :darkslategray :darkslategray1
 :darkslategray2 :darkslategray3 :darkslategray4 :darkslategrey :darkturquoise
 :darkviolet :debianred :deeppink :deeppink1 :deeppink2 :deeppink3 :deeppink4
 :deepskyblue :deepskyblue1 :deepskyblue2 :deepskyblue3 :deepskyblue4 :dimgray
 :dimgrey :dodgerblue :dodgerblue1 :dodgerblue2 :dodgerblue3 :dodgerblue4
 :firebrick :firebrick1 :firebrick2 :firebrick3 :firebrick4 :floralwhite
 :forestgreen :gainsboro :ghostwhite :gold :gold1 :gold2 :gold3 :gold4
 :goldenrod :goldenrod1 :goldenrod2 :goldenrod3 :goldenrod4 :gray :gray0 :gray1
 :gray10 :gray100 :gray11 :gray12 :gray13 :gray14 :gray15 :gray16 :gray17
 :gray18 :gray19 :gray2 :gray20 :gray21 :gray22 :gray23 :gray24 :gray25 :gray26
 :gray27 :gray28 :gray29 :gray3 :gray30 :gray31 :gray32 :gray33 :gray34 :gray35
 :gray36 :gray37 :gray38 :gray39 :gray4 :gray40 :gray41 :gray42 :gray43 :gray44
 :gray45 :gray46 :gray47 :gray48 :gray49 :gray5 :gray50 :gray51 :gray52 :gray53
 :gray54 :gray55 :gray56 :gray57 :gray58 :gray59 :gray6 :gray60 :gray61 :gray62
 :gray63 :gray64 :gray65 :gray66 :gray67 :gray68 :gray69 :gray7 :gray70 :gray71
 :gray72 :gray73 :gray74 :gray75 :gray76 :gray77 :gray78 :gray79 :gray8 :gray80
 :gray81 :gray82 :gray83 :gray84 :gray85 :gray86 :gray87 :gray88 :gray89 :gray9
 :gray90 :gray91 :gray92 :gray93 :gray94 :gray95 :gray96 :gray97 :gray98
 :gray99 :green :green1 :green2 :green3 :green4 :greenyellow :grey :grey0
 :grey1 :grey10 :grey100 :grey11 :grey12 :grey13 :grey14 :grey15 :grey16
 :grey17 :grey18 :grey19 :grey2 :grey20 :grey21 :grey22 :grey23 :grey24 :grey25
 :grey26 :grey27 :grey28 :grey29 :grey3 :grey30 :grey31 :grey32 :grey33 :grey34
 :grey35 :grey36 :grey37 :grey38 :grey39 :grey4 :grey40 :grey41 :grey42 :grey43
 :grey44 :grey45 :grey46 :grey47 :grey48 :grey49 :grey5 :grey50 :grey51 :grey52
 :grey53 :grey54 :grey55 :grey56 :grey57 :grey58 :grey59 :grey6 :grey60 :grey61
 :grey62 :grey63 :grey64 :grey65 :grey66 :grey67 :grey68 :grey69 :grey7 :grey70
 :grey71 :grey72 :grey73 :grey74 :grey75 :grey76 :grey77 :grey78 :grey79 :grey8
 :grey80 :grey81 :grey82 :grey83 :grey84 :grey85 :grey86 :grey87 :grey88
 :grey89 :grey9 :grey90 :grey91 :grey92 :grey93 :grey94 :grey95 :grey96 :grey97
 :grey98 :grey99 :honeydew :honeydew1 :honeydew2 :honeydew3 :honeydew4 :hotpink
 :hotpink1 :hotpink2 :hotpink3 :hotpink4 :indianred :indianred1 :indianred2
 :indianred3 :indianred4 :ivory :ivory1 :ivory2 :ivory3 :ivory4 :khaki :khaki1
 :khaki2 :khaki3 :khaki4 :lavender :lavenderblush :lavenderblush1
 :lavenderblush2 :lavenderblush3 :lavenderblush4 :lawngreen :lemonchiffon
 :lemonchiffon1 :lemonchiffon2 :lemonchiffon3 :lemonchiffon4 :lightblue
 :lightblue1 :lightblue2 :lightblue3 :lightblue4 :lightcoral :lightcyan
 :lightcyan1 :lightcyan2 :lightcyan3 :lightcyan4 :lightgoldenrod
 :lightgoldenrod1 :lightgoldenrod2 :lightgoldenrod3 :lightgoldenrod4
 :lightgoldenrodyellow :lightgray :lightgreen :lightgrey :lightpink :lightpink1
 :lightpink2 :lightpink3 :lightpink4 :lightsalmon :lightsalmon1 :lightsalmon2
 :lightsalmon3 :lightsalmon4 :lightseagreen :lightskyblue :lightskyblue1
 :lightskyblue2 :lightskyblue3 :lightskyblue4 :lightslateblue :lightslategray
 :lightslategrey :lightsteelblue :lightsteelblue1 :lightsteelblue2
 :lightsteelblue3 :lightsteelblue4 :lightyellow :lightyellow1 :lightyellow2
 :lightyellow3 :lightyellow4 :limegreen :linen :magenta :magenta1 :magenta2
 :magenta3 :magenta4 :maroon :maroon1 :maroon2 :maroon3 :maroon4
 :mediumaquamarine :mediumblue :mediumorchid :mediumorchid1 :mediumorchid2
 :mediumorchid3 :mediumorchid4 :mediumpurple :mediumpurple1 :mediumpurple2
 :mediumpurple3 :mediumpurple4 :mediumseagreen :mediumslateblue
 :mediumspringgreen :mediumturquoise :mediumvioletred :midnightblue :mintcream
 :mistyrose :mistyrose1 :mistyrose2 :mistyrose3 :mistyrose4 :moccasin
 :navajowhite :navajowhite1 :navajowhite2 :navajowhite3 :navajowhite4 :navy
 :navyblue :oldlace :olivedrab :olivedrab1 :olivedrab2 :olivedrab3 :olivedrab4
 :orange :orange1 :orange2 :orange3 :orange4 :orangered :orangered1 :orangered2
 :orangered3 :orangered4 :orchid :orchid1 :orchid2 :orchid3 :orchid4
 :palegoldenrod :palegreen :palegreen1 :palegreen2 :palegreen3 :palegreen4
 :paleturquoise :paleturquoise1 :paleturquoise2 :paleturquoise3 :paleturquoise4
 :palevioletred :palevioletred1 :palevioletred2 :palevioletred3 :palevioletred4
 :papayawhip :peachpuff :peachpuff1 :peachpuff2 :peachpuff3 :peachpuff4 :peru
 :pink :pink1 :pink2 :pink3 :pink4 :plum :plum1 :plum2 :plum3 :plum4
 :powderblue :purple :purple1 :purple2 :purple3 :purple4 :red :red1 :red2 :red3
 :red4 :rosybrown :rosybrown1 :rosybrown2 :rosybrown3 :rosybrown4 :royalblue
 :royalblue1 :royalblue2 :royalblue3 :royalblue4 :saddlebrown :salmon :salmon1
 :salmon2 :salmon3 :salmon4 :sandybrown :seagreen :seagreen1 :seagreen2
 :seagreen3 :seagreen4 :seashell :seashell1 :seashell2 :seashell3 :seashell4
 :sienna :sienna1 :sienna2 :sienna3 :sienna4 :skyblue :skyblue1 :skyblue2
 :skyblue3 :skyblue4 :slateblue :slateblue1 :slateblue2 :slateblue3 :slateblue4
 :slategray :slategray1 :slategray2 :slategray3 :slategray4 :slategrey :snow
 :snow1 :snow2 :snow3 :snow4 :springgreen :springgreen1 :springgreen2
 :springgreen3 :springgreen4 :steelblue :steelblue1 :steelblue2 :steelblue3
 :steelblue4 :tan :tan1 :tan2 :tan3 :tan4 :thistle :thistle1 :thistle2
 :thistle3 :thistle4 :tomato :tomato1 :tomato2 :tomato3 :tomato4 :turquoise
 :turquoise1 :turquoise2 :turquoise3 :turquoise4 :violet :violetred :violetred1
 :violetred2 :violetred3 :violetred4 :wheat :wheat1 :wheat2 :wheat3 :wheat4
 :white :whitesmoke :yellow :yellow1 :yellow2 :yellow3 :yellow4 :yellowgreen)

A list of supported color names.


[511d]: http://richsomerfield.com/apps/textbar/
[2c55]: https://40ants.com/barista/#x-28BARISTA-2FCLASSES-3ASTATUS-ITEM-20CLASS-29
[7a11]: https://en.wikipedia.org/wiki/Pomodoro_Technique
[d27f]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/bar-icon.lisp#L1
[9b5b]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/bar-icon.lisp#L91
[6c43]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L1
[6a5f]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L114
[4c75]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L221
[a36b]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L236
[6d3d]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L244
[0dc6]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L262
[2fdb]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L294
[f97a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L63
[a25e]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L64
[6e21]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L67
[1aaa]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L77
[b489]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L81
[d94d]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/classes.lisp#L89
[b766]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/config.lisp#L1
[d0c6]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/config.lisp#L28
[1ebe]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/config.lisp#L58
[306a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/config.lisp#L67
[6927]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/config.lisp#L74
[f2e4]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/main.lisp#L1
[a013]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/main.lisp#L36
[c81e]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/main.lisp#L43
[70df]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/main.lisp#L50
[5c31]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/main.lisp#L58
[0a75]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L1
[9770]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L175
[6f71]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L221
[987a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L320
[656f]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L346
[e30e]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L360
[2db0]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L369
[5b37]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L378
[985f]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/menu.lisp#L399
[c9b1]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/notify.lisp#L1
[e5e5]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/notify.lisp#L17
[b8a4]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/notify.lisp#L25
[86d3]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L1
[28d1]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L101
[f2fc]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L104
[0e3f]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L124
[5136]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L136
[b768]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L142
[28ba]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L154
[a02a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L160
[939b]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L163
[9aad]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L166
[bcd9]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L251
[b267]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L342
[bf1a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L350
[d71d]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L56
[1742]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L59
[509a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L62
[6b7a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L65
[6b37]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/plugin.lisp#L97
[749d]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/system-plugin.lisp#L1
[9a77]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/system-plugin.lisp#L111
[b9b0]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/system-plugin.lisp#L132
[18c6]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/system-plugin.lisp#L141
[de6e]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/system-plugin.lisp#L151
[97fb]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/system-plugin.lisp#L58
[5118]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/utils.lisp#L1
[c7e4]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/utils.lisp#L16
[b6fc]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/utils.lisp#L69
[4f7a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/utils.lisp#L78
[3696]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/vars.lisp#L1
[6f1f]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/vars.lisp#L13
[f868]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/vars.lisp#L17
[a542]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/vars.lisp#L20
[6f3a]: https://github.com/40ants/barista/blob/dd48589644bb35ba3b5217aa1dd209adb5fba026/src/vars.lisp#L23
[9e19]: https://github.com/40ants/barista/releases
[e3ea]: https://github.com/fukamachi/qlot
[4aa0]: https://github.com/matryer/xbar
[795a]: https://github.com/roswell/roswell
[422a]: https://quickdocs.org/40ants-logging
[8236]: https://quickdocs.org/alexandria
[3dbf]: https://quickdocs.org/bordeaux-threads
[0383]: https://quickdocs.org/cffi
[b139]: https://quickdocs.org/cl-colors
[6303]: https://quickdocs.org/cxml
[3266]: https://quickdocs.org/defmain
[8347]: https://quickdocs.org/dexador
[a3a3]: https://quickdocs.org/fmt
[46a1]: https://quickdocs.org/local-time
[6422]: https://quickdocs.org/local-time-duration
[7f8b]: https://quickdocs.org/log4cl
[691c]: https://quickdocs.org/log4cl-extras
[b440]: https://quickdocs.org/slynk
[3803]: https://quickdocs.org/swank
[efc1]: https://quickdocs.org/trivial-main-thread
[a899]: https://quickdocs.org/ubiquitous
[5274]: https://quickdocs.org/uiop
[ce80]: https://quickdocs.org/zpng
[3c6a]: https://www.cbr.ru/

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
