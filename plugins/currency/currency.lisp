(defpackage #:barista-plugins/currency/currency
  (:use #:cl)
  (:import-from #:barista/menu
                #:build-menu
                #:add-item)
  (:import-from #:barista/plugin
                #:defplugin
                #:get-title)
  (:import-from #:barista/vars
                #:*plugin*)
  (:import-from #:local-time
                #:now
                #:format-timestring))
(in-package #:barista-plugins/currency/currency)


;;; ---- CBR XML fetch & parse -----------------------------------------------
;;;
;;; The CBR feed is declared as windows-1251.  dexador is asked for raw octets
;;; (:force-binary t) so that CXML can honour the encoding declaration and
;;; produce correct Unicode strings from the start.

(defparameter +cbr-url+
  "https://www.cbr.ru/scripts/XML_daily.asp"
  "URL of the Central Bank of Russia daily currency rates XML feed.")

(defun fetch-cbr-octets ()
  "Fetch the CBR XML feed and return it as an octet vector, or NIL on error."
  (handler-case
      (dex:get +cbr-url+ :force-binary t)
    (error (e)
      (log:warn "Currency fetch failed: ~A" e)
      nil)))

(defun dom-text (node)
  "Return the concatenated text content of DOM NODE's child text nodes."
  (with-output-to-string (s)
    (dom:do-node-list (child (dom:child-nodes node))
      (when (dom:text-node-p child)
        (write-string (dom:node-value child) s)))))

(defun parse-valute (document char-code)
  "Find the Valute element with CharCode CHAR-CODE in DOCUMENT and return
  its rate as a float (Value / Nominal), or NIL if not found."
  (dom:do-node-list (valute (dom:get-elements-by-tag-name document "Valute"))
      (let ((codes    (dom:get-elements-by-tag-name valute "CharCode"))
            (nominals (dom:get-elements-by-tag-name valute "Nominal"))
            (values   (dom:get-elements-by-tag-name valute "Value")))
      (when (and (plusp (dom:length codes))
                 (string= (dom-text (dom:item codes 0)) char-code))
        (let ((nominal (parse-integer (dom-text (dom:item nominals 0))))
              (value   (read-from-string
                        (substitute #\. #\,
                                    (dom-text (dom:item values 0))))))
          (return-from parse-valute (/ (float value) nominal))))))
  nil)

(defun fetch-rates ()
  "Fetch USD and EUR rates from CBR and update the plugin slots."
  (let ((octets (fetch-cbr-octets)))
    (when octets
      (handler-case
          (let* ((document (cxml:parse octets (cxml-dom:make-dom-builder)))
                 (usd      (parse-valute document "USD"))
                 (eur      (parse-valute document "EUR")))
            (when usd (setf (get-usd-rate *plugin*) usd))
            (when eur (setf (get-eur-rate *plugin*) eur))
            (when (or usd eur)
              (setf (get-updated-at *plugin*) (now))))
        (error (e)
          (log:warn "Currency XML parse failed: ~A" e))))))


;;; ---- display helpers -----------------------------------------------------

(defun format-rate (rate symbol)
  "Format RATE as e.g. \"$89.50\", or \"symbol...\" while loading."
  (if rate
      (format nil "~A~,2F" symbol rate)
      (concatenate 'string symbol "...")))

(defun update-title ()
  "Flip between USD and EUR display in the status bar."
  (setf (get-title *plugin*)
        (if (get-show-usd *plugin*)
            (format-rate (get-usd-rate *plugin*) "$")
            (format-rate (get-eur-rate *plugin*) "€")))
  (setf (get-show-usd *plugin*) (not (get-show-usd *plugin*))))


;;; ---- menu builder --------------------------------------------------------

(defun build-currency-menu (plugin)
  "Build the currency NSMenu from PLUGIN's current rates."
  (build-menu
    (add-item (format nil "USD  ~A ₽" (format-rate (get-usd-rate plugin) ""))
              :disabled t)
    (add-item (format nil "EUR  ~A ₽" (format-rate (get-eur-rate plugin) ""))
              :disabled t)))


;;; ---- plugin ---------------------------------------------------------------

(defplugin currency
    ((usd-rate   :initform nil :accessor get-usd-rate)
     (eur-rate   :initform nil :accessor get-eur-rate)
     (updated-at :initform nil :accessor get-updated-at)
     (show-usd   :initform t   :accessor get-show-usd)
     (tick-count :initform 0   :accessor get-tick-count)
     (menu-ready :initform nil :accessor get-menu-ready))
  (:title "$...")
  ;; Flip display every 5 seconds; fetch rates every 360 ticks (30 minutes).
  (:every (5 :seconds)
          ;; Wire up dynamic menu on first tick.
          (unless (get-menu-ready *plugin*)
            (setf (barista/classes:get-menu-thunk
                   (barista/plugin:get-status-item *plugin*))
                  (let ((p *plugin*))
                    (lambda () (build-currency-menu p))))
            (setf (get-menu-ready *plugin*) t))
          (let ((tick (incf (get-tick-count *plugin*))))
            ;; Fetch on first tick and every 360 ticks thereafter.
            (when (or (= tick 1)
                      (zerop (mod tick 360)))
              (fetch-rates))
            (update-title))))
