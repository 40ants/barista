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
  (:import-from #:cl-ppcre
                #:scan-to-strings)
  (:import-from #:local-time
                #:now
                #:format-timestring))
(in-package #:barista-plugins/currency/currency)


;;; ---- CBR XML fetch & parse -----------------------------------------------

(defparameter +cbr-url+
  "https://www.cbr.ru/scripts/XML_daily.asp"
  "URL of the Central Bank of Russia daily currency rates XML feed.")

(defun fetch-cbr-xml ()
  "Fetch the CBR daily rates XML and return it as a string, or NIL on error."
  (handler-case
      (dex:get +cbr-url+)
    (error (e)
      (log:warn "Currency fetch failed: ~A" e)
      nil)))

(defun parse-rate (xml char-code)
  "Extract the exchange rate for CHAR-CODE (e.g. \"USD\") from CBR XML string.
  Returns a float, or NIL if not found.
  CBR XML structure (simplified):
    <Valute ...>
      <CharCode>USD</CharCode>
      <Nominal>1</Nominal>
      <Value>89,5000</Value>
    </Valute>"
  (when xml
    (handler-case
        (multiple-value-bind (match regs)
            (scan-to-strings
             (concatenate 'string
                          "<CharCode>" char-code "</CharCode>"
                          "\\s*<Nominal>(\\d+)</Nominal>"
                          "\\s*<Value>([\\d,]+)</Value>")
             xml)
          (when match
            (let* ((nominal (parse-integer (aref regs 0)))
                   (value-str (substitute #\. #\, (aref regs 1)))
                   (value (read-from-string value-str)))
              (/ (float value) nominal))))
      (error (e)
        (log:warn "Currency parse error for ~A: ~A" char-code e)
        nil))))

(defun fetch-rates ()
  "Fetch USD and EUR rates from CBR. Updates plugin slots on success."
  (let ((xml (fetch-cbr-xml)))
    (when xml
      (let ((usd (parse-rate xml "USD"))
            (eur (parse-rate xml "EUR")))
        (when usd (setf (get-usd-rate *plugin*) usd))
        (when eur (setf (get-eur-rate *plugin*) eur))
        (when (or usd eur)
          (setf (get-updated-at *plugin*) (now)))))))


;;; ---- display helpers -----------------------------------------------------

(defun format-rate (rate symbol)
  "Format RATE as e.g. \"$89.50\", or \"…\" while loading."
  (if rate
      (format nil "~A~,2F" symbol rate)
      (concatenate 'string symbol "...")))

(defun update-title ()
  "Flip between USD and EUR display in the status bar."
  (let ((show-usd (get-show-usd *plugin*))
        (usd      (get-usd-rate *plugin*))
        (eur      (get-eur-rate *plugin*)))
    (setf (get-title *plugin*)
          (if show-usd
              (format-rate usd "$")
              (format-rate eur "€")))
    (setf (get-show-usd *plugin*) (not show-usd))))


;;; ---- menu builder --------------------------------------------------------

(defun format-updated-at (ts)
  "Return a human-readable last-updated string for timestamp TS."
  (if ts
      (format-timestring nil ts :format '("Updated " :hour ":" :min))
      "Not yet updated"))

(defun build-currency-menu (plugin)
  "Build the currency NSMenu from PLUGIN's current rates."
  (build-menu
    (add-item (format nil "USD  ~A ₽" (format-rate (get-usd-rate plugin) "")))
    (add-item (format nil "EUR  ~A ₽" (format-rate (get-eur-rate plugin) "")))
    (add-item (format-updated-at (get-updated-at plugin)))))


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
