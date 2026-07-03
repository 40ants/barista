(uiop:define-package #:barista/bar-icon
  (:use #:cl)
  (:import-from #:barista/classes
                #:make-image-file)
  (:export
   #:render-bars-icon
   #:+scale+))
(in-package #:barista/bar-icon)


;;; ---- layout constants ----------------------------------------------------

(defconstant +scale+        2   "Retina scale factor (2x for HiDPI).")
(defconstant +icon-h+      40   "Icon height in device pixels (20pt x 2).")
(defconstant +bar-w+       16   "Width of each bar in device pixels (8pt x 2).")
(defconstant +bar-gap+      6   "Gap between bars in device pixels (3pt x 2).")
(defconstant +icon-pad+     4   "Horizontal padding in device pixels (2pt x 2).")

(defconstant +bg-radius+    6   "Background corner radius in device pixels (3pt x 2).")
(defconstant +fill-radius+  4   "Fill corner radius in device pixels (2pt x 2).")


;;; ---- colour thresholds ---------------------------------------------------

(defparameter +color-green+ '(76  175  80) "Green  — usage below 50%.")
(defparameter +color-amber+ '(255 193   7) "Amber  — usage 50–80%.")
(defparameter +color-red+   '(244  67  54) "Red    — usage above 80%.")


;;; ---- colour selection ----------------------------------------------------

(defun usage-color (ratio)
  "Return the RGB colour list appropriate for RATIO in [0.0, 1.0]."
  (cond ((< ratio 0.5) +color-green+)
        ((< ratio 0.8) +color-amber+)
        (t             +color-red+)))


;;; ---- pixel helpers -------------------------------------------------------

(defun inside-rounded-rect-p (px py x1 y1 x2 y2 r)
  "Return the coverage alpha [0.0 1.0] for pixel (PX PY) against rounded rect
[X1 Y1]–[X2 Y2] with corner radius R.  Uses sub-pixel sampling (2x2 grid)
for anti-aliased edges."
  (flet ((point-alpha (sx sy)
           (let ((fx (+ px sx))
                 (fy (+ py sy)))
             (if (and (<= x1 fx x2) (<= y1 fy y2))
                 (let* ((left   (+ x1 r))
                        (right  (- x2 r))
                        (top    (+ y1 r))
                        (bottom (- y2 r))
                        (cx (cond ((< fx left)  left)
                                  ((> fx right) right)
                                  (t fx)))
                        (cy (cond ((< fy top)    top)
                                  ((> fy bottom) bottom)
                                  (t fy)))
                        (d2 (+ (expt (- fx cx) 2) (expt (- fy cy) 2))))
                   (if (<= d2 (expt r 2)) 1.0 0.0))
                 0.0))))
    (/ (+ (point-alpha 0.25 0.25)
          (point-alpha 0.75 0.25)
          (point-alpha 0.25 0.75)
          (point-alpha 0.75 0.75))
       4.0)))

(defun set-pixel! (data x y width r g b a)
  "Write RGBA bytes into DATA (flat octet vector) at pixel (X Y).
A may be a float in [0.0 1.0] or an integer in [0 255]."
  (let ((a-byte (etypecase a
                  (integer a)
                  (float (round (* a 255))))))
    (let ((idx (* 4 (+ (* y width) x))))
      (setf (aref data idx)       r
            (aref data (+ idx 1)) g
            (aref data (+ idx 2)) b
            (aref data (+ idx 3)) a-byte))))


;;; ---- bar drawing ---------------------------------------------------------

(defun draw-bar! (data img-width bar-index ratio r g b)
  "Draw one bar into DATA at BAR-INDEX.
Renders a rounded grey background at full height and a coloured fill
rising from the bottom proportional to RATIO in [0.0, 1.0]."
  (let* ((ratio  (max 0.0 (min 1.0 (float ratio))))
         (x      (+ +icon-pad+ (* bar-index (+ +bar-w+ +bar-gap+))))
         (x2     (+ x +bar-w+))
         (y2     (- +icon-h+ 1))
         (fill-h (max 1 (floor (* (- +icon-h+ 2) ratio))))
         (y-top  (- y2 fill-h)))
    (loop for py from 0 below +icon-h+ do
      (loop for px from x to x2 do
        (let ((fill-a (inside-rounded-rect-p px py (1+ x) y-top (1- x2) (- y2 1) +fill-radius+))
              (bg-a   (inside-rounded-rect-p px py x 0 x2 y2 +bg-radius+)))
          (cond
            ((> fill-a 0.0)
             (set-pixel! data px py img-width r g b (round (* fill-a 230))))
            ((> bg-a 0.0)
             (set-pixel! data px py img-width 100 100 100 (round (* bg-a 120))))))))))


;;; ---- public API ----------------------------------------------------------

(defun icon-width (n-bars)
  "Return the device-pixel width of an icon holding N-BARS bars."
  (+ (* 2 +icon-pad+)
     (* +bar-w+ n-bars)
     (* +bar-gap+ (max 0 (1- n-bars)))))

(defun render-bars-icon (ratios path)
  "Render a bar-chart icon for RATIOS (a list of floats in [0.0, 1.0]) to PATH.
Each ratio gets one coloured vertical bar; colour is chosen by usage-color.
Writes a truecolor-alpha PNG at +scale+x resolution and returns an IMAGE-FILE."
  (let* ((n    (length ratios))
         (w    (icon-width n))
         (png  (make-instance 'zpng:png
                              :color-type :truecolor-alpha
                              :width w
                              :height +icon-h+))
         (data (zpng:image-data png)))
    (fill data 0)
    (loop for ratio in ratios
          for i from 0
          do (destructuring-bind (r g b) (usage-color ratio)
               (draw-bar! data w i ratio r g b)))
    (zpng:write-png png path)
    (make-image-file path w +icon-h+)))
