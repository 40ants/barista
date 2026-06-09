(defpackage #:barista/bar-icon
  (:use #:cl)
  (:export
   #:render-bars-icon))
(in-package #:barista/bar-icon)


;;; ---- layout constants ----------------------------------------------------

(defconstant +icon-h+  20 "Icon height in pixels.")
(defconstant +bar-w+    8 "Width of each bar in pixels.")
(defconstant +bar-gap+  3 "Gap between bars in pixels.")
(defconstant +icon-pad+ 2 "Horizontal padding on each side in pixels.")


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
  "Return true when pixel (PX PY) is inside rounded rect [X1 Y1]–[X2 Y2]
  with corner radius R (integer, pixel-exact)."
  (when (and (<= x1 px x2) (<= y1 py y2))
    (let ((left   (+ x1 r))
          (right  (- x2 r))
          (top    (+ y1 r))
          (bottom (- y2 r)))
      (or (<= left px right)
          (<= top  py bottom)
          (let ((cx (cond ((< px left)  left)
                          ((> px right) right)
                          (t px)))
                (cy (cond ((< py top)    top)
                          ((> py bottom) bottom)
                          (t py))))
            (<= (+ (expt (- px cx) 2) (expt (- py cy) 2))
                (expt r 2)))))))

(defun set-pixel! (data x y width r g b a)
  "Write RGBA bytes into DATA (flat octet vector) at pixel (X Y)."
  (let ((idx (* 4 (+ (* y width) x))))
    (setf (aref data idx)       r
          (aref data (+ idx 1)) g
          (aref data (+ idx 2)) b
          (aref data (+ idx 3)) a)))


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
        (cond
          ((inside-rounded-rect-p px py (1+ x) y-top (1- x2) (- y2 1) 2)
           (set-pixel! data px py img-width r g b 230))
          ((inside-rounded-rect-p px py x 0 x2 y2 3)
           (set-pixel! data px py img-width 100 100 100 120)))))))


;;; ---- public API ----------------------------------------------------------

(defun icon-width (n-bars)
  "Return the pixel width of an icon holding N-BARS bars."
  (+ (* 2 +icon-pad+)
     (* +bar-w+ n-bars)
     (* +bar-gap+ (max 0 (1- n-bars)))))

(defun render-bars-icon (ratios path)
  "Render a bar-chart icon for RATIOS (a list of floats in [0.0, 1.0]) to PATH.
  Each ratio gets one coloured vertical bar; colour is chosen by usage-color.
  Writes a truecolor-alpha PNG and returns PATH."
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
    path))
