;;; Boxes of non-positive width: Imagine that l >= r (left is greater than or
;;; equal to r). A point can only be inside such a box if the boxe's width is
;;; exactly zero because "inside" is defined by the condition that x >= l and x
;;; <= r. Boxes of negative width are "safe" in the sense that nothing is inside
;;; them. It's an open question whether such boxes have any sensible
;;; interpretation, but we'll include them in testing until we're sure that we
;;; want to get rid of them. Ditto non-positive height.

;;; A "positive" box is one with positive width and positive height. A "zero"
;;; box is one where at least one of its width or height is zero. A "negative"
;;; box has a negative width, negative height, or both.

;;; We'll have a world-box that contains a window box that contains room-boxes.

(defclass box ()
  ((left
    :accessor box-left
    :initform 0
    :initarg  :left)
   (top
    :accessor box-top
    :initform 0
    :initarg  :top)
   (width
    :accessor box-width
    :initform 0
    :initarg  :width)
   (height
    :accessor box-height
    :initform 0
    :initarg  :height)))

(defmethod non-negative-p ((b box))
  (and (<= 0 (box-width b))
       (<= 0 (box-height b))))

;;; Boxes are not very different from matrices. Maybe that's sufficient reason
;;; to disallow non-positive widths and heights.

(defmethod transpose ((b box))
  (make-instance 'box
                 :left   (box-left   b)
                 :top    (box-top    b)
                 :width  (box-height b)
                 :height (box-width  b)))

(defmethod box-bottom ((b box))
  (+ (box-top b) (box-height b)))

(defmethod box-right ((b box))
  (+ (box-left b) (box-width b)))

(defmethod box-top-left ((b box))
  (make-point :x (box-left  b) :y (box-top    b)))

(defmethod box-top-right ((b box))
  (make-point :x (box-right b) :y (box-top    b)))

(defmethod box-bottom-left ((b box))
  (make-point :x (box-left  b) :y (box-bottom b)))

(defmethod box-bottom-right ((b box))
  (make-point :x (box-right b) :y (box-bottom b)))

(defmethod point-in-box ((p point) (b box) &key (boundary 0))
  (let ((lb (+ (box-left   b) boundary))
        (rb (- (box-right  b) boundary))
        (tb (+ (box-top    b) boundary))
        (bb (- (box-bottom b) boundary))
        (x  (point-x p))
        (y  (point-y p)))
    (and (>= x lb)
         (<= x rb)
         (>= y tb)
         (<= y bb))))

(defmethod box-confined-p ((bi box) (bo box)))

(defmethod displace-confined ((p point) (b box) direction)
  "Move a point within a box, excluding the inner boundary, but don't let the
point escape the box."
  (case direction
    ((:left)  (let ((lb (box-left b))
                    (px (point-x  p)))
                (when (> (- px 1) lb) (displace-left p))))
    ((:up)    (let ((tb (box-top b))
                    (py (point-y  p)))
                (when (> (- py 1) tb) (displace-up p))))
    ((:right) (let ((rb (box-right b))
                    (px (point-x   p)))
                (when (< px (- rb 2)) (displace-right p))))
    ((:down)  (let ((bb (box-bottom b))
                   (py (point-y   p)))
               (when (< py (- bb 2)) (displace-down p))))
    ))

;;; A "block" is a box of unit width and height, a.k.a. "unit box."

