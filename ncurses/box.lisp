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
  ((left   :accessor box-left   :initform 0 :initarg :left  )
   (top    :accessor box-top    :initform 0 :initarg :top   )
   (width  :accessor box-width  :initform 0 :initarg :width )
   (height :accessor box-height :initform 0 :initarg :height)))

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

(defmethod boxes-equal ((b1 box) (b2 box))
  (and (= (box-left   b1) (box-left   b2))
       (= (box-top    b1) (box-top    b2))
       (= (box-width  b1) (box-width  b2))
       (= (box-height b1) (box-height b2))))

(defmethod point-in-box-p ((p point) (b box) &key (boundary 0))
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

(defmethod box-in-box-p ((bi box) (bo box) &key (boundary 0))
  (let ((lb (+ (box-left   bo) boundary))
        (rb (- (box-right  bo) boundary))
        (tb (+ (box-top    bo) boundary))
        (bb (- (box-bottom bo) boundary))
        (xl (box-left   bi))
        (xr (box-right  bi))
        (yt (box-top    bi))
        (yb (box-bottom bi)))
    (and (>= xl lb)
         (<= xr rb)
         (>= yt tb)
         (<= yb bb))))

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

;;; An "m-block" is a box of unit width and height, a.k.a. "unit box" (I can't
;;; use "block" because it's a reserved symbol, a "special operator.").

(defclass m-block (box) ())
(defmethod initialize-instance
    :around
    ((mb m-block)
     &key (left 0) (top 0))
  (call-next-method mb :left left :top top
                       :width 1 :height 1))

