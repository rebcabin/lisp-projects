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

(defmethod flip-about-diagonal-pinning-upper-left ((b box))
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
  (make-instance 'point :x (box-left  b) :y (box-top    b)))

(defmethod box-top-right ((b box))
  (make-instance 'point :x (box-right b) :y (box-top    b)))

(defmethod box-bottom-left ((b box))
  (make-instance 'point :x (box-left  b) :y (box-bottom b)))

(defmethod box-bottom-right ((b box))
  (make-instance 'point :x (box-right b) :y (box-bottom b)))

(defmethod displace ((p point) (b box) direction)
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
