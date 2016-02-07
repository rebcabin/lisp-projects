;;; R E N D E R I N G ==========================================================

(defmethod write-clip-char ((bb box)
                            (c character) (x integer) (y integer)
                            (gw function))
  (when (and (>= x (box-left   bb))
             (<  x (box-right  bb))
             (>= y (box-top    bb))
             (<  y (box-bottom bb)))
    (funcall gw x y c)))

(defmethod incr ((c character))
  (code-char (1+ (char-code c))))

(defmethod draw-line% ((bb box) (tl point) (br point)
                       (glypher function) (glyph-writer function))
  "Bresenham's cribbed from http://goo.gl/9ptT1g."
  (declare (ignorable bb))
  (let* (; (c  #\0) ;debugging
         (x1 (point-y tl))
         (x2 (point-y br))
         (y1 (point-x tl))
         (y2 (point-x br))
         (dist-x (abs (- x1 x2)))
         (dist-y (abs (- y1 y2)))
         (steep  (> dist-y dist-x)))
    (when steep
      (psetf x1 y1 y1 x1
             x2 y2 y2 x2))
    (when (> x1 x2)
      (psetf x1 x2 x2 x1
             y1 y2 y2 y1))
    (let* ((delta-x (- x2 x1))
           (delta-y (abs (- y1 y2)))
           (erroire (floor delta-x 2))
           (y-step  (if (< y1 y2) 1 -1))
           (y       y1))
      (loop
        :for x :upfrom x1 :to x2
        :do (if steep
                (write-clip-char bb (funcall glypher x y 'straight) x y glyph-writer)
                (write-clip-char bb (funcall glypher x y 'swapped ) y x glyph-writer))
            (setf erroire (- erroire delta-y))
            ;(setf c (incr c))
            (when (< erroire 0)
              (incf y y-step)
              (incf erroire delta-x)))) ))

(defun draw-line (&key bounding-box from-point to-point
                    glyph-fn glyph-writer-fn)
  (draw-line% bounding-box from-point to-point
              glyph-fn glyph-writer-fn))

(defmethod draw ((b box) (wb box) (c character) (gw function))
  (let ((tl (box-top-left b))
        (tr (decr-x (box-top-right b)))
        (bl (decr-y (box-bottom-left b)))
        (br (decr-x (decr-y (box-bottom-right b))))
        (gl (lambda (x y dir) (declare (ignorable x y dir)) c)))
    (draw-line% *window-box* tl tr gl gw)
    (draw-line% *window-box* tl bl gl gw)
    (draw-line% *window-box* br tr gl gw)
    (draw-line% *window-box* br bl gl gw)
    ))

